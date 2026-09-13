# ---------------------------------------------------------------------------
# Reading a guideline comparison
#
# Whether a result exceeded its guideline - and in particular whether a
# non-detect whose limit of reporting sits above the guideline counts - is
# settled once by join_action_levels(), through its `lor_as_exceedance`
# argument. Every function that later reports an exceedance reads that verdict
# rather than re-applying the rule, which is what keeps analyte_summary()'s
# counts, historical_range()'s flags and exceedance_summary()'s rows from
# disagreeing about the same round.
#
# The fallbacks below only apply to a `criteria` column added by hand, which
# carries no verdict. They are the detects-only rule, which is the
# conservative reading.
# ---------------------------------------------------------------------------

#' Which results were detected
#'
#' @param data chemistry tibble
#' @returns a logical vector, `FALSE` where the flag is missing
#' @noRd
is_detect <- function(data) {
  if (!"detect_flag" %in% names(data)) {
    return(rep(TRUE, nrow(data)))
  }
  !is.na(data$detect_flag) & data$detect_flag == "Y"
}


#' Which results sit above their guideline, detected or not
#'
#' A result with no guideline, or no readable concentration, is not above
#' anything - `FALSE` rather than `NA`, so the callers do not each have to
#' guard the comparison they wrap this in.
#'
#' @param data chemistry tibble
#' @param value_name name of the guideline value column
#' @returns a logical vector
#' @noRd
above_criteria <- function(data, value_name) {
  crit <- suppressWarnings(as.numeric(data[[value_name]]))
  conc <- suppressWarnings(as.numeric(data$concentration))
  !is.na(crit) & !is.na(conc) & conc > crit
}


#' Whether each result exceeded its guideline
#'
#' [join_action_levels()]'s verdict where the table carries one, and the
#' detects-only fallback where it does not.
#'
#' @param data chemistry tibble
#' @param value_name name of the guideline value column
#' @param cmp the comparison column names, from [comparison_columns()]
#' @returns a logical vector, one per row of `data`
#' @noRd
exceedance_verdict <- function(
  data,
  value_name,
  cmp = comparison_columns(value_name)
) {
  if (cmp[["exceedance"]] %in% names(data)) {
    return(as.logical(data[[cmp[["exceedance"]]]]))
  }
  above_criteria(data, value_name) & is_detect(data)
}


#' Whether each result is a non-detect reported above its guideline
#'
#' A limit of reporting too high to demonstrate compliance is a finding in its
#' own right, and is told apart from an exceedance rather than pooled with it.
#'
#' @param data chemistry tibble
#' @param value_name name of the guideline value column
#' @param cmp the comparison column names, from [comparison_columns()]
#' @returns a logical vector, one per row of `data`
#' @noRd
lor_verdict <- function(
  data,
  value_name,
  cmp = comparison_columns(value_name)
) {
  if (cmp[["lor"]] %in% names(data)) {
    return(as.logical(data[[cmp[["lor"]]]]))
  }
  above_criteria(data, value_name) & !is_detect(data)
}


#' Name the comparison columns for a guideline set
#'
#' The default set keeps the bare names the rest of the package reads. A
#' second set joined under its own `value_col` takes that name as a prefix, so
#' joining it does not overwrite the first set's verdict - which is what makes
#' several sets stackable by [criteria_long()].
#'
#' @param value_name name of the guideline value column
#' @returns named character vector: ratio, exceedance, lor
#' @noRd
comparison_columns <- function(value_name) {
  stem <- if (identical(value_name, "criteria")) {
    ""
  } else {
    paste0(value_name, "_")
  }
  c(
    ratio = paste0(stem, "exceedance_ratio"),
    exceedance = paste0(stem, "exceedance"),
    lor = paste0(stem, "lor_above_criteria")
  )
}


#' Name every column belonging to one guideline set
#'
#' @param value_name name of the guideline value column
#' @returns character vector of column names
#' @noRd
set_columns <- function(value_name) {
  c(
    value_name,
    paste0(value_name, c("_name", "_unit", "_basis")),
    unname(comparison_columns(value_name))
  )
}


#' Find the guideline sets joined onto a chemistry table
#'
#' A set is a value column carrying the `_name`, `_unit` and `_basis` columns
#' [join_action_levels()] writes beside it. Matching on all three keeps a
#' chemistry column that merely happens to be called `criteria` from being
#' read as a set.
#'
#' @param data chemistry tibble
#' @returns character vector of value column names
#' @noRd
criteria_sets <- function(data) {
  nms <- names(data)
  suffixed <- grep("_name$", nms, value = TRUE)
  stems <- sub("_name$", "", suffixed)
  stems <- stems[nzchar(stems) & stems %in% nms]
  stems[
    paste0(stems, "_unit") %in% nms &
      paste0(stems, "_basis") %in% nms
  ]
}


#' Reduce a group's criteria values to one
#'
#' [join_action_levels()] converts each guideline into the unit its result was
#' reported in, so one chemical measured in two units carries two criteria
#' values. The lowest is kept, since that is the one the exceedance count is
#' most conservative against.
#'
#' @param x criteria values for one location/chemical group
#' @param chem_name the group's chemical, used only in the warning
#' @returns a single value
#' @noRd
single_criteria <- function(x, chem_name = NULL) {
  vals <- unique(x[!is.na(x)])
  if (length(vals) == 0) {
    return(NA_real_)
  }
  if (length(vals) > 1) {
    warning(
      "Multiple criteria values for ",
      if (is.null(chem_name)) "a group" else chem_name[[1]],
      ": ",
      paste(vals, collapse = ", "),
      ". Using ",
      min(vals),
      ". Check the results are all reported in one unit."
    )
  }
  min(vals)
}


#' Put the guideline columns back under the name the set was joined as
#'
#' A set joined under its own `value_col` keeps that name here too, so
#' summaries of two sets can be bound together without either losing its
#' identity - the same rule [summary_stats()] follows. Shared with
#' [historical_range()], so it covers both tables' guideline columns and
#' renames only the ones present.
#'
#' @param out the summary table
#' @param value_name name of the guideline value column
#' @returns `out`, renamed
#' @noRd
rename_criteria_columns <- function(out, value_name) {
  if (identical(value_name, "criteria")) {
    return(out)
  }
  renamed <- c(
    criteria = value_name,
    n_exceedances = paste0(value_name, "_n_exceedances"),
    n_exceedance_locations = paste0(value_name, "_n_exceedance_locations"),
    exceedance_locations = paste0(value_name, "_exceedance_locations"),
    current_exceedance = paste0(value_name, "_current_exceedance")
  )
  hit <- match(names(renamed), names(out))
  names(out)[hit[!is.na(hit)]] <- renamed[!is.na(hit)]
  out
}

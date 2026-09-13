# ---------------------------------------------------------------------------
# Shared preamble for the reporting functions
#
# analyte_summary(), historical_range(), exceedance_summary() and
# min_max_locations() all open by asking `data` the same questions: does it
# carry the columns we cannot work without, which results belong to the round
# being reported on, is a guideline joined onto it, and what are we grouping
# by? Each used to answer them for itself, which meant a change to the
# grouping rule - or to the wording of an error - had to land in four places
# with nothing to keep them in step. They are answered once here.
# ---------------------------------------------------------------------------

#' Stop unless `data` carries the columns a function cannot work without
#'
#' Raised from the caller's frame, so the error names the function the user
#' actually called rather than this one.
#'
#' @param data the table to check
#' @param required character vector of column names
#' @param arg name of the argument being checked, for the message
#' @param hint what to do about it, appended to the message
#' @param call frame the error is attributed to
#' @returns `data`, invisibly
#' @noRd
require_columns <- function(
  data,
  required,
  arg = "data",
  hint = "Pass a table from data_processor().",
  call = rlang::caller_env()
) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    rlang::abort(
      paste0(
        "`",
        arg,
        "` is missing required columns: ",
        paste(missing, collapse = ", "),
        if (nzchar(hint)) paste0(". ", hint) else ""
      ),
      call = call
    )
  }
  invisible(data)
}


#' Read a column name out of a quoted argument
#'
#' The reporting functions take their column arguments with or without
#' quotes. A string has to be unwrapped rather than labelled: `as_label()`
#' renders `"event"` as `"\"event\""`, quotes included, which then matches no
#' column. `NULL` stays `NULL`, which is what tells [resolve_round()] to find
#' a round column for itself.
#'
#' @param quo a quosure, from `rlang::enquo()`
#' @returns a single string, or `NULL`
#' @noRd
quo_column_name <- function(quo) {
  if (rlang::quo_is_null(quo)) {
    return(NULL)
  }
  expr <- rlang::quo_get_expr(quo)
  if (is.character(expr)) {
    return(expr[[1]])
  }
  rlang::as_label(quo)
}


#' Check the extra columns a caller asked to group by
#'
#' @param data the table being summarised
#' @param group_vars what the caller passed, or NULL
#' @param call frame the error is attributed to
#' @returns a character vector, possibly empty
#' @noRd
resolve_group_vars <- function(data, group_vars, call = rlang::caller_env()) {
  group_vars <- if (is.null(group_vars)) {
    character(0)
  } else {
    as.character(group_vars)
  }
  unknown <- setdiff(group_vars, names(data))
  if (length(unknown) > 0) {
    rlang::abort(
      paste0(
        "`group_vars` names columns `data` does not have: ",
        paste(unknown, collapse = ", "),
        "."
      ),
      call = call
    )
  }
  group_vars
}


#' The columns a per-analyte summary groups by
#'
#' Analytes are always grouped by `chem_name`, and by `output_unit` and
#' `criteria_set` where the table carries them, so results reported in two
#' units - or compared against two guideline sets - are never pooled.
#' `criteria_set` is written by [criteria_long()]; grouping by it is what
#' turns several joined guideline sets into one row per analyte per set, with
#' no bookkeeping beyond having stacked them.
#'
#' @param data the table being summarised
#' @param group_vars further columns the caller asked for
#' @returns a character vector of grouping columns
#' @noRd
summary_groups <- function(data, group_vars) {
  unique(c(
    group_vars,
    "chem_name",
    intersect(c("output_unit", "criteria_set"), names(data))
  ))
}


#' Decide whether the guideline columns are being reported
#'
#' `NULL` means "include them where a guideline is present". `TRUE` asks for
#' them, and is an error where none was joined. `criteria_required` is for
#' [exceedance_summary()], which has nothing to say at all without one.
#'
#' @param data the table being summarised
#' @param value_name name of the guideline value column, or NULL where the
#'   function reports no guideline
#' @param include_criteria what the caller passed
#' @param criteria_required a guideline is not optional for this function
#' @param call frame the error is attributed to
#' @returns a single logical
#' @noRd
resolve_include_criteria <- function(
  data,
  value_name,
  include_criteria = NULL,
  criteria_required = FALSE,
  call = rlang::caller_env()
) {
  if (is.null(value_name)) {
    return(FALSE)
  }
  present <- value_name %in% names(data)

  # exceedance_summary(): without a guideline there is no question to answer,
  # so the reason it cannot proceed is worth saying rather than the bare
  # "column not found" the optional case gives.
  if (criteria_required) {
    if (!present) {
      rlang::abort(
        paste0(
          "`data` has no '",
          value_name,
          "' column, so nothing can be said to have exceeded anything. Join ",
          "a guideline set onto it with join_action_levels(), or name the ",
          "column it was joined into with `criteria_col`."
        ),
        call = call
      )
    }
    return(TRUE)
  }

  if (is.null(include_criteria)) {
    return(present)
  }
  if (isTRUE(include_criteria) && !present) {
    rlang::abort(
      paste0(
        "`data` has no '",
        value_name,
        "' column. Join a guideline set onto it with join_action_levels(), ",
        "or name the column it was joined into with `criteria_col`."
      ),
      call = call
    )
  }
  isTRUE(include_criteria)
}


#' Answer the questions every reporting function opens by asking
#'
#' @param data chemistry tibble from [data_processor()]
#' @param required columns the calling function cannot work without
#' @param round_col name of the round column, or NULL to find one
#' @param round the round to report on, or NULL for the latest present
#' @param group_vars further columns to group by, or NULL
#' @param value_name name of the guideline value column, or NULL where the
#'   function reports no guideline
#' @param include_criteria what the caller passed for `include_criteria`
#' @param criteria_required a guideline is not optional for this function
#' @param quiet suppress the message naming the round
#' @param call frame errors are attributed to
#' @returns list(picked, group_vars, groups, include_criteria)
#' @noRd
prepare_round_summary <- function(
  data,
  required,
  round_col = NULL,
  round = NULL,
  group_vars = NULL,
  value_name = NULL,
  include_criteria = NULL,
  criteria_required = FALSE,
  quiet = FALSE,
  call = rlang::caller_env()
) {
  require_columns(data, required, call = call)

  # Argument checks first, then the round. Resolving the round messages the
  # user about which one it picked, and a call that is about to be rejected
  # for a bad argument should not announce work it never did.
  include_criteria <- resolve_include_criteria(
    data,
    value_name,
    include_criteria,
    criteria_required,
    call = call
  )

  group_vars <- resolve_group_vars(data, group_vars, call = call)

  picked <- resolve_round(data, round_col, round, quiet = quiet)

  list(
    picked = picked,
    group_vars = group_vars,
    groups = summary_groups(data, group_vars),
    include_criteria = include_criteria
  )
}


#' Name the round a message or warning is about
#'
#' @param picked the resolved round, from [resolve_round()]
#' @returns a single string, e.g. `"monitoring_round = 2024 Q1"`
#' @noRd
round_label <- function(picked) {
  paste0(picked$col, " = ", format(picked$value))
}


#' Write a summary table, creating its directory if need be
#'
#' @param table tibble to write
#' @param path full file path including filename, or NULL to write nothing
#' @noRd
write_summary <- function(table, path) {
  if (is.null(path)) {
    return(invisible(NULL))
  }
  out_dir <- dirname(path)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
    message(glue::glue("Created directory: {out_dir}"))
  }
  writexl::write_xlsx(table, path)
  message(glue::glue("Saved: {basename(path)} -> {path}"))
}


#' min()/max() over a group that may hold nothing but missing values
#'
#' Base R returns `Inf` with a warning for an empty set; a group with no
#' readable concentration has no minimum, and `NA` says so.
#'
#' @param x numeric vector
#' @noRd
safe_min <- function(x) {
  if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
}


#' @rdname safe_min
#' @noRd
safe_max <- function(x) {
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}


# ---------------------------------------------------------------------------
# Reducing a group of results to one row
#
# analyte_summary() and historical_range() both pick out extreme results and
# then read several columns off the row each one landed on. Indexing back into
# the full vectors - rather than subsetting first - is what keeps a prefix
# with the value reported beside it.
# ---------------------------------------------------------------------------

#' The columns a row-reducing summary reads off each group
#'
#' @param df the group's results, without its grouping columns
#' @returns list(conc, detect, usable, prefix), each one row per result
#' @noRd
group_vectors <- function(df) {
  conc <- suppressWarnings(as.numeric(df$concentration))
  list(
    conc = conc,
    detect = !is.na(df$detect_flag) & df$detect_flag == "Y",
    usable = !is.na(conc),
    prefix = if ("prefix" %in% names(df)) {
      as.character(df$prefix)
    } else {
      rep(NA_character_, nrow(df))
    }
  )
}


#' Take the value at a chosen row, or a default where there was none
#'
#' @param v the column to read
#' @param i the chosen row position, possibly `integer(0)`
#' @param empty what to return when nothing was chosen
#' @returns a single value
#' @noRd
value_at <- function(v, i, empty) {
  if (length(i) == 0) empty else v[[i]]
}


#' Find the most extreme usable result within a mask
#'
#' @param conc the group's concentrations
#' @param usable which of them are readable
#' @param mask which results are eligible
#' @param choose `which.min` or `which.max`
#' @returns a single row position, or `integer(0)`
#' @noRd
pick_within <- function(conc, usable, mask, choose) {
  i <- which(mask & usable)
  if (length(i) == 0) integer(0) else i[[choose(conc[i])]]
}


#' The results a maximum may be taken from
#'
#' The maximum is the highest detection. Where nothing was detected there is
#' no detection to report, so every readable result stands in and the `<` is
#' carried through rather than the row coming out blank. Shared so that
#' analyte_summary(), historical_range() and min_max_locations() cannot
#' disagree about what the highest result of a round was.
#'
#' @param detect which results were detections
#' @param usable which results are readable
#' @returns a logical mask
#' @noRd
detected_pool <- function(detect, usable) {
  if (any(detect & usable)) detect else usable
}


#' Drop the location/analyte groups a screening rule excludes, saying which
#'
#' [mann_kendall_test()] screens its groups twice over - on the proportion of
#' non-detects, and on the count of detections - and both screens do the same
#' four things: measure each group, name the ones being dropped, drop them,
#' and refuse to carry on with nothing left. Written once here so the two
#' cannot report themselves differently.
#'
#' @param df nested tibble, one row per location/analyte group
#' @param keep logical vector, one per row: `TRUE` to keep the group
#' @param describe a function of the excluded rows returning one line of text
#'   per group, for the message
#' @param loc_name,ana_name names of the location and analyte columns
#' @param headline a function of the number excluded, returning the first line
#'   of the message
#' @param empty_message what to say when nothing survives
#' @returns `df`, with the excluded rows removed
#' @noRd
exclude_groups <- function(
  df,
  keep,
  describe,
  loc_name,
  ana_name,
  headline,
  empty_message
) {
  excluded <- df[!keep, , drop = FALSE]

  if (nrow(excluded) > 0) {
    message(paste0(
      headline(nrow(excluded)),
      "\n",
      paste(
        sprintf(
          "  - %s / %s (%s)",
          excluded[[loc_name]],
          excluded[[ana_name]],
          describe(excluded)
        ),
        collapse = "\n"
      )
    ))
  }

  df <- df[keep, , drop = FALSE]
  if (nrow(df) == 0) {
    stop(empty_message, call. = FALSE)
  }
  df
}

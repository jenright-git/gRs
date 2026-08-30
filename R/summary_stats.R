#' Summary statistics table
#'
#' One row per location and chemical, with detection counts, the usual
#' descriptive statistics and a set of percentiles. Optionally carries the
#' guideline value joined on by [join_action_levels()] and a count of the
#' results that exceeded it.
#'
#' @param data tibble from [data_processor()].
#' @param save_path full file path, including filename, for the wide-format
#'   export - stats as columns, one row per location/chemical, e.g.
#'   `"output/summary.xlsx"`. The directory is created if it does not exist.
#' @param tidy_path full file path, including filename, for the long-format
#'   export - one row per statistic per location/chemical, with columns
#'   `location_code`, `chem_name`, `stat` and `value`.
#' @param include_criteria include the guideline value and an exceedance
#'   count. The count comes from the `exceedance` column written by
#'   [join_action_levels()], so whether a non-detect above the guideline
#'   counts is decided there by `lor_as_exceedance`, not here.
#' @param criteria_col name of the column holding the guideline value,
#'   matching the `value_col` it was joined under by [join_action_levels()].
#'   Can be given with or without quotes. Default `criteria`. The output
#'   columns take the same name - `criteria_99` and
#'   `criteria_99_exceedance_count` - so two sets summarised separately can be
#'   told apart. Ignored unless `include_criteria = TRUE`.
#'
#' @returns A tibble of summary statistics, invisibly written to `save_path`
#'   and `tidy_path` where those are given.
#' @export
#'
#' @examples
#' summary_stats(gRs_data)
#'
#' # With a guideline set joined on by join_action_levels()
#' \dontrun{
#' summary_stats(compared, include_criteria = TRUE)
#' summary_stats(compared, include_criteria = TRUE, criteria_col = criteria_99)
#' }
#' @importFrom dplyr select group_by summarise arrange n all_of any_of
#'   left_join
#' @importFrom stats quantile sd
#' @importFrom tidyr pivot_longer pivot_wider unnest
#' @importFrom writexl write_xlsx
#' @importFrom glue glue
#' @importFrom rlang enquo quo_name
summary_stats <- function(
  data,
  save_path = NULL,
  tidy_path = NULL,
  include_criteria = FALSE,
  criteria_col = criteria
) {
  value_name <- rlang::quo_name(rlang::enquo(criteria_col))
  cmp <- comparison_columns(value_name)

  # chem_group, fraction and prefix are carried through where the export has
  # them. data_processor() warns rather than errors when chem_group is absent
  # and tells the user the table still works, so this must not error either.
  carried <- c(
    "date",
    "location_code",
    "chem_group",
    "fraction",
    "chem_name",
    "prefix",
    "detect_flag",
    "concentration",
    "output_unit"
  )
  required <- c("location_code", "chem_name", "detect_flag", "concentration")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(
      "`data` is missing required columns: ",
      paste(missing, collapse = ", "),
      ". Pass a table from data_processor()."
    )
  }

  selected_data <- dplyr::select(data, dplyr::any_of(carried))

  if (include_criteria) {
    if (!value_name %in% names(data)) {
      stop(
        "`data` has no '",
        value_name,
        "' column. Join a guideline set onto it with join_action_levels(), ",
        "or name the column it was joined into with `criteria_col`."
      )
    }

    # Carried under the canonical name for the rest of the function, and put
    # back under its own on the way out.
    selected_data$criteria <- as.numeric(data[[value_name]])

    # What counts as an exceedance is settled by join_action_levels() - detects
    # only, or LORs above the guideline too, per its `lor_as_exceedance`. Its
    # verdict is counted as it stands rather than the rule being applied a
    # second time here, where the argument is not available to honour. A
    # criteria column added by hand carries no verdict, and falls back to the
    # detects-only rule.
    selected_data$exceedance <- if (cmp[["exceedance"]] %in% names(data)) {
      as.logical(data[[cmp[["exceedance"]]]])
    } else {
      selected_data$detect_flag == "Y" &
        selected_data$concentration > selected_data$criteria
    }
  }

  summary_table <- selected_data %>%
    dplyr::group_by(location_code, chem_name) %>%
    dplyr::summarise(
      n_samples = n(),
      n_detects = sum(detect_flag == "Y", na.rm = TRUE),
      n_non_detects = sum(detect_flag == "N", na.rm = TRUE),
      pct_detects = round(n_detects / n_samples * 100, 1),
      pct_non_detects = round(n_non_detects / n_samples * 100, 1),
      min = safe_min(concentration),
      mean = mean(concentration, na.rm = TRUE),
      max = safe_max(concentration),
      std_dev = sd(concentration, na.rm = TRUE),
      # na.rm on every one of these, as on every statistic above. A single
      # missing concentration in a group is enough for quantile() to error
      # outright otherwise, taking the whole table with it.
      !!!percentile_exprs(),
      .groups = "drop"
    )

  if (include_criteria) {
    # A location/chemical group can carry more than one criteria value when
    # its results are reported in more than one unit, so the criteria is
    # reduced to a single value per group rather than returned as-is.
    criteria_table <- selected_data %>%
      dplyr::group_by(location_code, chem_name) %>%
      dplyr::summarise(
        criteria = single_criteria(criteria, chem_name),
        exceedance_count = sum(exceedance, na.rm = TRUE),
        .groups = "drop"
      )

    # The set goes back out under the name it was joined in as, so summaries
    # of two sets can be bound together without either losing its identity.
    names(criteria_table)[names(criteria_table) == "criteria"] <- value_name
    names(criteria_table)[names(criteria_table) == "exceedance_count"] <-
      paste0(cmp[["exceedance"]], "_count")

    summary_table <- summary_table %>%
      dplyr::left_join(criteria_table, by = c("location_code", "chem_name"))
  }

  write_summary(summary_table, save_path)

  if (!is.null(tidy_path)) {
    tidy_table <- summary_table %>%
      tidyr::pivot_longer(
        cols = c(-location_code, -chem_name),
        names_to = "stat",
        values_to = "value"
      )
    write_summary(tidy_table, tidy_path)
  }

  summary_table
}

# Percentiles reported by summary_stats(), as a named list of expressions so
# the set is stated once rather than sixteen near-identical lines.
PERCENTILES <- c(5, 10, 20, 25, 50, 70, 75, 80, 85, 90, 95, 99)

#' Build the percentile expressions for summary_stats()
#' @noRd
percentile_exprs <- function() {
  exprs <- lapply(PERCENTILES / 100, function(p) {
    rlang::expr(unname(quantile(concentration, !!p, na.rm = TRUE)))
  })
  stats::setNames(exprs, paste0("p", PERCENTILES))
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

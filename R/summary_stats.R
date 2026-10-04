#' Summary statistics table
#'
#' One row per location and chemical, with detection counts, the usual
#' descriptive statistics and a set of percentiles. Optionally carries the
#' guideline value joined on by [join_action_levels()] and a count of the
#' results that exceeded it.
#'
#' Results are also grouped by `output_unit` and `criteria_set` where `data`
#' carries them, as [analyte_summary()] and [historical_range()] group them.
#' An analyte reported in two units therefore gets a row per unit rather than
#' having its statistics pooled across both, and a table stacked by
#' [criteria_long()] gets a row per guideline set, each counting its results
#' once and its exceedances against its own guideline.
#'
#' The maximum is the highest *detected* result, as [analyte_summary()]
#' reports it: a non-detect says only that the result was below its LOR, so
#' an LOR above every detect is not the highest result, however high the lab
#' set it. Where nothing was detected, the highest result stands in. The
#' minimum is the lowest result, detected or not. `min_nd` and `max_nd` say
#' where that end of the range is a non-detect - `<0.5` rather than `0.5`:
#' the minimum where any non-detect sits at the lowest value, the maximum only
#' where nothing was detected.
#'
#' The mean, standard deviation and percentiles take non-detects at their LOR
#' times `lor_multiplier`, substituted by [half_lor()] - at the full LOR by
#' default, so where an LOR sits above the detects an upper percentile can be
#' higher than the maximum. The counts, minimum and maximum are always of the
#' results as reported: a `<0.5` stays `<0.5`, whatever multiplier the
#' statistics were calculated with. The multiplier is kept in the table's
#' `"lor_multiplier"` attribute, for [summary_stats_to_excel()] to head the
#' columns it changed with.
#'
#' Pass the table on to [summary_stats_to_excel()] for a formatted workbook.
#' The name [join_action_levels()] recorded for each guideline set -
#' `"NEMP 99%"`, say - goes with it, in the table's `"criteria_names"`
#' attribute, to head that set's columns.
#'
#' @param data tibble from [data_processor()].
#' @param save_path full file path, including filename, for the wide-format
#'   export - stats as columns, one row per location/chemical, e.g.
#'   `"output/summary.xlsx"`. The directory is created if it does not exist.
#' @param tidy_path full file path, including filename, for the long-format
#'   export - one row per statistic per location/chemical, with the grouping
#'   columns (`location_code`, `chem_name`, and `output_unit` and
#'   `criteria_set` where present) followed by `stat` and `value`.
#' @param include_criteria include the guideline value and an exceedance
#'   count. The count comes from the `exceedance` column written by
#'   [join_action_levels()], so whether a non-detect above the guideline
#'   counts is decided there by `lor_as_exceedance`, not here.
#' @param criteria_col the column or columns holding the guideline value,
#'   matching the `value_col` each set was joined under by
#'   [join_action_levels()]. One column, or several as
#'   `c(criteria_95, criteria_99)`, with or without quotes; `all_of()` and
#'   helpers such as `starts_with("criteria")` work too. Default `criteria`.
#'   Each set adds two columns, in the order named, under its own name -
#'   `criteria_99` and `criteria_99_exceedance_count` - so the sets sit side
#'   by side, each counted against its own verdicts. Ignored unless
#'   `include_criteria = TRUE`.
#' @param group_vars character vector of further columns to group by, ahead
#'   of the location, e.g. `"monitoring_zone"`, which is how the zone reaches
#'   [summary_stats_to_excel()]. Default `NULL`.
#' @param lor_multiplier what to multiply a non-detect's LOR by for the mean,
#'   standard deviation and percentiles, as [half_lor()] and
#'   [mann_kendall_test()] take it: 1 (default) for the full LOR, 0.5 for
#'   half, 0 for zero.
#'
#' @returns A tibble of summary statistics, one row per `group_vars`,
#'   `location_code`, `chem_name`, and `output_unit` and `criteria_set` where
#'   present, also written to `save_path` and `tidy_path` where those are
#'   given. The long form at `tidy_path` holds the numbers only, without
#'   `min_nd` and `max_nd`. With `include_criteria = TRUE`, the
#'   `"criteria_names"` attribute names each set, `NA` where
#'   [join_action_levels()] recorded no single name.
#' @export
#'
#' @seealso [summary_stats_to_excel()] for the same statistics as a formatted
#'   workbook, and [half_lor()] for the substitution `lor_multiplier` makes.
#'
#' @examples
#' summary_stats(gRs_data)
#'
#' # Non-detects at half their LOR in the mean, SD and percentiles
#' summary_stats(gRs_data, lor_multiplier = 0.5)
#'
#' # With a guideline set joined on by join_action_levels()
#' \dontrun{
#' summary_stats(compared, include_criteria = TRUE)
#' summary_stats(compared, include_criteria = TRUE, criteria_col = criteria_99)
#'
#' # Several sets side by side, a guideline and a count for each
#' summary_stats(
#'   compared,
#'   include_criteria = TRUE,
#'   criteria_col = c(criteria_95, criteria_99)
#' )
#'
#' # Every joined set at once, one row per set
#' compared %>%
#'   criteria_long() %>%
#'   summary_stats(include_criteria = TRUE)
#' }
#'
#' # Grouped by monitoring zone, then written to a formatted workbook
#' gRs_data %>%
#'   summary_stats(group_vars = "monitoring_zone") %>%
#'   summary_stats_to_excel(
#'     save_path = tempfile(fileext = ".xlsx"),
#'     include_zone = TRUE
#'   )
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
  criteria_col = criteria,
  group_vars = NULL,
  lor_multiplier = 1
) {
  crit_col <- rlang::enquo(criteria_col)

  if (
    !is.numeric(lor_multiplier) ||
      length(lor_multiplier) != 1 ||
      is.na(lor_multiplier) ||
      lor_multiplier < 0
  ) {
    stop("`lor_multiplier` must be a single number, 0 or more.")
  }

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
    "output_unit",
    "criteria_set"
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

  group_vars <- if (is.null(group_vars)) {
    character(0)
  } else {
    as.character(group_vars)
  }
  unknown <- setdiff(group_vars, names(data))
  if (length(unknown) > 0) {
    stop(
      "`group_vars` names columns `data` does not have: ",
      paste(unknown, collapse = ", "),
      "."
    )
  }

  # The grouping analyte_summary() and historical_range() use. Without
  # output_unit, results in two units are pooled into one mean; without
  # criteria_set, a table stacked by criteria_long() counts every result once
  # per set and pools the exceedances of all of them.
  groups <- unique(c(
    group_vars,
    "location_code",
    "chem_name",
    intersect(c("output_unit", "criteria_set"), names(data))
  ))

  # Settled before any statistic is calculated, so a misnamed set fails fast.
  sets <- if (include_criteria) {
    picked <- tryCatch(
      names(dplyr::select(data, !!crit_col)),
      vctrs_error_subscript_oob = function(e) e
    )
    if (inherits(picked, "error")) {
      stop(
        "`data` has no ",
        paste0("'", picked$i, "'", collapse = ", "),
        " column. Join a guideline set onto it with join_action_levels(), ",
        "or name the column it was joined into with `criteria_col`."
      )
    }
    picked <- guideline_value_columns(picked, data)
    if (length(picked) == 0) {
      stop(
        "`include_criteria = TRUE`, but `criteria_col` names no guideline ",
        "column."
      )
    }
    picked
  }

  selected_data <- dplyr::select(data, dplyr::any_of(c(group_vars, carried)))
  # The statistics' own copy of the concentrations, so the counts, minimum
  # and maximum are still of the results as the lab reported them.
  selected_data$.value <- substituted_concentration(
    selected_data,
    lor_multiplier
  )

  summary_table <- selected_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
    dplyr::summarise(
      n_samples = n(),
      n_detects = sum(detect_flag == "Y", na.rm = TRUE),
      n_non_detects = sum(detect_flag == "N", na.rm = TRUE),
      pct_detects = round(n_detects / n_samples * 100, 1),
      pct_non_detects = round(n_non_detects / n_samples * 100, 1),
      min = safe_min(concentration),
      mean = mean(.value, na.rm = TRUE),
      max = max_detected(concentration, detect_flag),
      std_dev = sd(.value, na.rm = TRUE),
      # na.rm on every one of these, as on every statistic above. A single
      # missing concentration in a group is enough for quantile() to error
      # outright otherwise, taking the whole table with it.
      !!!percentile_exprs(".value"),
      min_nd = nd_extremes(concentration, detect_flag)[[1]],
      max_nd = nd_extremes(concentration, detect_flag)[[2]],
      .groups = "drop"
    )

  # Each set adds its own pair, in the order named.
  for (set in sets) {
    summary_table <- summary_table %>%
      dplyr::left_join(criteria_summary(data, groups, set), by = groups)
  }
  if (length(sets) > 0) {
    attr(summary_table, "criteria_names") <- recorded_set_names(
      list(data),
      sets
    )
  }
  attr(summary_table, "lor_multiplier") <- lor_multiplier

  write_summary(summary_table, save_path)

  if (!is.null(tidy_path)) {
    # The flags are not statistics, and stacked into `value` they would turn
    # TRUE into a 1 beside the numbers.
    tidy_table <- summary_table %>%
      dplyr::select(-dplyr::any_of(c("min_nd", "max_nd"))) %>%
      tidyr::pivot_longer(
        cols = -dplyr::all_of(groups),
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
#' @param col the column to take the percentiles of
#' @noRd
percentile_exprs <- function(col = "concentration") {
  col <- rlang::sym(col)
  exprs <- lapply(PERCENTILES / 100, function(p) {
    rlang::expr(unname(quantile(!!col, !!p, na.rm = TRUE)))
  })
  stats::setNames(exprs, paste0("p", PERCENTILES))
}

#' Concentrations with non-detects at a multiple of their LOR
#'
#' [half_lor()] reads the non-detects from `prefix`. A table without one is
#' read from `detect_flag` instead, as summary_stats() counts them.
#'
#' @param data the results being summarised
#' @param lor_multiplier what to multiply a non-detect's LOR by
#' @returns a numeric vector, one value per row of `data`
#' @noRd
substituted_concentration <- function(data, lor_multiplier) {
  if ("prefix" %in% names(data)) {
    return(half_lor(data, lor_multiplier)$concentration)
  }
  nd <- data$detect_flag %in% "N"
  ifelse(nd, data$concentration * lor_multiplier, data$concentration)
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

#' The highest detected result
#'
#' The rule [analyte_summary()] reports its maximum by. Where nothing was
#' detected, the highest result stands in, and `nd_extremes()` gives it its
#' "<".
#'
#' @param x,detect_flag one group's concentrations and detect flags
#' @returns a single number, `NA` where no concentration is readable
#' @noRd
max_detected <- function(x, detect_flag) {
  detected <- !is.na(x) & detect_flag %in% "Y"
  if (any(detected)) max(x[detected]) else safe_max(x)
}

#' Whether a group's minimum and maximum are non-detects
#'
#' The minimum is one where any non-detect sits at the lowest value, since
#' "<0.001" is below a detect of 0.001. The maximum is the highest detected
#' result, so it is one only where nothing was detected - and then only where
#' every result at the highest value is a non-detect rather than unflagged.
#'
#' @param concentration,detect_flag one group's results
#' @returns two logicals, for the minimum then the maximum
#' @noRd
nd_extremes <- function(concentration, detect_flag) {
  keep <- !is.na(concentration)
  conc <- concentration[keep]
  nd <- detect_flag[keep] %in% "N"

  if (length(conc) == 0) {
    return(c(FALSE, FALSE))
  }
  c(
    any(nd[conc == min(conc)]),
    !any(detect_flag[keep] %in% "Y") && all(nd[conc == max(conc)])
  )
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

#' One guideline set's value and exceedance count for each summary group
#'
#' @param data the table passed to [summary_stats()]
#' @param groups the columns [summary_stats()] groups by
#' @param set the guideline value column
#' @returns a tibble of the `groups`, then the guideline under the set's own
#'   name and the count under its exceedance column's name plus `_count` -
#'   `criteria_99` and `criteria_99_exceedance_count`
#' @noRd
criteria_summary <- function(data, groups, set) {
  cmp <- comparison_columns(set)
  rows <- data[groups]
  rows$.guideline <- as.numeric(data[[set]])

  # What counts as an exceedance is settled by join_action_levels() - detects
  # only, or LORs above the guideline too, per its `lor_as_exceedance`. Its
  # verdict is counted as it stands rather than the rule being applied a
  # second time here, where the argument is not available to honour. A
  # criteria column added by hand carries no verdict, and falls back to the
  # detects-only rule.
  rows$.exceedance <- if (cmp[["exceedance"]] %in% names(data)) {
    as.logical(data[[cmp[["exceedance"]]]])
  } else {
    data$detect_flag == "Y" & data$concentration > rows$.guideline
  }

  # Grouped by unit and set, a group should hold one criteria value, but a
  # table with no output_unit can still carry several, so it is reduced to
  # a single value per group rather than returned as-is.
  out <- rows %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
    dplyr::summarise(
      .guideline = single_criteria(.guideline, chem_name),
      .count = sum(.exceedance, na.rm = TRUE),
      .groups = "drop"
    )

  # The set goes back out under the name it was joined in as, so summaries
  # of two sets can be bound together without either losing its identity.
  names(out)[names(out) == ".guideline"] <- set
  names(out)[names(out) == ".count"] <- paste0(cmp[["exceedance"]], "_count")
  out
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

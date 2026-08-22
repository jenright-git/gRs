#' Summary statistics table
#'
#' @param data tibble from data_processor
#' @param save_path full file path including filename for the wide-format export, e.g. "C:/project/output/summary.xlsx". Stats are columns, rows are location x chemical. Directory is created if it does not exist.
#' @param tidy_path full file path including filename for the tidy/long-format export. Produces one row per stat per location-chemical pair with columns: location_code, chem_name, stat, value. Directory is created if it does not exist.
#' @param include_criteria logical; if TRUE, includes the guideline value and an exceedance count. The count is taken from the `exceedance` column written by [join_action_levels()], so whether a non-detect above the guideline counts is decided there via its `lor_as_exceedance` argument, not here.
#' @param value_col name of the column holding the guideline value, matching
#'   the `value_col` it was joined under by [join_action_levels()]. Default
#'   `"criteria"`. A set joined under its own name is summarised by naming it
#'   here, and the output columns take that name too - `criteria_99` and
#'   `criteria_99_exceedance_count` - so two sets summarised separately can be
#'   told apart. Ignored unless `include_criteria = TRUE`.
#'
#' @return tibbles and csv files
#' @export
#'
#' @examples summary_stats(df, save_path = "users/project/stats")
#' @importFrom dplyr select group_by summarise arrange n all_of
#' @importFrom stats quantile sd
#' @importFrom tidyr pivot_longer pivot_wider unnest
#' @importFrom writexl write_xlsx
#' @importFrom glue glue
summary_stats <- function(
  data,
  save_path = NULL,
  tidy_path = NULL,
  include_criteria = FALSE,
  value_col = "criteria"
) {
  value_name <- as.character(value_col)[[1]]
  cmp <- comparison_columns(value_name)

  if (include_criteria) {
    if (!value_name %in% names(data)) {
      stop(
        "`data` has no '",
        value_name,
        "' column. Join a guideline set onto it with join_action_levels(), ",
        "or name the column it was joined into with `value_col`."
      )
    }

    # Carried under the canonical name for the rest of the function, and put
    # back under its own on the way out.
    selected_data <- data %>%
      dplyr::select(
        date,
        location_code,
        chem_group,
        fraction,
        chem_name,
        prefix,
        detect_flag,
        concentration,
        output_unit,
        dplyr::all_of(c(criteria = value_name))
      )

    # What counts as an exceedance is settled by join_action_levels() - detects
    # only, or LORs above the guideline too, per its `lor_as_exceedance`. Its
    # verdict is counted as it stands rather than the rule being applied a
    # second time here, where the argument is not available to honour. A
    # criteria column added by hand carries no verdict, and falls back to the
    # detects-only rule.
    if (cmp[["exceedance"]] %in% names(data)) {
      selected_data$exceedance <- as.logical(data[[cmp[["exceedance"]]]])
    } else {
      selected_data$exceedance <- selected_data$detect_flag == "Y" &
        selected_data$concentration > selected_data$criteria
    }
  } else {
    selected_data <- data %>%
      dplyr::select(
        date,
        location_code,
        chem_group,
        fraction,
        chem_name,
        prefix,
        detect_flag,
        concentration,
        output_unit
      )
  }

  summary_table <- selected_data %>%
    dplyr::group_by(location_code, chem_name) %>%
    dplyr::summarise(
      n_samples = n(),
      n_detects = sum(detect_flag == "Y", na.rm = TRUE),
      n_non_detects = sum(detect_flag == "N", na.rm = TRUE),
      pct_detects = round(sum(detect_flag == "Y", na.rm = TRUE) / n() * 100, 1),
      pct_non_detects = round(
        sum(detect_flag == "N", na.rm = TRUE) / n() * 100,
        1
      ),
      min = min(concentration, na.rm = TRUE),
      mean = mean(concentration, na.rm = TRUE),
      max = max(concentration, na.rm = TRUE),
      std_dev = sd(concentration, na.rm = TRUE),
      p5 = quantile(concentration, 0.05),
      p10 = quantile(concentration, 0.10),
      p20 = quantile(concentration, 0.20),
      p25 = quantile(concentration, 0.25),
      p50 = quantile(concentration, 0.50),
      p70 = quantile(concentration, 0.70),
      p75 = quantile(concentration, 0.75),
      p80 = quantile(concentration, 0.80),
      p85 = quantile(concentration, 0.85),
      p90 = quantile(concentration, 0.90),
      p95 = quantile(concentration, 0.95),
      p99 = quantile(concentration, 0.99),
      .groups = "drop"
    ) %>%
    base::unique()

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
      ) %>%
      base::unique()

    # The set goes back out under the name it was joined in as, so summaries
    # of two sets can be bound together without either losing its identity.
    names(criteria_table)[names(criteria_table) == "criteria"] <- value_name
    names(criteria_table)[names(criteria_table) == "exceedance_count"] <-
      paste0(cmp[["exceedance"]], "_count")

    summary_table <- summary_table %>%
      dplyr::left_join(criteria_table, by = c("location_code", "chem_name"))
  }

  if (!is.null(save_path)) {
    out_dir <- dirname(save_path)
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
      message(glue::glue("Created directory: {out_dir}"))
    }
    writexl::write_xlsx(summary_table, save_path)
    message(glue::glue("Saved: {basename(save_path)} -> {save_path}"))
  }

  if (!is.null(tidy_path)) {
    tidy_dir <- dirname(tidy_path)
    if (!dir.exists(tidy_dir)) {
      dir.create(tidy_dir, recursive = TRUE)
      message(glue::glue("Created directory: {tidy_dir}"))
    }
    tidy_table <- summary_table %>%
      tidyr::pivot_longer(
        cols = c(-location_code, -chem_name),
        names_to = "stat",
        values_to = "value"
      )
    writexl::write_xlsx(tidy_table, tidy_path)
    message(glue::glue("Saved: {basename(tidy_path)} -> {tidy_path}"))
  }

  return(summary_table)
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

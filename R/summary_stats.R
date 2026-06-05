#' Summary statistics table
#'
#' @param data tibble from data_processor
#' @param save_path full file path including filename for the wide-format export, e.g. "C:/project/output/summary.xlsx". Stats are columns, rows are location x chemical. Directory is created if it does not exist.
#' @param tidy_path full file path including filename for the tidy/long-format export. Produces one row per stat per location-chemical pair with columns: location_code, chem_name, stat, value. Directory is created if it does not exist.
#' @param include_criteria logical; if TRUE, includes criteria and exceedance_count columns
#'
#' @return tibbles and csv files
#' @export
#'
#' @examples summary_stats(df, save_path = "users/project/stats")
#' @importFrom dplyr select group_by summarise arrange n
#' @importFrom stats quantile sd
#' @importFrom tidyr pivot_longer pivot_wider unnest
#' @importFrom writexl write_xlsx
#' @importFrom glue glue
summary_stats <- function(
  data,
  save_path = NULL,
  tidy_path = NULL,
  include_criteria = FALSE
) {
  if (include_criteria) {
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
        criteria
      )
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
    criteria_table <- selected_data %>%
      dplyr::group_by(location_code, chem_name) %>%
      dplyr::summarise(
        criteria = criteria,
        exceedance_count = sum(concentration > criteria),
        .groups = "drop"
      ) %>%
      base::unique()

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

#' Summary statistics table
#'
#' @param data tibble from data_processor
#' @param save_path path to save summary tables as .xlsx files
#'
#' @return tibbles and csv files
#' @export
#'
#' @examples summary_stats(df, save_path = "users/project/stats")
#' @importFrom dplyr select group_by summarise arrange
#' @importFrom tidyr pivot_longer pivot_wider unnest
#' @importFrom writexl write_xlsx
#' @importFrom glue glue
summary_stats <- function(data, save_path=NULL){

  summary_table <- data %>%
    dplyr::select(date, location_code, chem_group, total_or_filtered, chem_name, prefix, concentration, output_unit, criteria) %>%
    dplyr::group_by(location, analyte) %>%
    dplyr::summarise(observations = n(),
              criteria = criteria,
              exceedance_count = sum(concentration > criteria),
              min = min(concentration, na.rm = T),
              max = max(concentration, na.rm = T),
              average = mean(concentration, na.rm = T),
              `10th Percentile` = quantile(concentration, 0.10),
              `20th Percentile` = quantile(concentration, 0.20),
              `25th Percentile` = quantile(concentration, 0.25),
              `50th Percentile` = quantile(concentration, 0.50),
              `70th Percentile` = quantile(concentration, 0.70),
              `75th Percentile` = quantile(concentration, 0.75),
              `80th Percentile` = quantile(concentration, 0.80),
              `90th Percentile` = quantile(concentration, 0.90),
              `95th Percentile` = quantile(concentration, 0.95),
              `99th Percentile` = quantile(concentration, 0.99),  .groups = "drop") %>%
    base::unique()

  if (!is.null(save_path)) {
    writexl::write_xlsx(summary_table, glue::glue("{save_path}/summary_statistics.xlsx"))



   summary_table_wide <-  summary_table %>%
      tidyr::pivot_longer(cols = c(-location, -analyte), names_to = "stat", values_to = "result") %>%
      dplyr::arrange(stat) %>%
      tidyr::pivot_wider(names_from = analyte, values_from = result) %>%
      tidyr::unnest() %>%
      base::unique()

      writexl::write_xlsx(summary_table_wide, path = glue::glue("{save_path}/summary_statistics_wide_format.xlsx"))
  }

  return(summary_table)

}

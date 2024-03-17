#' Mann_Kendall Function to loop through data set but only lists trends as: Not Significant, Increasing or Decreasing
#'
#' @param data output tibble from data_processor or similar
#'
#' @return tibble with trend data
#' @export
#'
#' @examples mann_kendall_reduced_test(mk_export)
#' @importFrom dplyr bind_rows filter mutate case_when
#' @importFrom tidyr separate tibble
mann_kendall_reduced_test <- function(data){

  establish_plotting_variables(data)

   # Create list to store results for each combination of location and analyte
  results_list <- list()

  # Loop through each combination of location and analyte, perform Mann-Kendall analysis, and store results in list   Changed from 1 to 3 becasue I think is the number of data points needed. mk.test needs at least 3
  for (i in base::unique(data$location_code)) {
    for (j in base::unique(data$chem_name)) {
      subset_data <- data %>% dplyr::filter(location_code == i, chem_name == j)
      if (base::nrow(subset_data) > 3) {
        results_list[[paste0(i, "XXX", j)]] <- mk_analysis(subset_data)
      }
    }
  }

  # Combine results into a data frame
  results_data <- dplyr::bind_rows(lapply(names(results_list), function(x) {
    tidyr::tibble(location_analyte = x,
                  p_value = results_list[[x]][1],
                  tau_statistic = results_list[[x]][2],
                  sample_mean = results_list[[x]][3],
                  SD = results_list[[x]][4],
                  COV = results_list[[x]][5])
  }))

  mk.export <- results_data %>%
    dplyr::mutate(trend = dplyr::case_when(p_value < 0.05 & tau_statistic > 0 ~ "Increasing",
                             p_value < 0.05 & tau_statistic < 0 ~ "Decreasing",
                             p_value > 0.05 ~ "No Significant Trend",
                             TRUE ~ "No Significant Trend"))



  mk.export <- mk.export %>%
    tidyr::separate(col = location_analyte, into = c("location_code", "analyte"), sep = "XXX")


  return(mk.export)

}

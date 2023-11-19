#' Mann Kendall Function to loop through entire dataset
#'
#' @param data tibble processed with the data_processor function
#'
#' @return tibble of increasing trends called mk.export
#' @export
#'
#' @examples mann_kendall_test(data)
#' @importFrom dplyr bind_rows filter mutate case_when
#' @importFrom tidyr separate tibble
mann_kendall_test <- function(data){

    establish_plotting_variables(data)
    # Create list to store results for each combination of location and analyte
    results_list <- list()

    # Loop through each combination of location and analyte, perform Mann-Kendall analysis, and store results in list   Changed from 1 to 3 becasue I think is the number of data points needed. mk.test needs at least 3
    for (i in base::unique(data$location)) {
      for (j in base::unique(data$analyte)) {
        subset_data <- data %>% dplyr::filter(location == i, analyte == j)
        if (base::nrow(subset_data) > 3) {
          results_list[[paste0(i, "-", j)]] <- mk_analysis(subset_data)
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
      #mutate(p_value = ifelse(NaN, yes = "No", no = p_value)) %>%
      dplyr::mutate(trend = dplyr::case_when(
                                              p_value < 0.05 & tau_statistic > 0 ~ "Increasing",
                                              p_value >= 0.05 & p_value <= 0.1 & tau_statistic > 0 ~"Probably Increasing",
                                              p_value > 0.1 & tau_statistic > 0 ~ "No Significant Trend",
                                              p_value > 0.1 & tau_statistic <= 0 & COV >=1 ~ "No Significant Trend",
                                              p_value > 0.1 & tau_statistic <= 0 & COV < 1 ~ "Stable",
                                              p_value >= 0.05 & p_value <= 0.1 & tau_statistic < 0 ~ "Probably Decreasing",
                                              p_value < 0.05 & tau_statistic < 0 ~ "Decreasing",
                                              is.nan(p_value) & SD==0 & COV==0 ~ "Stable" #if all <LOR results then p_value comes back as NA..
      ))



    mk.export <- mk.export %>%
      tidyr::separate(col = location_analyte, into = c("location", "analyte"), sep = "-")


    return(mk.export)

  }


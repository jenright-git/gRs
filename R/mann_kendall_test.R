#' Mann Kendall Function to loop through entire dataset
#'
#' @param data tibble processed with the data_processor function
#' @param traditional perform the standard analysis and not analyse for "Probably" or "Stable" trends
#'
#' @return A nested tibble of trends as well as the original nested data
#' @export
#'
#' @examples mann_kendall_test(data)
#' @importFrom dplyr bind_rows filter mutate case_when
#' @importFrom tidyr tibble nest unnest
#' @importFrom purrr map
mann_kendall_test <- function(data, traditional=FALSE){

  df <- data %>%
    tidyr::nest(.by = c(location_code, chem_name)) %>%
    dplyr::mutate(n_samples = purrr::map(data, nrow)) %>%
    tidyr::unnest(n_samples) %>%
    dplyr::filter(n_samples > 3) %>%  #filter out entries with less than 4 data points
    dplyr::select(-n_samples)

  # Make sure there is at least one set of results to be analysed
  base::stopifnot("No data with more than 3 samples" = nrow(df)>0)

  df <- df %>%
    dplyr::mutate(results = purrr::map(data, mk_analysis)) %>%
    tidyr::unnest(results)

  if(traditional){

    nested_df <- df %>%
      dplyr::mutate(trend = dplyr::case_when(p_value < 0.05 & tau_statistic > 0 ~ "Increasing",
                                             p_value < 0.05 & tau_statistic < 0 ~ "Decreasing",
                                             p_value > 0.05 ~ "No Significant Trend",
                                             TRUE ~ "No Significant Trend"))

  } else {

    nested_df <- df %>%
      dplyr::mutate(trend = dplyr::case_when(
        p_value < 0.05 & tau_statistic > 0 ~ "Increasing",
        p_value >= 0.05 & p_value <= 0.1 & tau_statistic > 0 ~"Probably Increasing",
        p_value > 0.1 & tau_statistic > 0 ~ "No Significant Trend",
        p_value > 0.1 & tau_statistic <= 0 & COV >=1 ~ "No Significant Trend",
        p_value > 0.1 & tau_statistic <= 0 & COV < 1 ~ "Stable",
        p_value >= 0.05 & p_value <= 0.1 & tau_statistic < 0 ~ "Probably Decreasing",
        p_value < 0.05 & tau_statistic < 0 ~ "Decreasing",
        is.nan(p_value) & SD==0 & COV==0 ~ "Stable" ))  #if all <LOR results then p_value comes back as NA..
  }


  return(nested_df)

  }


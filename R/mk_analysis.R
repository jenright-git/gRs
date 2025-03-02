#' Mann_Kendall Test returning test result and stats
#'
#' @param data filtered tibble with column of "concentration"
#'
#' @return tibble with result and stats
#' @export
#'
#' @examples mk_analysis(df)
#' @importFrom trend mk.test
#' @import dplyr
mk_analysis <- function(data) {

  # turn <LOR into zero - Fixes issues with changing LOR.

  data <- data %>%
    dplyr::mutate(mka_concentration = case_when(
      prefix == "<" ~ 0,  # Check for "<" first
      is.na(prefix) ~ concentration,  # Then handle NA values
      TRUE ~ concentration  # Handle all other cases
    ))

  result <- trend::mk.test(data$mka_concentration)

  mk_result <- tibble(

    p_value = result$p.value,
    tau_statistic = result$estimates[3],
    sample_mean = base::mean(data$concentration, na.rm=T),
    SD = stats::sd(data$concentration, na.rm=T),
    COV =  SD / sample_mean

  )
}

#' Mann_Kendall Test returning test result and stats
#'
#' @param data filtered tibble with column of "concentration"
#'
#' @return tibble with result and stats
#' @export
#'
#' @examples mk_analysis(df)
#' @param use_zero convert <LOR to zero for mann kendall test
#' @importFrom trend mk.test
#' @import dplyr tidyr

mk_analysis <- function(data, use_zero=FALSE) {

data <- data %>% arrange(date)

    # turn <LOR into zero - Fixes issues with changing LOR.

if(use_zero){
  data <- data %>%
    dplyr::mutate(mka_concentration = case_when(
      prefix == "<" ~ 0,  # Check for "<" first
      is.na(prefix) ~ concentration,  # Then handle NA values
      TRUE ~ concentration  # Handle all other cases
    )) %>%
    tidyr::drop_na(concentration) %>%
    dplyr::arrange(date)

  result <- trend::mk.test(data$mka_concentration)
} else {


  data <- data %>% tidyr::drop_na(concentration)
  result <- trend::mk.test(data$concentration)
}


  mk_result <- tibble(

    p_value = result$p.value,
    tau_statistic = result$estimates[3],
    S_statistic = result$estimates[1],
    sample_mean = base::mean(data$concentration, na.rm=T),
    SD = stats::sd(data$concentration, na.rm=T),
    COV =  ifelse(is.na(SD / sample_mean), 0, SD / sample_mean)

  )

  return(mk_result)
}

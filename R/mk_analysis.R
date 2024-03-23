#' Mann_Kendall Test returning test result and stats
#'
#' @param data filtered tibble with column of "concentration"
#'
#' @return tibble with result and stats
#' @export
#'
#' @examples mk_analysis(df)
#' @importFrom trend mk.test
mk_analysis <- function(data) {

    result <- trend::mk.test(data$concentration)

  mk_result <- tibble(

    p_value = result$p.value,
    tau_statistic = result$estimates[3],
    sample_mean = base::mean(data$concentration, na.rm=T),
    SD = stats::sd(data$concentration, na.rm=T),
    COV =  SD / sample_mean

  )
}

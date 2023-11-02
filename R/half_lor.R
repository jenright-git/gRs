#' Half concentrations that are < LOR
#'
#' @param data A tibble with columns "prefix" and "concentration". Prefix must be either "<" or "="
#'
#' @return A tibble with modified concentrations
#' @export
#'
#' @examples <4 becomes =2
#' @importFrom dplyr mutate
#' @importFrom tidyr replace_na
half_lor <- function(data){

  modified_data <- data %>%
    mutate(
      prefix = tidyr::replace_na(prefix, "="),
      concentration = ifelse(prefix == "<", concentration * 0.5, concentration),
      prefix = "="
    )

  return(modified_data)

}



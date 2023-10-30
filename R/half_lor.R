
#' Half concentrations that are < LOR
#'
#' @param tibble with columns "prefix" and "concentration". Prefix must be either "<" or "="
#'
#' @return tibble
#' @export
#'
#' @examples <4 becomes =2
half_lor <- function(data){

  data <- data %>%
    dplyr::mutate(prefix = tidyr::replace_na(prefix, "="),
                  concentration = base::ifelse(prefix == "<",
                                               yes = concentration*0.5,
                                               no = concentration),
                  prefix = "=")

  return(data)


}


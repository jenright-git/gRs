#' Establish parameters for automated plots
#'
#' @param data tibble imported from the data_processor function
#'
#' @return lists and vectors for plots
#' @export
#'
#' @examples anlaytes date_range zones locations and colours
#' @importFrom dplyr filter select distinct
#' @importFrom tidyr drop_na
#' @importFrom Polychrome alphabet.colors

establish_plotting_variables <- function(data){

  chem_group <<- base::unique(data$chem_group)

  analytes <<- dplyr::filter(data, chem_group %in% chem_group) %>%
    dplyr::select(chem_name) %>%
    dplyr::distinct() %>%
    tidyr::drop_na() %>%
    base::as.vector() %>%
    base::unlist()

  date_range <<- c(base::min(data$date), base::max(data$date))

  if (all(is.na(data$monitoring_zone))) {
    data <- data %>%
      dplyr::mutate(monitoring_zone = site_id)

    zones <<- base::unique(data$monitoring_zone)
  } else {
    zones <<- base::unique(data$monitoring_zone)
  }


  locations_vec <<- base::unique(data$location_code)

set.seed(1239755)

  colour <-  grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  colours_vec <<- sample(colour,
           size = base::length(locations_vec),
           replace=FALSE)

  # assign locations to colours
  location_colours <<- stats::setNames(colours_vec, locations_vec)

}


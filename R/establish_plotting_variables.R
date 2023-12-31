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

  chemgroup <<- base::unique(data$group)

  analytes <<- dplyr::filter(data, group %in% chemgroup) %>%
    dplyr::select(analyte) %>%
    dplyr::distinct() %>%
    tidyr::drop_na() %>%
    base::as.vector() %>%
    base::unlist()

  date_range <<- c(base::min(data$date), base::max(data$date))

  if (all(is.na(data$zone))) {
    data <- data %>%
      dplyr::mutate(zone = site)

    zones <<- base::unique(data$zone)
  } else {
    zones <<- base::unique(data$zone)
  }


  locations_vec <<- base::unique(data$location)

set.seed(1239755)

  colour <-  grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  colours_vec <<- sample(colour,
           size = base::length(locations_vec),
           replace=FALSE)

  # assign locations to colours
  location_colours <<- stats::setNames(colours_vec, locations_vec)

}


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

  zones <<- base::unique(data$zone)

  locations_vec <<- base::unique(data$location)

  colours_vec <<- Polychrome::alphabet.colors(n = base::length(locations_vec))

  # assign locations to colours
  location_colours <<- stats::setNames(colours_vec, locations_vec)

}


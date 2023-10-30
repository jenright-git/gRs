#' Establish parameters for automated plots
#'
#' @param =esdat data frame imported from the data_processor function
#'
#' @return lists and vectors for plots
#' @export
#'
#' @examples anlaytes, date_range, zones, locations and colours. All lists and vectors used for plotting in plot functions

establish_plotting_variables <- function(data) {
  # What analytes to look for in future plots
  chemgroup <<- dplyr::unique(data$group)

  analytes <<- dplyr::filter(data, group %in% chemgroup) %>%
    dplyr::select(analyte) %>%
    dplyr::distinct() %>%
    tidyr::drop_na() %>%
    base::as.vector() %>%
    base::unlist()

  date_range <<- c(base::min(data$date), base::max(data$date))

  zones <<- dplyr::unique(data$zone)

  locations_vec <<- dplyr::unique(data$location)

  colours_vec <<- Polychrome::alphabet.colors(n = length(locations_vec))

  # assign locations to colours
  location_colours <<- base::setNames(colours_vec, locations_vec)
}


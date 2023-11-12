#' Plots a timeseries_plot for each analyte and saves to designated location
#'
#' @param data tibble of concentration data
#' @param save_path path to directory where plots will be saved
#' @inheritDotParams timeseries_plot
#'
#' @return jpeg of timeseries plots
#' @export
#'
#' @examples plot_by_analyte("my_project/figures")
#' @importFrom dplyr filter select
#' @importFrom ggplot2 ggsave
#' @importFrom glue glue
#' @importFrom purrr pwalk
#' @importFrom tidyr crossing

### Must figure out how to add timeseies_plot arguments
plot_by_analyte <- function(data, save_path=NULL){


  chemgroup <- base::unique(data$group)

  analytes <- dplyr::filter(data, group %in% chemgroup) %>%
    dplyr::select(analyte) %>%
    dplyr::distinct() %>%
    tidyr::drop_na() %>%
    base::as.vector() %>%
    base::unlist()

  date_range <- c(base::min(data$date), base::max(data$date))

  zones <- base::unique(data$zone)

  locations_vec <- base::unique(data$location)

  colours_vec <- Polychrome::alphabet.colors(n = base::length(locations_vec))

  # assign locations to colours
  location_colours <- stats::setNames(colours_vec, locations_vec)




  analyte_zone_combinations <- tidyr::crossing(analyte = analytes, zone = zones) #create tibble of vectors to map over

  plots <- analyte_zone_combinations %>%
    purrr::pwalk(~ {
      i <- ..1
      x <- ..2

      y_unit <- dplyr::filter(data, analyte == i) %>%
        dplyr::select(units) %>% base::unique()  # Example of a base function

     # limit <- base::unique(dplyr::filter(data, analyte == i)$criteria)  # Example of a base function


      data %>%
        dplyr::filter(zone == x, analyte == i) %>%
        timeseries_plot(.)

      number <- dplyr::filter(data, zone == x, analyte == i)

      if (nrow(number) > 1) {
        ggplot2::ggsave(glue::glue("{save_path}/{x}/{i}-plot.png"), height = 10, width = 14, units = "cm")
        #message(x, " ", i, " Saved")
      } else {
        base::message(x, " ", i, " No Data")  # Example of a base function
      }


    }, .progress = TRUE)



}




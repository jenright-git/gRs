#' Plots a timeseries_plot for each analyte and saves to designated location
#'
#' @param data tibble of concentration data
#' @param save_path path to directory where plots will be saved
#' @param smooth Apply a geom_smooth to the plot
#' @param linear_smooth Apply a linear trendline to the plot
#' @inheritDotParams timeseries_plot
#'
#' @return jpeg of timeseries plots
#' @export
#'
#' @examples plot_by_analyte("my_project/figures")
#' @importFrom dplyr filter select
#' @importFrom ggplot2 ggsave geom_smooth
#' @importFrom glue glue
#' @importFrom purrr pwalk
#' @importFrom tidyr crossing

### Must figure out how to add timeseies_plot arguments

plot_by_analyte <- function(data, save_path=NULL, smooth=FALSE, linear_smooth=FALSE){

  chemgroup <- base::unique(data$group)

  analytes <- dplyr::filter(data, group %in% chemgroup) %>%
    dplyr::select(analyte) %>%
    dplyr::distinct() %>%
    tidyr::drop_na() %>%
    base::as.vector() %>%
    base::unlist()

  date_range <- c(base::min(data$date), base::max(data$date))

  if (all(is.na(data$zone))) {
    data <- data %>%
      dplyr::mutate(zone = site)

    zones <- base::unique(data$zone)
  } else {
    zones <- base::unique(data$zone)
  }



  locations_vec <- base::unique(data$location)

  #colours_vec <- Polychrome::alphabet.colors(n = base::length(locations_vec))

  set.seed(221294)

  colour = grDevices::colors()[base::grep('gr(a|e)y', grDevices::colors(), invert = T)]

  colours_vec <- base::sample(colour,
                         size = base::length(locations_vec),
                         replace=FALSE)


  # assign locations to colours
  location_colours <- stats::setNames(colours_vec, locations_vec)


 # establish_plotting_variables(data)



  analyte_zone_combinations <- tidyr::crossing(analyte = analytes, zone = zones) #create tibble of vectors to map over

  plots <- analyte_zone_combinations %>%
    purrr::pwalk(~ {
      i <- ..1
      x <- ..2

      y_unit <- dplyr::filter(data, analyte == i) %>%
        dplyr::select(units) %>% base::unique()

     # limit <- base::unique(dplyr::filter(data, analyte == i)$criteria)  # Example of a base function


     plot1 <-  data %>%
        dplyr::filter(zone == x, analyte == i) %>%
        timeseries_plot(., y_unit = unique(.$units))+


       if(smooth==TRUE){
          plot + geom_smooth(method = 'loess', formula = 'y ~ x', se = FALSE, size=0.5)
       }

        if(linear_smooth==TRUE){
          plot + geom_smooth(method="lm", formula = 'y ~ x', se=FALSE, size=0.5)
        }

     plot1

      number <- dplyr::filter(data, zone == x, analyte == i)

      if (nrow(number) > 1) {
        ggplot2::ggsave(glue::glue("{save_path}/{x}/{i}-plot.png"), height = 10, width = 14, units = "cm")
        #message(x, " ", i, " Saved")
      } else {
        base::message(x, " ", i, " No Data")
      }


    }, .progress = TRUE)



}




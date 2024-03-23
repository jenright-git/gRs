#' Plots a timeseries_plot for each analyte and saves to designated location
#'
#' @param data tibble of concentration data
#' @param save_path path to directory where plots will be saved
#' @param smooth Apply a geom_smooth to the plot
#' @param linear_smooth Apply a linear trendline to the plot
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
plot_by_analyte <- function(data, save_path=NULL, smooth=FALSE, linear_smooth=FALSE, ...){

  chemgroup <- base::unique(data$chem_group)

  analytes <- dplyr::filter(data, chem_group %in% chemgroup) %>%
    dplyr::select(chem_name) %>%
    dplyr::distinct() %>%
    tidyr::drop_na() %>%
    base::as.vector() %>%
    base::unlist()

  date_range <- c(base::min(data$date), base::max(data$date))

  if (all(is.na(data$monitoring_zone))) {
    data <- data %>%
      dplyr::mutate(monitoring_zone = site_id)

    zones <- base::unique(data$monitoring_zone)
  } else {
    zones <- base::unique(data$monitoring_zone)
  }



  locations_vec <- base::unique(data$location_code)

  #colours_vec <- Polychrome::alphabet.colors(n = base::length(locations_vec))


  colour = grDevices::colors()[base::grep('gr(a|e)y', grDevices::colors(), invert = T)]

  set.seed(24168)
  colours_vec <- base::sample(colour,
                         size = base::length(locations_vec),
                         replace=FALSE)


  # assign locations to colours
  location_colours <- stats::setNames(colours_vec, locations_vec)


 # establish_plotting_variables(data)



  analyte_zone_combinations <- tidyr::crossing(chem_name = analytes, monitoring_zone = zones) #create tibble of vectors to map over

  plots <- analyte_zone_combinations %>%
    purrr::pwalk(~ {
      i <- ..1
      x <- ..2

      y_unit <- dplyr::filter(data, chem_name == i) %>%
        dplyr::select(output_unit) %>% base::unique()

     # limit <- base::unique(dplyr::filter(data, analyte == i)$criteria)  # Example of a base function


      plot1 <-  data %>%
        dplyr::filter(monitoring_zone == x, chem_name == i) %>%
        timeseries_plot(., y_unit = unique(.$output_unit))


       if(smooth==TRUE){
        plot1 <-  plot1 + geom_smooth(method = 'loess', formula = 'y ~ x', se = FALSE, size=0.5)
       }

        if(linear_smooth==TRUE){
          plot1 + geom_smooth(method="lm", formula = 'y ~ x', se=FALSE, size=0.5)
        }

     plot1

      number <- dplyr::filter(data, monitoring_zone == x, chem_name == i)

      if (nrow(number) > 1) {
        ggplot2::ggsave(glue::glue("{save_path}/{x}/{i}-plot.png"), height = 10, width = 14, units = "cm")
        #message(x, " ", i, " Saved")
      } else {
        base::message(x, " ", i, " No Data")
      }


    }, .progress = TRUE)



}




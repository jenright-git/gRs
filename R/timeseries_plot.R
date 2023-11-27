#' Timeseries Plot with standard formatting
#'
#' @param data tibble
#' @param date_size size of x-axis date labels
#' @param date_break date breaks to be used ("2 weeks")
#' @param date_label format of date label (%b-%y)
#' @param x_angle angle of x axis text
#' @param legend_text_size size of the text in the legend
#' @param y_title_size size of y_axis label
#' @param y_unit unit to display on y_axis heading
#'
#' @return a timeseries plot based on the scale and date range of the dataset
#' @export
#'
#' @examples timeseries_plot(data)
#' @importFrom ggplot2 ggplot aes geom_point geom_path scale_colour_manual geom_line geom_hline theme_bw ylim labs scale_x_datetime theme element_text element_blank
#' @importFrom openair quickText
#' @importFrom glue glue
timeseries_plot <-  function(data, date_size=10, date_break="year", date_label="%b-%y",
                             x_angle=90, legend_text_size=10, y_title_size=10, y_unit="mg/L"){

  y_unit <- y_unit

  #establish_plotting_variables(data)

  # y_unit <- df %>%
  #   filter(analyte == i) %>%
  #   select(units) %>% unique()
  #
  # limit <- df[df$analyte == i, ]$criteria %>%
  #   unique()

 plot <-  data %>%
   ggplot2::ggplot(aes(x = date, y = concentration, colour = location)) +
   ggplot2::geom_point(size = 0.6, alpha = 0.5) +
   ggplot2::geom_path() +
   ggplot2::scale_color_manual(values = location_colours) +  # Specify color scheme for locations
   #ggplot2::geom_line(aes(x = date, y = criteria), linetype = "dashed", colour = "black") +
   #ggplot2::geom_hline(yintercept = limit, linetype = "dashed", colour = "black") +
   ggplot2::theme_bw() +
   ggplot2::ylim(c(0, NA)) +
   ggplot2::labs(x = NULL, y = openair::quickText(glue::glue("Concentration ({y_unit})"))) +
   ggplot2::scale_x_datetime(
     date_breaks = date_break,
     date_labels = date_label,
     limits = date_range
   ) +
   ggplot2::theme(
     legend.position = "bottom",
     legend.title = ggplot2::element_blank(),
     legend.text = ggplot2::element_text(size = legend_text_size),
     plot.title = ggplot2::element_text(hjust = 0.5, size = 10, face = "bold"),
     plot.subtitle = ggplot2::element_text(size = 6, face = "italic"),
     axis.title.y = ggplot2::element_text(size = y_title_size),
     axis.text.x = ggplot2::element_text(angle = x_angle, size = date_size)
   )
 # ggplot2::ggtitle(label = i, subtitle = x)


  return(plot)


}


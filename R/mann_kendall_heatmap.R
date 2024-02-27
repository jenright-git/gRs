#' Function to plot mann-kendall trends on a heatmap
#'
#' @param data tibble from the mann_kendall_test function
#' @param label_text_size size of the label text
#' @param plot_title Title Text
#' @param heatmap_colours vector of colours to use for each category/tile
#'http://127.0.0.1:12443/graphics/plot_zoom_png?width=1920&height=1017
#' @return ggplot heatmap
#' @export
#'
#' @examples mann_kendall_heatmap(mk_export)
#' @importFrom dplyr mutate
#' @import ggplot2
#' @importFrom stringr str_wrap
mann_kendall_heatmap <- function(data, label_text_size=3.2,
                                 plot_title="Mann-Kendall Trend Analysis",
                                 heatmap_colours=NULL, width=20){

  if (is.null(heatmap_colours)) {
    heatmap_colours <- c("#E9A3C9", "#FDE0EF", "whitesmoke","#E6F5D0", "#A1D76A", "#4D9221EE")
  }


  heatmap <- data %>%
    dplyr::mutate(trend = factor(trend, levels = c("Increasing",
                                                   "Probably Increasing",
                                                   "No Significant Trend",
                                                   "Stable",
                                                   "Probably Decreasing",
                                                   "Decreasing"))) %>%
    ggplot2::ggplot(aes(x = location_code, y = analyte, fill = trend)) +
    ggplot2::geom_tile(colour = "black", ) +
    ggplot2::scale_fill_manual(values = heatmap_colours,
                      breaks = c("Increasing",
                                 "Probably Increasing",
                                 "No Significant Trend",
                                 "Stable",
                                 "Probably Decreasing",
                                 "Decreasing")) +
    ggplot2::geom_text(aes(label = stringr::str_wrap(trend, width = width)),
              colour = "black",
              size = label_text_size) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(plot_title) +
    ggplot2::theme(legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          panel.grid = element_blank(),
          axis.text.x = element_text(angle = 75, vjust = 0.5)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0))

  return(heatmap)

}

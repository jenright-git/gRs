#' Black and White Heatmap for Mann-Kendall Test
#'
#' @param data tibble exported from mann_kendall_reduced_test
#' @param label_text_size size of trend labels
#' @param plot_title Text for plot title
#'
#' @return ggplot heatmap
#' @export
#'
#' @examples mann_kendall_heatmap_bw(mk_export_reduced)
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot geom_tile aes theme_bw ggtitle theme element_blank element_text labs scale_x_discrete scale_y_discrete
#' @importFrom stringr str_wrap
#' @importFrom ggtext geom_richtext
#' @importFrom glue glue
mann_kendall_heatmap_bw <- function(data, label_text_size=3.5, plot_title="Mann-Kendall Trend Analysis", width=20){

  data %>%
    dplyr::mutate(trend = factor(trend, levels = c("Increasing", "No Significant Trend", "Decreasing"))) %>%
    ggplot2::ggplot(aes(x = location_code, y = chem_name)) +
    ggplot2::geom_tile(colour = "black", fill = "white") +
    ggtext::geom_richtext(ggplot2::aes(label = base::ifelse(base::grepl("Increasing|Decreasing", trend),
                                                            glue::glue("<b>{str_wrap(trend, width = 20)}</b>"),
                                                            stringr::str_wrap(trend, width = width))),
                          colour = "black", size = label_text_size, label.color="white") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(plot_title) +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   panel.grid = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 0, vjust = 0.5)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0))
}

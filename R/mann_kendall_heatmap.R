#' Heatmap of Mann-Kendall trends
#'
#' One tile per location and analyte, filled by trend direction.
#'
#' @param data tibble from [mann_kendall_test()].
#' @param label_text_size size of the trend label inside each tile
#' @param plot_title title text
#' @param heatmap_colours colours for the six trend categories, in the order
#'   Increasing, Probably Increasing, No Significant Trend, Stable, Probably
#'   Decreasing, Decreasing. `NULL` (default) uses a diverging pink-to-green
#'   scale.
#' @param width maximum character width for wrapping trend labels
#'
#' @returns A ggplot2 object.
#' @export
#'
#' @seealso [mann_kendall_heatmap_bw()] for a print-friendly version taking
#'   the three categories of `mann_kendall_test(traditional = TRUE)`.
#'
#' @examples
#' mann_kendall_test(gRs_data) %>%
#'   mann_kendall_heatmap()
#' @importFrom dplyr mutate
#' @import ggplot2
#' @importFrom stringr str_wrap
mann_kendall_heatmap <- function(
  data,
  label_text_size = 3.2,
  plot_title = "Mann-Kendall Trend Analysis",
  heatmap_colours = NULL,
  width = 20
) {
  if (is.null(heatmap_colours)) {
    heatmap_colours <- c(
      "#E9A3C9",
      "#FDE0EF",
      "whitesmoke",
      "#E6F5D0",
      "#A1D76A",
      "#4D9221EE"
    )
  }

  data %>%
    dplyr::mutate(trend = factor(trend, levels = TREND_LEVELS)) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = location_code,
      y = chem_name,
      fill = trend
    )) +
    ggplot2::geom_tile(colour = "black") +
    ggplot2::scale_fill_manual(
      values = heatmap_colours,
      breaks = TREND_LEVELS
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = stringr::str_wrap(trend, width = width)),
      colour = "black",
      size = label_text_size
    ) +
    heatmap_theme(plot_title, x_angle = 75)
}

# The six categories mann_kendall_test() assigns, ordered from increasing to
# decreasing so the fill scale reads as a gradient.
TREND_LEVELS <- c(
  "Increasing",
  "Probably Increasing",
  "No Significant Trend",
  "Stable",
  "Probably Decreasing",
  "Decreasing"
)

# The three categories mann_kendall_test(traditional = TRUE) assigns.
TREND_LEVELS_TRADITIONAL <- c(
  "Increasing",
  "No Significant Trend",
  "Decreasing"
)

#' Shared theme and axis treatment for the trend heatmaps
#'
#' @param plot_title title text
#' @param x_angle angle of the x-axis labels
#' @returns a list of ggplot2 components
#' @noRd
heatmap_theme <- function(plot_title, x_angle = 0) {
  list(
    ggplot2::theme_bw(),
    ggplot2::ggtitle(plot_title),
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = x_angle, vjust = 0.5)
    ),
    ggplot2::labs(x = NULL, y = NULL),
    ggplot2::scale_x_discrete(expand = c(0, 0)),
    ggplot2::scale_y_discrete(expand = c(0, 0))
  )
}

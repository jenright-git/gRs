#' Black and white heatmap of Mann-Kendall trends
#'
#' The same grid as [mann_kendall_heatmap()] with no fill, for printing. A
#' significant trend is emboldened in place of being coloured.
#'
#' Takes the three trend categories of `mann_kendall_test(traditional = TRUE)`;
#' the six-category default has no unshaded equivalent for "Probably
#' Increasing" and "Stable", and those tiles would come out blank.
#'
#' @param data tibble from `mann_kendall_test(traditional = TRUE)`.
#' @param label_text_size size of the trend label inside each tile
#' @param plot_title title text
#' @param width maximum character width for wrapping trend labels
#'
#' @returns A ggplot2 object.
#' @export
#'
#' @seealso [mann_kendall_heatmap()] for the coloured six-category version.
#'
#' @examples
#' mann_kendall_test(gRs_data, traditional = TRUE) |>
#'   mann_kendall_heatmap_bw()
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot geom_tile aes theme_bw ggtitle theme
#'   element_blank element_text labs scale_x_discrete scale_y_discrete
#' @importFrom stringr str_wrap
#' @importFrom ggtext geom_richtext
#' @importFrom glue glue
mann_kendall_heatmap_bw <- function(
  data,
  label_text_size = 3.5,
  plot_title = "Mann-Kendall Trend Analysis",
  width = 20
) {
  data %>%
    dplyr::mutate(
      trend = factor(trend, levels = TREND_LEVELS_TRADITIONAL),
      # Emboldened rather than filled, so the tile stays readable in black and
      # white. Wrapped before the markup goes on, or the tags are counted
      # towards the width.
      .label = stringr::str_wrap(as.character(trend), width = width),
      .label = ifelse(
        grepl("Increasing|Decreasing", trend),
        glue::glue("<b>{.label}</b>"),
        .label
      )
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = location_code, y = chem_name)) +
    ggplot2::geom_tile(colour = "black", fill = "white") +
    ggtext::geom_richtext(
      ggplot2::aes(label = .label),
      colour = "black",
      size = label_text_size,
      label.color = "white"
    ) +
    heatmap_theme(plot_title, x_angle = 0)
}

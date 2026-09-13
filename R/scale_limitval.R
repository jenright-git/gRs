#' Position scales for continuous data (x & y) with labelled vertical/horizontal
#' markers
#'
#' This function is a wrapper around [ggplot2::scale_x_continuous()] or
#' [ggplot2::scale_y_continuous()] which allows users to automatically draw
#' labelled horizontal or vertical markers at specified points. In the air
#' quality world, this may be particularly useful to display limit values, such
#' as those defined in legislation or recommended by health authorities.
#'
#' This function uses [ggplot2::sec_axis()] to display the line labels. One can
#' therefore not pass [ggplot2::sec_axis()] to \code{...}. If users wish to use
#' [ggplot2::sec_axis()] it is recommended to use [ggplot2::geom_abline()] to
#' draw lines manually.
#'
#' Unlike other "scale" functions the order in which this function is added to
#' the [ggplot2::ggplot()] object matters. Adding \code{scale_*_limitval()}
#' *after* [ggplot2::geom_line()] will draw the line markers on top of the trend
#' line, whereas adding it *before* will draw the markers below.
#'
#' @param marker_values Numeric vector of values at which to draw marker lines.
#' @param marker_colours Character vector of colours for marker lines. Should be
#'   the same length as \code{marker_values}.
#' @param marker_labels Character vector of labels for marker lines. Should be
#'   the same length as \code{marker_values}. Defaults to using the numeric
#'   values given in \code{marker_values}.
#' @param marker_linetypes Vector of values for marker linetypes. Should be the
#'   same length as \code{marker_values}. Defaults to dashed lines (2).
#' @param trans The name of a transformation object. See
#'   [ggplot2::scale_x_continuous()] for more information.
#' @param ... Other arguments to pass to
#'   [ggplot2::scale_x_continuous()]/[ggplot2::scale_y_continuous()].
#'
#' @returns A list of ggplot2 components: one marker line per value, plus the
#'   position scale carrying their labels on a secondary axis.
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(gRs_data, aes(x = date, y = concentration)) +
#'   geom_point() +
#'   scale_y_limitval(c(10, 50), marker_labels = c("Guideline", "Trigger"))
#'
#' ggplot(gRs_data, aes(x = concentration, y = chem_name)) +
#'   geom_point() +
#'   scale_x_limitval(10, marker_colours = "red")
#'
#' @name scale_limitval
#' @aliases NULL
#' @importFrom purrr pmap
#' @importFrom ggplot2 geom_hline geom_vline sec_axis scale_x_continuous
#'   scale_y_continuous
NULL

#' Build one axis's marker lines and the scale carrying their labels
#'
#' `scale_x_limitval()` and `scale_y_limitval()` differ only in the axis they
#' act on, so the axis is the argument and the body is written once.
#'
#' @param axis `"x"` or `"y"`
#' @inheritParams scale_y_limitval
#' @returns a list of ggplot2 components
#' @noRd
limitval_scale <- function(
  axis,
  marker_values,
  marker_colours = "black",
  marker_labels = marker_values,
  marker_linetypes = 5,
  trans = "identity",
  ...
) {
  axis <- match.arg(axis, c("x", "y"))
  horizontal <- identical(axis, "y")

  marker <- if (horizontal) ggplot2::geom_hline else ggplot2::geom_vline
  intercept <- if (horizontal) "yintercept" else "xintercept"
  position_scale <- if (horizontal) {
    ggplot2::scale_y_continuous
  } else {
    ggplot2::scale_x_continuous
  }

  out <- purrr::pmap(
    list(marker_values, marker_colours, marker_linetypes),
    function(value, colour, linetype) {
      do.call(
        marker,
        stats::setNames(
          list(value, colour, linetype),
          c(intercept, "colour", "lty")
        )
      )
    }
  )

  append(
    out,
    position_scale(
      trans = trans,
      ...,
      sec.axis = ggplot2::sec_axis(
        ~.,
        breaks = marker_values,
        labels = marker_labels
      )
    )
  )
}

#' @rdname scale_limitval
#' @export
scale_y_limitval <- function(
  marker_values,
  marker_colours = "black",
  marker_labels = marker_values,
  marker_linetypes = 5,
  trans = "identity",
  ...
) {
  limitval_scale(
    "y",
    marker_values = marker_values,
    marker_colours = marker_colours,
    marker_labels = marker_labels,
    marker_linetypes = marker_linetypes,
    trans = trans,
    ...
  )
}

#' @rdname scale_limitval
#' @export
scale_x_limitval <- function(
  marker_values,
  marker_colours = "black",
  marker_labels = marker_values,
  marker_linetypes = 5,
  trans = "identity",
  ...
) {
  limitval_scale(
    "x",
    marker_values = marker_values,
    marker_colours = marker_colours,
    marker_labels = marker_labels,
    marker_linetypes = marker_linetypes,
    trans = trans,
    ...
  )
}

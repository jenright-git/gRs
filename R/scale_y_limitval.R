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
#' @name scale_limitval
#' @aliases NULL
NULL

#' @rdname scale_limitval
#' @export
scale_y_limitval <-
  function(marker_values,
           marker_colours = "black",
           marker_labels = marker_values,
           marker_linetypes = 5,
           trans = "identity",
           ...) {
    out <-
      purrr::pmap(
        list(marker_values, marker_colours, marker_linetypes),
        ~ ggplot2::geom_hline(yintercept = ..1, colour = ..2, lty = ..3)
      )

    out <- append(
      out,
      ggplot2::scale_y_continuous(
        trans = trans,
        ...,
        sec.axis = ggplot2::sec_axis(
          ~.,
          breaks = marker_values,
          labels = marker_labels
        )
      )
    )

    out
  }

#' @rdname scale_limitval
#' @export
scale_x_limitval <-
  function(marker_values,
           marker_colours = "black",
           marker_labels = marker_values,
           marker_linetypes = 5,
           trans = "identity",
           ...) {
    out <-
      purrr::pmap(
        list(marker_values, marker_colours, marker_linetypes),
        ~ ggplot2::geom_hline(xintercept = ..1, colour = ..2, lty = ..3)
      )

    out <- append(
      out,
      ggplot2::scale_x_continuous(
        trans = trans,
        ...,
        sec.axis = ggplot2::sec_axis(
          ~.,
          breaks = marker_values,
          labels = marker_labels
        )
      )
    )

    out
  }

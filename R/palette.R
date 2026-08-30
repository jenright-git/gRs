#' Build a named colour vector for a set of labels
#'
#' The one place the package picks colours, so a location keeps the same
#' colour whether it was coloured by [timeseries_plot()] or looked up through
#' [get_plotting_variables()].
#'
#' RColorBrewer's Set1 is used where the package is installed, ramped out
#' beyond eight labels. The fallback samples the non-grey base colours, and
#' does so through [withr::with_seed()] so the caller's random stream is left
#' exactly as it was found - drawing a plot should not change the next
#' `runif()`.
#'
#' @param labels labels to colour, one colour each
#' @param seed seed for the fallback palette; `NULL` draws from the stream as
#'   it stands rather than seeding it
#' @returns named character vector of colours, one per label
#' @noRd
gRs_colours <- function(labels, seed = 1239755) {
  labels <- as.character(labels)
  n <- length(labels)
  if (n == 0) {
    return(stats::setNames(character(0), character(0)))
  }

  if (requireNamespace("RColorBrewer", quietly = TRUE)) {
    cols <- if (n <= 8) {
      RColorBrewer::brewer.pal(max(3, n), "Set1")[seq_len(n)]
    } else {
      grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(n)
    }
    return(stats::setNames(cols, labels))
  }

  pool <- grDevices::colors()
  pool <- pool[!grepl("gr(a|e)y", pool)]
  draw <- function() sample(pool, size = n, replace = n > length(pool))
  cols <- if (is.null(seed)) draw() else withr::with_seed(seed, draw())
  stats::setNames(cols, labels)
}

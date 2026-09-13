# ---------------------------------------------------------------------------
# Monitoring zones
#
# Plots are grouped by monitoring zone, which not every export records.
# get_plotting_variables() and plot_by_analyte() both need the same fallback
# to site_id and used to carry a copy each - and the copies disagreed, so a
# table carrying neither column drew a warning from one and then an error from
# the other. Decided once here, with the caller saying how hard a miss is.
# ---------------------------------------------------------------------------

#' Resolve the column that groups results into monitoring zones
#'
#' `monitoring_zone` where the table records one, `site_id` where it does not
#' or where every zone is missing. Which of those happened is said once, by
#' this function, so the two callers cannot describe it differently.
#'
#' @param data the chemistry table
#' @param zone_col quosure naming the monitoring zone column
#' @param site_col quosure naming the site id column, the fallback
#' @param on_missing what to do when neither column can supply a zone:
#'   `"warn"` returns no zones and carries on, `"error"` stops.
#' @param call frame an error is attributed to
#' @returns list(data, zones); `data` carries the zone column, filled from
#'   `site_id` where that was the fallback, and `zones` are its unique values
#' @noRd
resolve_zone_column <- function(
  data,
  zone_col,
  site_col,
  on_missing = c("warn", "error"),
  call = rlang::caller_env()
) {
  on_missing <- match.arg(on_missing)
  zone_name <- rlang::as_label(zone_col)
  site_name <- rlang::as_label(site_col)

  has_zone <- zone_name %in% names(data)
  has_site <- site_name %in% names(data)

  # A zone column present but entirely missing is the same situation as no
  # zone column at all: nothing to group by.
  if (has_zone) {
    zone_values <- dplyr::pull(data, !!zone_col)
    if (!all(is.na(zone_values))) {
      return(list(
        data = data,
        zones = unique(zone_values[!is.na(zone_values)])
      ))
    }
    empty_reason <- paste0("All ", zone_name, " values are NA")
  } else {
    empty_reason <- paste0("Column ", zone_name, " not found")
  }

  if (has_site) {
    data <- dplyr::mutate(data, !!zone_col := !!site_col)
    message(empty_reason, ". Using ", site_name, " as zones.")
    return(list(data = data, zones = unique(dplyr::pull(data, !!zone_col))))
  }

  problem <- paste0(
    empty_reason,
    ", and no ",
    site_name,
    " column to fall back on, so results cannot be grouped into zones."
  )
  if (identical(on_missing, "error")) {
    rlang::abort(problem, call = call)
  }
  warning(problem, " Zones will be empty.", call. = FALSE)
  list(data = data, zones = character(0))
}

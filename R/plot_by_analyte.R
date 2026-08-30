#' Create and save timeseries plots for each analyte
#'
#' Generates individual timeseries plots for each combination of analyte and
#' monitoring zone, and saves them to the specified directory.
#'
#' @param data A tibble or data frame of concentration data
#' @param save_path Character. Path to directory where plots will be saved.
#'   If NULL, plots are not saved. Default is NULL
#' @param smooth Logical. Apply a loess smooth to the plot? Default is FALSE
#' @param linear_smooth Logical. Apply a linear trendline to the plot? Default is FALSE
#' @param location_col Name of the column containing location codes.
#'   Can be provided with or without quotes. Default is location_code
#' @param chem_name_col Name of the column containing chemical/analyte names.
#'   Can be provided with or without quotes. Default is chem_name
#' @param chem_group_col Name of the column containing chemical groups.
#'   Can be provided with or without quotes. Default is chem_group
#' @param date_col Name of the column containing dates.
#'   Can be provided with or without quotes. Default is date
#' @param concentration_col Name of the column containing concentration values.
#'   Can be provided with or without quotes. Default is concentration
#' @param monitoring_zone_col Name of the column containing monitoring zones.
#'   Can be provided with or without quotes. Default is monitoring_zone
#' @param site_id_col Name of the column containing site IDs.
#'   Can be provided with or without quotes. Default is site_id
#' @param output_unit_col Name of the column containing output units.
#'   Can be provided with or without quotes. Default is output_unit
#' @param show_progress Logical. Show progress bar? Default is TRUE
#' @param create_dirs Logical. Write each zone's plots into its own
#'   subdirectory of `save_path`. Default TRUE. With FALSE every plot is
#'   written to `save_path` itself, with the zone carried in the file name
#' @param plot_width Numeric. Width of saved plots in cm. Default is 14
#' @param plot_height Numeric. Height of saved plots in cm. Default is 10
#' @param ... Additional arguments passed to [timeseries_plot()]
#'
#' @return Invisibly, a tibble of every analyte/zone combination attempted,
#'   with columns `monitoring_zone`, `chem_name`, `filepath` and `status`
#'   (one of `"saved"`, `"not_saved"`, `"no_data"`, `"error"`,
#'   `"save_error"`).
#' @export
#'
#' @examples
#' # Build the plots without writing anything
#' plot_by_analyte(gRs_data, show_progress = FALSE)
#'
#' # Write them out, one directory per monitoring zone
#' \dontrun{
#' plot_by_analyte(gRs_data, save_path = "figures")
#'
#' # All in one directory instead, with a loess smooth
#' plot_by_analyte(gRs_data, save_path = "figures",
#'                 create_dirs = FALSE, smooth = TRUE)
#'
#' # Anything timeseries_plot() takes is passed through
#' plot_by_analyte(gRs_data, save_path = "figures",
#'                 ymax = 100, date_break = "3 months")
#' }
#'
#' @importFrom dplyr filter select distinct pull mutate
#' @importFrom ggplot2 ggsave geom_smooth
#' @importFrom glue glue
#' @importFrom purrr pmap
#' @importFrom tidyr crossing tibble
#' @importFrom rlang enquo quo_name !! :=

plot_by_analyte <- function(
  data,
  save_path = NULL,
  smooth = FALSE,
  linear_smooth = FALSE,
  location_col = location_code,
  chem_name_col = chem_name,
  chem_group_col = chem_group,
  date_col = date,
  concentration_col = concentration,
  monitoring_zone_col = monitoring_zone,
  site_id_col = site_id,
  output_unit_col = output_unit,
  show_progress = TRUE,
  create_dirs = TRUE,
  plot_width = 14,
  plot_height = 10,
  ...
) {
  # Quote the column name arguments
  location_col_q <- rlang::enquo(location_col)
  chem_name_col_q <- rlang::enquo(chem_name_col)
  chem_group_col_q <- rlang::enquo(chem_group_col)
  date_col_q <- rlang::enquo(date_col)
  conc_col_q <- rlang::enquo(concentration_col)
  zone_col_q <- rlang::enquo(monitoring_zone_col)
  site_col_q <- rlang::enquo(site_id_col)
  unit_col_q <- rlang::enquo(output_unit_col)

  # Convert to strings for validation
  location_name <- rlang::quo_name(location_col_q)
  chem_name_name <- rlang::quo_name(chem_name_col_q)
  chem_group_name <- rlang::quo_name(chem_group_col_q)
  date_name <- rlang::quo_name(date_col_q)
  conc_name <- rlang::quo_name(conc_col_q)
  zone_name <- rlang::quo_name(zone_col_q)
  site_name <- rlang::quo_name(site_col_q)
  unit_name <- rlang::quo_name(unit_col_q)

  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame or tibble")
  }

  required_cols <- c(
    location_name,
    chem_name_name,
    chem_group_name,
    date_name,
    conc_name,
    unit_name
  )
  missing_cols <- required_cols[!required_cols %in% names(data)]

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (nrow(data) == 0) {
    stop("Data contains no rows")
  }

  if (!is.null(save_path) && !is.character(save_path)) {
    stop("'save_path' must be a character string or NULL")
  }

  # Get plotting variables
  plot_vars <- get_plotting_variables(
    data,
    location_col = !!location_col_q,
    chem_name_col = !!chem_name_col_q,
    chem_group_col = !!chem_group_col_q,
    date_col = !!date_col_q,
    monitoring_zone_col = !!zone_col_q,
    site_id_col = !!site_col_q
  )

  analytes <- plot_vars$analytes
  date_range <- plot_vars$date_range
  zones <- plot_vars$zones
  location_colours <- plot_vars$location_colours

  # Handle monitoring zones
  if (zone_name %in% names(data)) {
    zone_values <- dplyr::pull(data, !!zone_col_q)
    if (all(is.na(zone_values))) {
      if (site_name %in% names(data)) {
        data <- data %>%
          dplyr::mutate(!!zone_col_q := !!site_col_q)
        message("Using site_id as monitoring_zone")
      } else {
        stop("monitoring_zone is all NA and site_id column not found")
      }
    }
  } else if (site_name %in% names(data)) {
    data <- data %>%
      dplyr::mutate(!!zone_col_q := !!site_col_q)
    message("monitoring_zone column not found. Using site_id")
  } else {
    stop("Neither monitoring_zone nor site_id column found")
  }

  # Where each zone's plots are written. With create_dirs = FALSE they all go
  # to save_path itself, and the zone is carried in the file name instead -
  # writing into a per-zone directory that was never created is how every
  # ggsave() used to fail.
  zone_dir <- function(zone) {
    if (create_dirs) file.path(save_path, safe_filename(zone)) else save_path
  }

  if (!is.null(save_path)) {
    if (!dir.exists(save_path)) {
      dir.create(save_path, recursive = TRUE)
      message("Created directory: ", save_path)
    }
    if (create_dirs) {
      for (zone in zones) {
        dir.create(zone_dir(zone), recursive = TRUE, showWarnings = FALSE)
      }
    }
  }

  combinations <- tidyr::crossing(
    analyte_val = analytes,
    zone_val = zones
  )

  # Captured, not forwarded. purrr passes each row of `combinations` to the
  # mapped function as named arguments, which land in a formula lambda's own
  # `...` - so `timeseries_plot(..., ...)` inside one splices `zone_val` and
  # `analyte_val` into timeseries_plot() rather than the arguments the caller
  # gave plot_by_analyte(). Every plot errors out that way.
  extra_args <- list(...)

  one_plot <- function(analyte_val, zone_val) {
    row <- function(status, filepath = NA_character_) {
      tidyr::tibble(
        monitoring_zone = zone_val,
        chem_name = analyte_val,
        filepath = as.character(filepath),
        status = status
      )
    }

    plot_data <- data %>%
      dplyr::filter(
        !!zone_col_q == zone_val,
        !!chem_name_col_q == analyte_val
      )

    # One point draws no line, so there is nothing to plot rather than
    # something that failed.
    if (nrow(plot_data) <= 1) {
      message(zone_val, " ", analyte_val, " - no data (n=", nrow(plot_data), ")")
      return(row("no_data"))
    }

    y_unit <- unique(dplyr::pull(plot_data, !!unit_col_q))
    if (length(y_unit) > 1) {
      warning(
        "Multiple units found for ",
        analyte_val,
        " in ",
        zone_val,
        ": ",
        paste(y_unit, collapse = ", "),
        ". Using the first: ",
        y_unit[1]
      )
      y_unit <- y_unit[1]
    }

    plot1 <- tryCatch(
      do.call(
        timeseries_plot,
        c(
          list(
            plot_data,
            date_col = date_col_q,
            concentration_col = conc_col_q,
            location_col = location_col_q,
            chem_name_col = chem_name_col_q,
            y_unit = y_unit,
            dates_range = date_range,
            location_colours = location_colours
          ),
          extra_args
        )
      ),
      error = function(e) {
        warning(
          "Error creating plot for ",
          zone_val,
          " ",
          analyte_val,
          ": ",
          conditionMessage(e)
        )
        NULL
      }
    )

    if (is.null(plot1)) {
      return(row("error"))
    }

    if (smooth) {
      plot1 <- plot1 +
        ggplot2::geom_smooth(
          method = "loess",
          formula = y ~ x,
          se = FALSE,
          linewidth = 0.5
        )
    }
    if (linear_smooth) {
      plot1 <- plot1 +
        ggplot2::geom_smooth(
          method = "lm",
          formula = y ~ x,
          se = FALSE,
          linewidth = 0.5
        )
    }

    if (is.null(save_path)) {
      return(row("not_saved"))
    }

    stem <- if (create_dirs) {
      safe_filename(analyte_val)
    } else {
      paste0(safe_filename(zone_val), "-", safe_filename(analyte_val))
    }
    filepath <- file.path(zone_dir(zone_val), paste0(stem, "-plot.png"))

    tryCatch(
      {
        ggplot2::ggsave(
          filepath,
          plot = plot1,
          height = plot_height,
          width = plot_width,
          units = "cm"
        )
        row("saved", filepath)
      },
      error = function(e) {
        warning(
          "Error saving plot for ",
          zone_val,
          " ",
          analyte_val,
          ": ",
          conditionMessage(e)
        )
        row("save_error")
      }
    )
  }

  results_df <- dplyr::bind_rows(purrr::pmap(
    combinations,
    one_plot,
    .progress = show_progress
  ))

  # Report
  if (!is.null(save_path)) {
    n_saved <- sum(results_df$status == "saved", na.rm = TRUE)
    n_no_data <- sum(results_df$status == "no_data", na.rm = TRUE)
    n_errors <- sum(
      results_df$status %in% c("error", "save_error"),
      na.rm = TRUE
    )

    message("\n--- Plot Summary ---")
    message("Successfully saved: ", n_saved)
    message("No data: ", n_no_data)
    message("Errors: ", n_errors)
    message("Total combinations: ", nrow(results_df))
  }

  return(invisible(results_df))
}

#' Make a string safe to use as a file or directory name
#'
#' Analyte names carry characters Windows will not accept in a path -
#' `Nitrate/Nitrite as N` and `1,2:3,4-Diepoxybutane` both fail - and ggsave()
#' reports only that it could not open a connection. Replaced rather than
#' dropped, so two analytes differing by a slash keep separate files.
#'
#' @param x character vector
#' @returns `x` with path-hostile characters replaced by `-`
#' @noRd
safe_filename <- function(x) {
  out <- gsub("[\\\\/:*?\"<>|]+", "-", as.character(x))
  out <- gsub("\\s+", " ", trimws(out))
  # A trailing dot or space is silently stripped by Windows, which would let
  # two analytes collapse onto one file.
  out <- sub("[. ]+$", "", out)
  # Nothing but separators is not a name; two such analytes would
  # otherwise share a file.
  ifelse(grepl("[A-Za-z0-9]", out), out, "unnamed")
}

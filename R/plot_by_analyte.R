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
#' @param create_dirs Logical. Create subdirectories for each zone? Default is TRUE
#' @param plot_width Numeric. Width of saved plots in cm. Default is 14
#' @param plot_height Numeric. Height of saved plots in cm. Default is 10
#' @param ... Additional arguments passed to timeseries_plot()
#'
#' @return Invisibly returns a tibble of successfully created plots with columns:
#'   monitoring_zone, chem_name, filepath, status
#' @export
#'
#' @examples
#' # Save plots to a directory
#' plot_by_analyte(data, save_path = "my_project/figures")
#'
#' # With smoothing
#' plot_by_analyte(data, save_path = "figures", smooth = TRUE)
#'
#' # With linear trend
#' plot_by_analyte(data, save_path = "figures", linear_smooth = TRUE)
#'
#' # Custom column names
#' plot_by_analyte(data,
#'                 save_path = "figures",
#'                 location_col = site_code,
#'                 chem_name_col = parameter,
#'                 date_col = sample_date)
#'
#' # Pass additional arguments to timeseries_plot
#' plot_by_analyte(data,
#'                 save_path = "figures",
#'                 ymin = 0,
#'                 ymax = 100,
#'                 date_break = "3 months")
#'
#' @importFrom dplyr filter select distinct pull mutate
#' @importFrom ggplot2 ggsave geom_smooth
#' @importFrom glue glue
#' @importFrom purrr pwalk
#' @importFrom tidyr crossing drop_na
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

  # Extract variables
  chemgroup <- plot_vars$chem_group
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

  # Create save_path directory if it doesn't exist
  if (!is.null(save_path)) {
    if (!dir.exists(save_path)) {
      dir.create(save_path, recursive = TRUE)
      message("Created directory: ", save_path)
    }

    # Create subdirectories for each zone if requested
    if (create_dirs) {
      for (zone in zones) {
        zone_dir <- file.path(save_path, zone)
        if (!dir.exists(zone_dir)) {
          dir.create(zone_dir, recursive = TRUE)
        }
      }
    }
  }

  # Create tibble of all analyte-zone combinations
  analyte_zone_combinations <- tidyr::crossing(
    chem_name = analytes,
    monitoring_zone = zones
  )

  # Initialize results tracker
  results <- list()

  # Create plots
  plots <- analyte_zone_combinations %>%
    purrr::pwalk(
      ~ {
        analyte_val <- ..1
        zone_val <- ..2

        # Filter data for this combination
        plot_data <- data %>%
          dplyr::filter(
            !!zone_col_q == zone_val,
            !!chem_name_col_q == analyte_val
          )

        # Check if there's data to plot
        if (nrow(plot_data) <= 1) {
          if (!is.null(save_path)) {
            message(
              zone_val,
              " ",
              analyte_val,
              " - No Data (n=",
              nrow(plot_data),
              ")"
            )
          }
          results[[paste(zone_val, analyte_val, sep = "_")]] <<- list(
            monitoring_zone = zone_val,
            chem_name = analyte_val,
            filepath = NA_character_,
            status = "no_data"
          )
          return(NULL)
        }

        # Get the unit for this analyte
        y_unit <- plot_data %>%
          dplyr::pull(!!unit_col_q) %>%
          unique()

        # Handle multiple units
        if (length(y_unit) > 1) {
          warning(
            "Multiple units found for ",
            analyte_val,
            " in ",
            zone_val,
            ": ",
            paste(y_unit, collapse = ", "),
            ". Using first: ",
            y_unit[1]
          )
          y_unit <- y_unit[1]
        }

        # Create base plot
        plot1 <- tryCatch(
          {
            timeseries_plot(
              plot_data,
              date_col = !!date_col_q,
              concentration_col = !!conc_col_q,
              location_col = !!location_col_q,
              analyte_col = !!chem_name_col_q,
              y_unit = y_unit,
              dates_range = date_range,
              location_colours = location_colours,
              ...
            )
          },
          error = function(e) {
            warning(
              "Error creating plot for ",
              zone_val,
              " ",
              analyte_val,
              ": ",
              e$message
            )
            return(NULL)
          }
        )

        if (is.null(plot1)) {
          results[[paste(zone_val, analyte_val, sep = "_")]] <<- list(
            monitoring_zone = zone_val,
            chem_name = analyte_val,
            filepath = NA_character_,
            status = "error"
          )
          return(NULL)
        }

        # Add smoothing if requested
        if (smooth) {
          plot1 <- plot1 +
            ggplot2::geom_smooth(
              method = 'loess',
              formula = 'y ~ x',
              se = FALSE,
              linewidth = 0.5
            )
        }

        if (linear_smooth) {
          plot1 <- plot1 +
            ggplot2::geom_smooth(
              method = "lm",
              formula = 'y ~ x',
              se = FALSE,
              linewidth = 0.5
            )
        }

        # Save plot if save_path is provided
        if (!is.null(save_path)) {
          filepath <- glue::glue(
            "{save_path}/{zone_val}/{analyte_val}-plot.png"
          )

          tryCatch(
            {
              ggplot2::ggsave(
                filepath,
                plot = plot1,
                height = plot_height,
                width = plot_width,
                units = "cm"
              )
              # message(zone_val, " ", analyte_val, " - Saved")
              results[[paste(zone_val, analyte_val, sep = "_")]] <<- list(
                monitoring_zone = zone_val,
                chem_name = analyte_val,
                filepath = filepath,
                status = "saved"
              )
            },
            error = function(e) {
              warning(
                "Error saving plot for ",
                zone_val,
                " ",
                analyte_val,
                ": ",
                e$message
              )
              results[[paste(zone_val, analyte_val, sep = "_")]] <<- list(
                monitoring_zone = zone_val,
                chem_name = analyte_val,
                filepath = NA_character_,
                status = "save_error"
              )
            }
          )
        } else {
          results[[paste(zone_val, analyte_val, sep = "_")]] <<- list(
            monitoring_zone = zone_val,
            chem_name = analyte_val,
            filepath = NA_character_,
            status = "not_saved"
          )
        }
      },
      .progress = show_progress
    )

  # Convert results to tibble
  results_df <- dplyr::bind_rows(results)

  # Print summary
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

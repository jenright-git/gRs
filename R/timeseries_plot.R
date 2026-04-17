#' Timeseries Plot with standard formatting
#'
#' @param data A tibble or data frame containing timeseries data
#' @param filter_location Character vector. Specific location(s) to plot.
#'   If NULL (default), plots all locations. If multiple locations provided,
#'   plots will be colored by location
#' @param filter_analyte Character vector. Specific analyte(s) to plot.
#'   If NULL (default), plots all analytes. If multiple analytes provided,
#'   plots will be faceted by analyte with free y-scales
#' @param date_col Name of the column containing dates.
#'   Can be provided with or without quotes. Default is date
#' @param concentration_col Name of the column containing concentration values.
#'   Can be provided with or without quotes. Default is concentration
#' @param location_col Name of the column containing location codes.
#'   Can be provided with or without quotes. Default is location_code
#' @param analyte_col Name of the column containing analyte/chemical names.
#'   Can be provided with or without quotes. Default is chem_name
#' @param date_size Numeric. Size of x-axis date labels. Default is 12
#' @param date_break Character. Date breaks to be used (e.g., "2 weeks", "month", "year").
#'   Default is "month"
#' @param date_label Character. Format of date label (e.g., "%b-%y", "%Y-%m-%d").
#'   Default is "%b-%y"
#' @param dates_range Date range vector to appear on x-axis. If NULL (default),
#'   uses the full range of dates in the data
#' @param x_angle Numeric. Angle of x-axis text. Default is 90
#' @param legend_text_size Numeric. Size of text in the legend. Default is 10
#' @param y_title_size Numeric. Size of y-axis label. Default is 10
#' @param y_unit Character. Unit to display on y-axis heading. Default is "mg/L"
#' @param ymin Numeric. Minimum value on y-axis. Default is 0
#' @param ymax Numeric or NULL. Maximum value on y-axis. If NULL (default),
#'   ggplot2 automatically calculates the upper limit based on the data
#' @param location_colours Named vector of colors for each location. If NULL (default),
#'   automatically generates colors. Names should match location codes
#' @param criteria_col Optional. Name of column containing criteria/limit values to plot
#'   as horizontal dashed lines. Default is NULL (no criteria line)
#' @param criteria_colour Character. Colour for criteria line. Default is "black"
#' @param criteria_linetype Numeric or character. Line type for criteria line. Default is "dashed"
#' @param plot_title Character. Optional title for the plot. Default is NULL
#' @param plot_subtitle Character. Optional subtitle for the plot. Default is NULL
#' @param n_facet_cols Numeric. Number of columns when faceting by analyte. Default is 2
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' # Basic usage with default column names
#' timeseries_plot(data)
#'
#' # Filter to a single location
#' timeseries_plot(data, filter_location = "LOC_01")
#'
#' # Filter to multiple locations (colored by location)
#' timeseries_plot(data, filter_location = c("LOC_01", "LOC_02", "LOC_03"))
#'
#' # Filter to a single analyte
#' timeseries_plot(data, filter_analyte = "Lead")
#'
#' # Filter to multiple analytes (faceted by analyte with free y-scales)
#' timeseries_plot(data, filter_analyte = c("Lead", "Copper", "Zinc"))
#'
#' # Combine filters: specific locations and analytes
#' timeseries_plot(data,
#'                 filter_location = c("LOC_01", "LOC_02"),
#'                 filter_analyte = c("Lead", "Copper"))
#'
#' # Custom column names without quotes
#' timeseries_plot(data,
#'                 date_col = sample_date,
#'                 concentration_col = result,
#'                 location_col = site_code,
#'                 analyte_col = parameter)
#'
#' # With criteria line
#' timeseries_plot(data,
#'                 filter_location = "LOC_01",
#'                 criteria_col = guideline_value)
#'
#' # Custom date formatting and colors
#' my_colors <- c("Site1" = "blue", "Site2" = "red", "Site3" = "green")
#' timeseries_plot(data,
#'                 filter_location = c("Site1", "Site2", "Site3"),
#'                 location_colours = my_colors,
#'                 date_break = "3 months",
#'                 date_label = "%Y-%m")
#'
#' # With title and custom y-axis limits
#' timeseries_plot(data,
#'                 filter_analyte = "Lead",
#'                 plot_title = "Lead Concentrations",
#'                 plot_subtitle = "2020-2024",
#'                 ymin = 0,
#'                 ymax = 100)
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_path scale_colour_manual
#'   theme_light labs scale_x_datetime theme element_text element_blank
#'   scale_y_continuous geom_hline ggtitle facet_wrap
#' @importFrom openair quickText
#' @importFrom glue glue
#' @importFrom rlang enquo quo_name !! sym
#' @importFrom dplyr pull filter

timeseries_plot <- function(
  data,
  filter_location = NULL,
  filter_analyte = NULL,
  date_col = date,
  concentration_col = concentration,
  location_col = location_code,
  analyte_col = chem_name,
  date_size = 12,
  date_break = "month",
  date_label = "%b-%y",
  dates_range = NULL,
  x_angle = 90,
  legend_text_size = 10,
  y_title_size = 10,
  y_unit = "mg/L",
  ymin = 0,
  ymax = NULL,
  location_colours = NULL,
  criteria_col = NULL,
  criteria_colour = "black",
  criteria_linetype = "dashed",
  plot_title = NULL,
  plot_subtitle = NULL,
  n_facet_cols = 2
) {
  # Quote the column name arguments
  date_col_q <- rlang::enquo(date_col)
  conc_col_q <- rlang::enquo(concentration_col)
  location_col_q <- rlang::enquo(location_col)
  analyte_col_q <- rlang::enquo(analyte_col)
  criteria_col_q <- rlang::enquo(criteria_col)

  # Convert to strings for validation
  date_name <- rlang::quo_name(date_col_q)
  conc_name <- rlang::quo_name(conc_col_q)
  location_name <- rlang::quo_name(location_col_q)
  analyte_name <- rlang::quo_name(analyte_col_q)
  criteria_name <- rlang::quo_name(criteria_col_q)

  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame or tibble")
  }

  required_cols <- c(date_name, conc_name, location_name, analyte_name)
  missing_cols <- required_cols[!required_cols %in% names(data)]

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Validate criteria column if specified
  if (
    !is.null(criteria_col) &&
      criteria_name != "NULL" &&
      !criteria_name %in% names(data)
  ) {
    warning(
      "Criteria column '",
      criteria_name,
      "' not found in data. Skipping criteria line."
    )
    criteria_name <- NULL
  }

  # Apply filters if specified
  original_n_rows <- nrow(data)

  if (!is.null(filter_location)) {
    if (!is.character(filter_location)) {
      stop("'filter_location' must be a character vector")
    }

    available_locations <- unique(dplyr::pull(data, !!location_col_q))
    missing_locations <- setdiff(filter_location, available_locations)

    if (length(missing_locations) > 0) {
      warning(
        "Requested locations not found in data: ",
        paste(missing_locations, collapse = ", ")
      )
    }

    data <- data %>%
      dplyr::filter(!!location_col_q %in% filter_location)
  }

  if (!is.null(filter_analyte)) {
    if (!is.character(filter_analyte)) {
      stop("'filter_analyte' must be a character vector")
    }

    available_analytes <- unique(dplyr::pull(data, !!analyte_col_q))
    missing_analytes <- setdiff(filter_analyte, available_analytes)

    if (length(missing_analytes) > 0) {
      warning(
        "Requested analytes not found in data: ",
        paste(missing_analytes, collapse = ", ")
      )
    }

    data <- data %>%
      dplyr::filter(!!analyte_col_q %in% filter_analyte)
  }

  # Check for sufficient data after filtering
  if (nrow(data) == 0) {
    stop(
      "No data remaining after filtering. ",
      "Original rows: ",
      original_n_rows,
      ", ",
      "After filtering: 0"
    )
  }

  # Extract unique locations (after filtering)
  locations_vec <- base::unique(dplyr::pull(data, !!location_col_q))
  n_locations_filtered <- length(locations_vec)

  # Extract unique analytes (after filtering)
  analytes_vec <- base::unique(dplyr::pull(data, !!analyte_col_q))
  n_analytes_filtered <- length(analytes_vec)

  # Determine if we should color by location (multiple locations)
  color_by_location <- n_locations_filtered > 1

  # Determine if we should facet by analyte (multiple analytes)
  facet_by_analyte <- n_analytes_filtered > 1

  # Generate colors if not provided (only needed if coloring by location)
  if (color_by_location && is.null(location_colours)) {
    n_locations <- length(locations_vec)

    # Try to use RColorBrewer if available
    if (requireNamespace("RColorBrewer", quietly = TRUE) && n_locations > 0) {
      if (n_locations <= 8) {
        colours_vec <- RColorBrewer::brewer.pal(
          max(3, n_locations),
          "Set1"
        )[1:n_locations]
      } else {
        colours_vec <- grDevices::colorRampPalette(
          RColorBrewer::brewer.pal(8, "Set1")
        )(n_locations)
      }
    } else {
      # Fallback to filtered colors if RColorBrewer not available
      set.seed(1239755)
      colour <- grDevices::colors()[grep(
        'gr(a|e)y',
        grDevices::colors(),
        invert = TRUE
      )]
      colours_vec <- sample(
        colour,
        size = n_locations,
        replace = FALSE
      )
    }

    location_colours <- stats::setNames(colours_vec, locations_vec)
  } else if (color_by_location && !is.null(location_colours)) {
    # Validate provided colors
    if (!is.null(names(location_colours))) {
      missing_locations <- setdiff(locations_vec, names(location_colours))
      if (length(missing_locations) > 0) {
        warning(
          "Some locations missing from location_colours: ",
          paste(missing_locations, collapse = ", "),
          ". Generating colors for these locations."
        )
        # Generate colors for missing locations
        set.seed(1239755)
        colour <- grDevices::colors()[grep(
          'gr(a|e)y',
          grDevices::colors(),
          invert = TRUE
        )]
        additional_colours <- sample(
          colour,
          size = length(missing_locations),
          replace = FALSE
        )
        additional_colours <- stats::setNames(
          additional_colours,
          missing_locations
        )
        location_colours <- c(location_colours, additional_colours)
      }
    }
  }

  # Set date range if not provided
  if (is.null(dates_range)) {
    dates_range <- c(
      min(dplyr::pull(data, !!date_col_q), na.rm = TRUE),
      max(dplyr::pull(data, !!date_col_q), na.rm = TRUE)
    )
  }

  # Validate numeric parameters
  if (!is.numeric(date_size) || date_size <= 0) {
    stop("'date_size' must be a positive number")
  }
  if (!is.numeric(x_angle)) {
    stop("'x_angle' must be numeric")
  }
  if (!is.numeric(legend_text_size) || legend_text_size <= 0) {
    stop("'legend_text_size' must be a positive number")
  }
  if (!is.numeric(y_title_size) || y_title_size <= 0) {
    stop("'y_title_size' must be a positive number")
  }
  if (!is.numeric(ymin)) {
    stop("'ymin' must be numeric")
  }
  if (!is.null(ymax) && (!is.numeric(ymax) || ymax <= ymin)) {
    stop("'ymax' must be numeric and greater than ymin")
  }

  # Set y-axis limits (NULL means use data range)
  y_limits <- if (is.null(ymax)) {
    c(ymin, NA) # NA allows ggplot2 to calculate upper limit
  } else {
    c(ymin, ymax)
  }

  # Create the base plot
  if (color_by_location) {
    # Multiple locations - color by location
    plot <- data %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = !!date_col_q,
          y = !!conc_col_q,
          colour = !!location_col_q
        )
      ) +
      ggplot2::geom_point(size = 0.6, alpha = 0.5) +
      ggplot2::geom_path() +
      ggplot2::scale_colour_manual(values = location_colours)
  } else {
    # Single location - no color aesthetic needed
    plot <- data %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = !!date_col_q,
          y = !!conc_col_q
        )
      ) +
      ggplot2::geom_point(size = 0.6, alpha = 0.5) +
      ggplot2::geom_path()
  }

  # Add common plot elements
  plot <- plot +
    ggplot2::theme_light() +
    ggplot2::labs(
      x = NULL,
      y = openair::quickText(glue::glue("Concentration ({y_unit})"))
    ) +
    ggplot2::scale_x_datetime(
      date_breaks = date_break,
      date_labels = date_label,
      limits = dates_range
    ) +
    ggplot2::scale_y_continuous(
      limits = y_limits
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = legend_text_size),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 10, face = "bold"),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5,
        size = 8,
        face = "italic"
      ),
      axis.title.y = ggplot2::element_text(size = y_title_size),
      axis.text.x = ggplot2::element_text(angle = x_angle, size = date_size),
      strip.background = element_rect(fill = NA, colour = 'black'),
      strip.text = element_text(colour = "black")
    )

  # Add faceting if multiple analytes
  if (facet_by_analyte) {
    plot <- plot +
      ggplot2::facet_wrap(
        rlang::as_label(analyte_col_q),
        scales = "free_y",
        ncol = n_facet_cols
      )
  }

  # Add criteria line if specified
  if (
    !is.null(criteria_col) &&
      criteria_name != "NULL" &&
      criteria_name %in% names(data)
  ) {
    criteria_value <- unique(dplyr::pull(data, !!criteria_col_q))

    # If multiple criteria values, use the first non-NA one
    criteria_value <- criteria_value[!is.na(criteria_value)]

    if (length(criteria_value) > 0) {
      if (length(criteria_value) > 1) {
        warning(
          "Multiple criteria values found: ",
          paste(criteria_value, collapse = ", "),
          ". Using first value: ",
          criteria_value[1]
        )
      }

      plot <- plot +
        ggplot2::geom_hline(
          yintercept = criteria_value[1],
          linetype = criteria_linetype,
          colour = criteria_colour,
          linewidth = 0.7
        )
    }
  }

  # Add title and subtitle if provided
  if (!is.null(plot_title) || !is.null(plot_subtitle)) {
    plot <- plot +
      ggplot2::ggtitle(label = plot_title, subtitle = plot_subtitle)
  }

  return(plot)
}

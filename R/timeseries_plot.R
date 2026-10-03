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
#' @param chem_name_col Name of the column containing analyte/chemical names.
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
#'   ggplot2 automatically calculates the upper limit based on the data.
#'   Results outside `ymin` and `ymax` are kept, running off the edge of the
#'   panel, rather than dropped from the plot
#' @param location_colours Named vector of colors for each location. If NULL (default),
#'   automatically generates colors. Names should match location codes
#' @param criteria_col Optional. Name of column containing criteria/limit values to plot
#'   as horizontal dashed lines. Default is NULL (no criteria line). Each panel
#'   gets the guideline of the analyte drawn in it, and where several analytes
#'   share a panel each gets its own line, in its own colour. Where one
#'   analyte's column holds more than one value the lowest is drawn - the same
#'   one [summary_stats()] counts exceedances against - and the rest are
#'   warned about. Several values for one guideline usually means the results
#'   are reported in more than one unit, in which case filter to a single unit
#'   before plotting: no single line is right for both
#' @param criteria_colour Character. Colour for criteria line. Default is "black"
#' @param criteria_linetype Numeric or character. Line type for criteria line. Default is "dashed"
#' @param plot_title Character. Optional title for the plot. Default is NULL
#' @param plot_subtitle Character. Optional subtitle for the plot. Default is NULL
#' @param n_facet_cols Numeric. Number of columns when faceting. Default is 2
#' @param facet_by Character. Whether to facet by \code{"analyte"} (default) or \code{"location"}.
#'   When \code{"analyte"}, multiple locations are coloured by location.
#'   When \code{"location"}, multiple analytes are coloured by analyte.
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' analyte <- gRs_data$chem_name[1]
#'
#' # Every location, one analyte
#' timeseries_plot(gRs_data, filter_analyte = analyte)
#'
#' # Several analytes - faceted, with free y-scales
#' timeseries_plot(gRs_data, filter_analyte = unique(gRs_data$chem_name)[1:3])
#'
#' # Faceted by location instead, and coloured by analyte
#' timeseries_plot(
#'   gRs_data,
#'   filter_analyte = unique(gRs_data$chem_name)[1:2],
#'   facet_by = "location"
#' )
#'
#' # Title, axis limits and date formatting
#' timeseries_plot(
#'   gRs_data,
#'   filter_analyte = analyte,
#'   plot_title = analyte,
#'   date_break = "3 months",
#'   date_label = "%Y-%m",
#'   ymax = 100
#' )
#'
#' # A guideline line, from join_action_levels()
#' \dontrun{
#' timeseries_plot(compared, filter_analyte = "Copper", criteria_col = criteria)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_path scale_colour_manual
#'   theme_light labs scale_x_datetime theme element_text element_blank
#'   element_rect scale_y_continuous geom_hline ggtitle facet_wrap
#' @importFrom openair quickText
#' @importFrom glue glue
#' @importFrom rlang enquo quo_name quo_is_null !! sym .data
#' @importFrom dplyr pull filter

timeseries_plot <- function(
  data,
  filter_location = NULL,
  filter_analyte = NULL,
  date_col = date,
  concentration_col = concentration,
  location_col = location_code,
  chem_name_col = chem_name,
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
  n_facet_cols = 2,
  facet_by = "analyte"
) {
  # Quote the column name arguments
  date_col_q <- rlang::enquo(date_col)
  conc_col_q <- rlang::enquo(concentration_col)
  location_col_q <- rlang::enquo(location_col)
  chem_name_col_q <- rlang::enquo(chem_name_col)
  criteria_col_q <- rlang::enquo(criteria_col)

  # Convert to strings for validation
  date_name <- rlang::quo_name(date_col_q)
  conc_name <- rlang::quo_name(conc_col_q)
  location_name <- rlang::quo_name(location_col_q)
  analyte_name <- rlang::quo_name(chem_name_col_q)
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

  # Validate facet_by
  facet_by <- match.arg(facet_by, choices = c("analyte", "location"))

  # Validate criteria column if specified. The quosure is tested rather than
  # the argument itself, because `criteria_col = criteria` is a bare column
  # name and evaluating it here would look for it outside the data.
  criteria_given <- !rlang::quo_is_null(criteria_col_q) &&
    criteria_name != "NULL"

  if (criteria_given && !criteria_name %in% names(data)) {
    warning(
      "Criteria column '",
      criteria_name,
      "' not found in data. Skipping criteria line."
    )
    criteria_given <- FALSE
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

    available_analytes <- unique(dplyr::pull(data, !!chem_name_col_q))
    missing_analytes <- setdiff(filter_analyte, available_analytes)

    if (length(missing_analytes) > 0) {
      warning(
        "Requested analytes not found in data: ",
        paste(missing_analytes, collapse = ", ")
      )
    }

    data <- data %>%
      dplyr::filter(!!chem_name_col_q %in% filter_analyte)
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
  analytes_vec <- base::unique(dplyr::pull(data, !!chem_name_col_q))
  n_analytes_filtered <- length(analytes_vec)

  # Determine coloring and faceting based on facet_by
  if (facet_by == "analyte") {
    color_by_location <- n_locations_filtered > 1
    color_by_analyte  <- FALSE
    do_facet_analyte  <- n_analytes_filtered > 1
    do_facet_location <- FALSE
  } else {
    color_by_location <- FALSE
    color_by_analyte  <- n_analytes_filtered > 1
    do_facet_analyte  <- FALSE
    do_facet_location <- n_locations_filtered > 1
  }

  if (color_by_location) {
    if (is.null(location_colours)) {
      location_colours <- gRs_colours(locations_vec)
    } else if (!is.null(names(location_colours))) {
      # A named palette covering some locations but not others leaves the rest
      # unpainted; fill the gaps rather than drop the series. An unnamed vector
      # is matched positionally by ggplot2 and is left alone.
      uncoloured <- setdiff(locations_vec, names(location_colours))
      if (length(uncoloured) > 0) {
        warning(
          "Some locations missing from location_colours: ",
          paste(uncoloured, collapse = ", "),
          ". Generating colours for these locations."
        )
        location_colours <- c(location_colours, gRs_colours(uncoloured))
      }
    }
  }

  analyte_colours <- if (color_by_analyte) gRs_colours(analytes_vec) else NULL

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

  # Set y-axis limits (NULL means use data range). They zoom rather than
  # filter: a scale's default is to drop whatever falls outside its limits,
  # which would take a result above `ymax` - the exceedance the plot is often
  # drawn to show - out of the figure altogether. Kept, it runs off the edge
  # of the panel instead, and the line to it still shows.
  y_limits <- if (is.null(ymax)) {
    c(ymin, NA) # NA allows ggplot2 to calculate upper limit
  } else {
    c(ymin, ymax)
  }

  # Create the base plot
  if (color_by_location) {
    plot <- data %>%
      ggplot2::ggplot(
        ggplot2::aes(x = !!date_col_q, y = !!conc_col_q, colour = !!location_col_q)
      ) +
      ggplot2::geom_point(size = 0.6, alpha = 0.5) +
      ggplot2::geom_path() +
      ggplot2::scale_colour_manual(values = location_colours)
  } else if (color_by_analyte) {
    plot <- data %>%
      ggplot2::ggplot(
        ggplot2::aes(x = !!date_col_q, y = !!conc_col_q, colour = !!chem_name_col_q)
      ) +
      ggplot2::geom_point(size = 0.6, alpha = 0.5) +
      ggplot2::geom_path() +
      ggplot2::scale_colour_manual(values = analyte_colours)
  } else {
    plot <- data %>%
      ggplot2::ggplot(
        ggplot2::aes(x = !!date_col_q, y = !!conc_col_q)
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
      limits = y_limits,
      oob = function(x, range) x
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
      strip.background = ggplot2::element_rect(fill = NA, colour = 'black'),
      strip.text = ggplot2::element_text(colour = "black")
    )

  # Add faceting
  if (do_facet_analyte) {
    plot <- plot +
      ggplot2::facet_wrap(
        rlang::as_label(chem_name_col_q),
        scales = "free_y",
        ncol = n_facet_cols
      )
  } else if (do_facet_location) {
    plot <- plot +
      ggplot2::facet_wrap(
        rlang::as_label(location_col_q),
        scales = "free_y",
        ncol = n_facet_cols
      )
  }

  # Add criteria lines if specified. A guideline belongs to an analyte, so
  # each panel draws the guideline of the analyte in it, and where analytes
  # share a panel each draws its own, in the analyte's colour. One line for
  # the whole plot would put one analyte's guideline on every other's panel.
  if (criteria_given) {
    line_by <- c(
      if (do_facet_analyte || color_by_analyte) analyte_name,
      if (do_facet_location) location_name
    )
    guideline <- criteria_lines(
      data,
      suppressWarnings(as.numeric(dplyr::pull(data, !!criteria_col_q))),
      line_by
    )

    if (!is.null(guideline$warning)) {
      warning(guideline$warning)
    }

    if (nrow(guideline$lines) > 0) {
      plot <- plot +
        if (color_by_analyte) {
          ggplot2::geom_hline(
            data = guideline$lines,
            mapping = ggplot2::aes(
              yintercept = .data$yintercept,
              colour = .data[[analyte_name]]
            ),
            linetype = criteria_linetype,
            linewidth = 0.7,
            show.legend = FALSE
          )
        } else {
          ggplot2::geom_hline(
            data = guideline$lines,
            mapping = ggplot2::aes(yintercept = .data$yintercept),
            linetype = criteria_linetype,
            colour = criteria_colour,
            linewidth = 0.7
          )
        }
    }
  }

  # Add title and subtitle if provided
  if (!is.null(plot_title) || !is.null(plot_subtitle)) {
    plot <- plot +
      ggplot2::ggtitle(label = plot_title, subtitle = plot_subtitle)
  }

  return(plot)
}

#' The guideline lines to draw on a timeseries plot
#'
#' One line per value of `line_by`: the panel, and the analyte where analytes
#' share a panel. Where one line has several values to choose from, the lowest
#' is drawn, not the first. join_action_levels() converts each guideline into
#' the unit its own result was reported in, so one chemical reported in both
#' ug/L and mg/L carries two numbers for a single guideline - 180 and 0.18.
#' Taking the first would take whichever way the rows happened to be sorted,
#' and an arrange() upstream would move the line by three orders of magnitude
#' without the data changing. The lowest is at least the same line every time,
#' and is the one summary_stats() counts exceedances against, so the plot and
#' the table agree.
#'
#' @param data the data being plotted
#' @param values the guideline value of each row of `data`, as numbers
#' @param line_by names of the columns to draw a line per; empty for one line
#' @returns a list of `lines`, a data frame of the `line_by` columns and the
#'   `yintercept` of each line, and `warning`, the text to warn with where a
#'   line had several values to choose from, or `NULL`
#' @noRd
criteria_lines <- function(data, values, line_by) {
  keep <- !is.na(values)
  keys <- as.data.frame(data[keep, line_by, drop = FALSE])
  values <- values[keep]
  units <- if ("output_unit" %in% names(data)) {
    as.character(data$output_unit[keep])
  } else {
    rep(NA_character_, length(values))
  }

  group <- if (length(line_by) == 0) {
    rep("", length(values))
  } else {
    do.call(paste, c(lapply(keys, as.character), sep = " / "))
  }
  first <- !duplicated(group)

  # Split on a factor in first-seen order, so the pieces line up with `first`
  # without being looked up by name - which never finds the one line's "".
  by_line <- split(values, factor(group, levels = group[first]))
  lines <- keys[first, , drop = FALSE]
  lines$yintercept <- vapply(by_line, min, numeric(1), USE.NAMES = FALSE)

  several <- lapply(by_line, function(v) sort(unique(v)))
  ambiguous <- lengths(several) > 1
  if (!any(ambiguous)) {
    return(list(lines = lines, warning = NULL))
  }

  found <- paste0(
    "Multiple criteria values found",
    if (length(line_by) > 0) paste0(" for ", group[first][ambiguous]),
    ": ",
    vapply(several[ambiguous], paste, character(1), collapse = ", "),
    ". Using the lowest: ",
    lines$yintercept[ambiguous],
    "."
  )
  mixed <- unique(stats::na.omit(units[group %in% group[first][ambiguous]]))
  unit_note <- if (length(mixed) > 1) {
    paste0(
      " These results are reported in more than one unit (",
      paste(mixed, collapse = ", "),
      "), so one guideline has become several numbers and the line ",
      "is only right for one of them - and the plotted ",
      "concentrations are mixed units too. Filter to a single unit ",
      "before plotting."
    )
  } else {
    ""
  }

  list(
    lines = lines,
    warning = paste0(paste(found, collapse = "\n"), unit_note)
  )
}

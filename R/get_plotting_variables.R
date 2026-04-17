#' Extract plotting variables from data
#'
#' This function extracts key plotting variables from a dataset and returns them
#' as a list. Unlike the original establish_plotting_variables(), this function
#' does not use global assignment and returns all variables in a structured list.
#'
#' @param data A tibble or data frame containing the data to extract variables from
#' @param location_col Name of the column containing location codes.
#'   Can be provided with or without quotes. Default is location_code
#' @param chem_name_col Name of the column containing chemical/analyte names.
#'   Can be provided with or without quotes. Default is chem_name
#' @param chem_group_col Name of the column containing chemical groups.
#'   Can be provided with or without quotes. Default is chem_group
#' @param date_col Name of the column containing dates.
#'   Can be provided with or without quotes. Default is date
#' @param monitoring_zone_col Name of the column containing monitoring zones.
#'   Can be provided with or without quotes. Default is monitoring_zone
#' @param site_id_col Name of the column containing site IDs (used as fallback
#'   for monitoring zones). Can be provided with or without quotes. Default is site_id
#' @param seed Numeric. Random seed for reproducible color generation. Default is 1239755
#' @param use_rcolorbrewer Logical. Should RColorBrewer be used if available?
#'   Default is TRUE
#'
#' @return A list containing:
#' \describe{
#'   \item{chem_group}{Vector of unique chemical groups}
#'   \item{analytes}{Vector of unique chemical/analyte names}
#'   \item{date_range}{Vector of length 2 with min and max dates}
#'   \item{zones}{Vector of unique monitoring zones}
#'   \item{locations_vec}{Vector of unique location codes}
#'   \item{colours_vec}{Vector of colors (same length as locations_vec)}
#'   \item{location_colours}{Named vector mapping location codes to colors}
#'   \item{n_locations}{Integer count of unique locations}
#'   \item{n_analytes}{Integer count of unique analytes}
#'   \item{n_zones}{Integer count of unique zones}
#' }
#'
#' @export
#'
#' @examples
#' # Extract plotting variables
#' plot_vars <- get_plotting_variables(my_data)
#'
#' # Access individual variables
#' analytes <- plot_vars$analytes
#' location_colours <- plot_vars$location_colours
#' date_range <- plot_vars$date_range
#'
#' # Use with timeseries_plot
#' timeseries_plot(my_data, location_colours = plot_vars$location_colours)
#'
#' # Custom column names
#' plot_vars <- get_plotting_variables(
#'   my_data,
#'   location_col = site_code,
#'   chem_name_col = parameter,
#'   date_col = sample_date
#' )
#'
#' # With custom seed for different colors
#' plot_vars <- get_plotting_variables(my_data, seed = 42)
#'
#' @importFrom dplyr filter select distinct pull mutate
#' @importFrom tidyr drop_na
#' @importFrom rlang enquo quo_name !!

get_plotting_variables <- function(
  data,
  location_col = location_code,
  chem_name_col = chem_name,
  chem_group_col = chem_group,
  date_col = date,
  monitoring_zone_col = monitoring_zone,
  site_id_col = site_id,
  seed = 1239755,
  use_rcolorbrewer = TRUE
) {
  # Quote the column name arguments
  location_col_q <- rlang::enquo(location_col)
  chem_name_col_q <- rlang::enquo(chem_name_col)
  chem_group_col_q <- rlang::enquo(chem_group_col)
  date_col_q <- rlang::enquo(date_col)
  zone_col_q <- rlang::enquo(monitoring_zone_col)
  site_col_q <- rlang::enquo(site_id_col)

  # Convert to strings for validation
  location_name <- rlang::quo_name(location_col_q)
  chem_name_name <- rlang::quo_name(chem_name_col_q)
  chem_group_name <- rlang::quo_name(chem_group_col_q)
  date_name <- rlang::quo_name(date_col_q)
  zone_name <- rlang::quo_name(zone_col_q)
  site_name <- rlang::quo_name(site_col_q)

  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame or tibble")
  }

  required_cols <- c(location_name, chem_name_name, chem_group_name, date_name)
  missing_cols <- required_cols[!required_cols %in% names(data)]

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (nrow(data) == 0) {
    stop("Data contains no rows")
  }

  # Extract unique chemical groups
  chem_group <- base::unique(dplyr::pull(data, !!chem_group_col_q))

  # Extract unique analytes
  analytes <- data %>%
    dplyr::filter(!!chem_group_col_q %in% chem_group) %>%
    dplyr::select(!!chem_name_col_q) %>%
    dplyr::distinct() %>%
    tidyr::drop_na() %>%
    dplyr::pull(!!chem_name_col_q)

  # Extract date range
  date_range <- c(
    base::min(dplyr::pull(data, !!date_col_q), na.rm = TRUE),
    base::max(dplyr::pull(data, !!date_col_q), na.rm = TRUE)
  )

  # Handle monitoring zones
  if (zone_name %in% names(data)) {
    zone_values <- dplyr::pull(data, !!zone_col_q)

    if (all(is.na(zone_values))) {
      # Use site_id as fallback if monitoring_zone is all NA
      if (site_name %in% names(data)) {
        data <- data %>%
          dplyr::mutate(!!zone_col_q := !!site_col_q)
        zones <- base::unique(dplyr::pull(data, !!zone_col_q))
        message("All monitoring_zone values are NA. Using site_id as zones.")
      } else {
        warning(
          "monitoring_zone column is all NA and site_id column not found. ",
          "Zones will be empty."
        )
        zones <- character(0)
      }
    } else {
      zones <- base::unique(zone_values[!is.na(zone_values)])
    }
  } else if (site_name %in% names(data)) {
    # If monitoring_zone doesn't exist, use site_id
    zones <- base::unique(dplyr::pull(data, !!site_col_q))
    message("monitoring_zone column not found. Using site_id as zones.")
  } else {
    warning(
      "Neither monitoring_zone nor site_id columns found. Zones will be empty."
    )
    zones <- character(0)
  }

  # Extract unique locations
  locations_vec <- base::unique(dplyr::pull(data, !!location_col_q))
  n_locations <- length(locations_vec)

  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Generate color palette
  if (
    use_rcolorbrewer &&
      requireNamespace("RColorBrewer", quietly = TRUE) &&
      n_locations > 0
  ) {
    # Use RColorBrewer if available
    if (n_locations <= 8) {
      colours_vec <- RColorBrewer::brewer.pal(
        max(3, n_locations),
        "Set1"
      )[1:n_locations]
    } else {
      # For more locations, create a larger palette
      colours_vec <- grDevices::colorRampPalette(
        RColorBrewer::brewer.pal(8, "Set1")
      )(n_locations)
    }
  } else {
    # Fallback to filtered colors if RColorBrewer not available
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

  # Assign locations to colours
  location_colours <- stats::setNames(colours_vec, locations_vec)

  # Return all variables in a list
  result <- list(
    chem_group = chem_group,
    analytes = analytes,
    date_range = date_range,
    zones = zones,
    locations_vec = locations_vec,
    colours_vec = colours_vec,
    location_colours = location_colours,
    n_locations = n_locations,
    n_analytes = length(analytes),
    n_zones = length(zones)
  )

  return(result)
}

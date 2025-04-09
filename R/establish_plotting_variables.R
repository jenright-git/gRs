#' Establish parameters for automated plots
#'
#' @param data tibble imported from the data_processor function
#'
#' @return lists and vectors for plots
#' @export
#'
#' @examples analaytes date_range zones locations and colours
#' @importFrom dplyr filter select distinct
#' @importFrom tidyr drop_na
#' @importFrom Polychrome alphabet.colors

establish_plotting_variables <- function(data){

  chem_group <<- base::unique(data$chem_group)

  analytes <<- dplyr::filter(data, chem_group %in% chem_group) %>%
    dplyr::select(chem_name) %>%
    dplyr::distinct() %>%
    tidyr::drop_na() %>%
    base::as.vector() %>%
    base::unlist()

  date_range <<- c(base::min(data$date), base::max(data$date))

  if (all(is.na(data$monitoring_zone))) {
    data <- data %>%
      dplyr::mutate(monitoring_zone = site_id)

    zones <<- base::unique(data$monitoring_zone)
  } else {
    zones <<- base::unique(data$monitoring_zone)
  }


  locations_vec <<- base::unique(data$location_code)

  set.seed(1239755)

  colour <-  grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  colours_vec <<- sample(colour,
           size = base::length(locations_vec),
           replace=FALSE)

  # assign locations to colours
  location_colours <<- stats::setNames(colours_vec, locations_vec)

}



# Alternate AI generated to remove global assignment  <<-

#' establish_plotting_variables <- function(data, seed = NULL) {
#'   # Input validation
#'   required_cols <- c("chem_group", "chem_name", "date", "site_id", "location_code")
#'   missing_cols <- required_cols[!required_cols %in% names(data)]
#'
#'   if (length(missing_cols) > 0) {
#'     stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
#'   }
#'
#'   # Extract unique chemical groups
#'   chem_group <- base::unique(data$chem_group)
#'
#'   # Extract unique analytes
#'   analytes <- dplyr::filter(data, chem_group %in% chem_group) %>%
#'     dplyr::select(chem_name) %>%
#'     dplyr::distinct() %>%
#'     tidyr::drop_na() %>%
#'     base::unlist()
#'
#'   # Extract date range
#'   date_range <- c(base::min(data$date, na.rm = TRUE),
#'                   base::max(data$date, na.rm = TRUE))
#'
#'   # Handle monitoring zones
#'   if (all(is.na(data$monitoring_zone))) {
#'     data <- data %>%
#'       dplyr::mutate(monitoring_zone = site_id)
#'     zones <- base::unique(data$monitoring_zone)
#'   } else {
#'     zones <- base::unique(data$monitoring_zone)
#'   }
#'
#'   # Extract unique locations
#'   locations_vec <- base::unique(data$location_code)
#'
#'   # Set seed if provided for reproducibility
#'   if (!is.null(seed)) {
#'     set.seed(seed)
#'   }
#'
#'   # Use a color palette from RColorBrewer if available, otherwise use filtered colors
#'   if (requireNamespace("RColorBrewer", quietly = TRUE)) {
#'     # Use appropriate palette based on number of locations
#'     n_locations <- length(locations_vec)
#'     if (n_locations <= 8) {
#'       colours_vec <- RColorBrewer::brewer.pal(max(3, n_locations), "Set1")[1:n_locations]
#'     } else {
#'       # For more locations, create a larger palette
#'       colours_vec <- grDevices::colorRampPalette(
#'         RColorBrewer::brewer.pal(8, "Set1"))(n_locations)
#'     }
#'   } else {
#'     # Fallback to filtered colors if RColorBrewer not available
#'     colour <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = TRUE)]
#'     colours_vec <- sample(colour, size = base::length(locations_vec), replace = FALSE)
#'   }
#'
#'   # Assign locations to colours
#'   location_colours <- stats::setNames(colours_vec, locations_vec)
#'
#'   # Return all variables in a list
#'   return(list(
#'     chem_group = chem_group,
#'     analytes = analytes,
#'     date_range = date_range,
#'     zones = zones,
#'     locations_vec = locations_vec,
#'     colours_vec = colours_vec,
#'     location_colours = location_colours
#'   ))
#' }
#'
#'
#'
#' # Example usage
# plot_vars <- establish_plotting_variables(my_data, seed = 1239755)
#
# # Access variables from the returned list
# chem_group <- plot_vars$chem_group
# location_colours <- plot_vars$location_colours

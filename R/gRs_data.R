#' Example environmental monitoring dataset
#'
#' A sample dataset for use in examples and testing of gRs package functions.
#'
#' @format A data frame with columns including:
#' \describe{
#'   \item{date}{Sample date (POSIXct)}
#'   \item{location_code}{Monitoring location identifier}
#'   \item{chem_name}{Chemical/analyte name}
#'   \item{chem_group}{Chemical group classification}
#'   \item{concentration}{Measured concentration value}
#'   \item{prefix}{Qualifier prefix (e.g. "<" for less than LOR, "=" for detected)}
#'   \item{output_unit}{Unit of concentration measurement}
#'   \item{site_id}{Site identifier}
#'   \item{monitoring_zone}{Monitoring zone classification}
#' }
#' @source Synthetic example data for package demonstration
"gRs_data"

#' Example environmental monitoring dataset
#'
#' A groundwater chemistry export read through [data_processor()], kept whole
#' rather than trimmed so that examples exercise the same shape of table a
#' real esdat export produces. Most columns are carried through from the
#' export untouched; the ones the package's functions read by name are
#' `date`, `location_code`, `chem_name`, `chem_group`, `concentration`,
#' `prefix`, `detect_flag`, `output_unit`, `site_id` and `monitoring_zone`.
#'
#' @format A data frame with 1,232 rows and 53 columns:
#' \describe{
#'   \item{x_coord}{numeric}
#'   \item{y_coord}{numeric}
#'   \item{latitude}{numeric}
#'   \item{longitude}{numeric}
#'   \item{site_id}{character}
#'   \item{project_id}{character}
#'   \item{monitoring_zone}{character}
#'   \item{sample_code}{character}
#'   \item{field_id}{character}
#'   \item{location_code}{character}
#'   \item{alternative_name}{numeric}
#'   \item{well}{numeric}
#'   \item{monitoring_unit}{numeric}
#'   \item{sample_elevation}{numeric}
#'   \item{sample_depth_avg}{numeric}
#'   \item{sampled_date_time}{POSIXct}
#'   \item{monitoring_round}{character}
#'   \item{sdg}{numeric}
#'   \item{chem_name}{character}
#'   \item{fraction}{character}
#'   \item{prefix}{character}
#'   \item{detect_flag}{character}
#'   \item{concentration}{numeric}
#'   \item{output_unit}{character}
#'   \item{chem_code}{character}
#'   \item{chem_name_abbrev}{character}
#'   \item{chem_group}{character}
#'   \item{method_type}{character}
#'   \item{method_name}{character}
#'   \item{eql}{numeric}
#'   \item{eql_units}{numeric}
#'   \item{lab_comments}{numeric}
#'   \item{lab_report_number}{numeric}
#'   \item{lab_name}{numeric}
#'   \item{location_type}{character}
#'   \item{sample_type}{character}
#'   \item{matrix_type}{character}
#'   \item{matrix_state}{character}
#'   \item{env_stds_conditional_matrix_type}{numeric}
#'   \item{sample_comments}{character}
#'   \item{qualifier}{numeric}
#'   \item{result_comments}{numeric}
#'   \item{data_source}{numeric}
#'   \item{id}{numeric}
#'   \item{sid}{numeric}
#'   \item{pid}{numeric}
#'   \item{metadata_id}{numeric}
#'   \item{elevation}{numeric}
#'   \item{purpose}{character}
#'   \item{description1}{character}
#'   \item{description2}{numeric}
#'   \item{description3}{numeric}
#'   \item{date}{POSIXct}
#' }
#' @source An esdat chemistry export, anonymised for use as example data.
"gRs_data"

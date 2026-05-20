#' Select maximum concentration sample from duplicate/triplicate groups
#'
#' Reduces field duplicate (Field_D) and interlaboratory duplicate (Interlab_D)
#' sample groups to a single representative row per location, date, and analyte.
#' For groups containing duplicate samples, detected results are preferred over
#' non-detects; if all are non-detects, the row with the highest LOR is returned.
#' Groups with only primary samples pass through unchanged.
#'
#' Intended for use after [data_processor()] and before [half_lor()].
#'
#' @param data A tibble with columns for location, date, analyte, concentration,
#'   prefix, and sample type. Typically the output of [data_processor()].
#' @param location_col Name of the column containing location identifiers.
#'   Can be provided with or without quotes. Default is location_code
#' @param date_col Name of the column containing sample dates.
#'   Can be provided with or without quotes. Default is date
#' @param chem_col Name of the column containing analyte names.
#'   Can be provided with or without quotes. Default is chem_name
#' @param concentration_col Name of the column containing concentration values.
#'   Can be provided with or without quotes. Default is concentration
#' @param prefix_col Name of the column containing detect qualifiers
#'   (`"<"` = non-detect). Can be provided with or without quotes. Default is prefix
#' @param sample_type_col Name of the column containing sample type values.
#'   Can be provided with or without quotes. Default is sample_type
#' @param duplicate_types Character vector of sample_type values that identify
#'   duplicate or triplicate samples. Default is `c("Field_D", "Interlab_D")`
#'
#' @return A tibble with the same columns as `data` but with duplicate/triplicate
#'   groups collapsed to one row each. Row order is not guaranteed.
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Build a small example with a primary and a field duplicate
#' test_data <- tibble::tribble(
#'   ~location_code, ~date,       ~chem_name, ~concentration, ~prefix, ~sample_type,
#'   "MW01", as.Date("2024-01-15"), "Benzene",  5.2,  "=", "Normal",
#'   "MW01", as.Date("2024-01-15"), "Benzene",  6.1,  "=", "Field_D",
#'   "MW02", as.Date("2024-01-15"), "Benzene",  1.0,  "<", "Normal"
#' )
#'
#' # Default usage — Field_D row returned for MW01 (higher concentration)
#' select_max_concentration(test_data)
#'
#' # Custom duplicate_types
#' select_max_concentration(test_data, duplicate_types = c("Field_D", "Interlab_D", "Field_T"))
#'
#' # Custom column names (with or without quotes)
#' select_max_concentration(test_data,
#'                          location_col = location_code,
#'                          concentration_col = concentration)
#'
#' # Typical workflow
#' \dontrun{
#' gRs_data <- data_processor("my_file.xlsx") |>
#'   select_max_concentration() |>
#'   half_lor(multiplier = 0.5)
#' }
#'
#' @importFrom dplyr group_by mutate filter slice_max select ungroup any_of
#' @importFrom rlang enquo !!
select_max_concentration <- function(
  data,
  location_col = location_code,
  date_col = date,
  chem_col = chem_name,
  concentration_col = concentration,
  prefix_col = prefix,
  sample_type_col = sample_type,
  duplicate_types = c("Field_D", "Interlab_D")
) {
  location_col <- rlang::enquo(location_col)
  date_col <- rlang::enquo(date_col)
  chem_col <- rlang::enquo(chem_col)
  conc_col <- rlang::enquo(concentration_col)
  prefix_col <- rlang::enquo(prefix_col)
  sample_type_col <- rlang::enquo(sample_type_col)

  data %>%
    dplyr::group_by(!!location_col, !!date_col, !!chem_col) %>%
    dplyr::mutate(
      .has_duplicate = any(!!sample_type_col %in% duplicate_types),
      .is_detect = !!prefix_col != "<" | is.na(!!prefix_col)
    ) %>%
    dplyr::filter(
      !.has_duplicate | .is_detect | !any(.is_detect)
    ) %>%
    dplyr::slice_max(!!conc_col, n = 1, with_ties = FALSE) %>%
    dplyr::select(-dplyr::any_of(c(".has_duplicate", ".is_detect"))) %>%
    dplyr::ungroup()
}

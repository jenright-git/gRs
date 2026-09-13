#' Select maximum concentration sample from duplicate/triplicate groups
#'
#' Reduces each location/date/analyte group to a single representative row.
#' Where the group holds a field duplicate (`Field_D`) or an interlaboratory
#' duplicate (`Interlab_D`), detected results are preferred over non-detects,
#' and the highest of them is kept; if every result is a non-detect, the row
#' with the highest LOR is returned.
#'
#' A group holding a single result is returned as it stands. A group holding
#' several results none of which is flagged as a duplicate is a data quality
#' problem rather than a duplicate pair - the highest concentration is still
#' returned, so the output is one row per location, date and analyte either
#' way.
#'
#' Intended for use after [data_processor()] and before [half_lor()].
#'
#' @param data A tibble with columns for location, date, analyte, concentration,
#'   prefix, and sample type. Typically the output of [data_processor()].
#' @param location_col Name of the column containing location identifiers.
#'   Can be provided with or without quotes. Default is location_code
#' @param date_col Name of the column containing sample dates.
#'   Can be provided with or without quotes. Default is date
#' @param chem_name_col Name of the column containing analyte names.
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
#' # A primary sample and its field duplicate, plus an unduplicated location
#' samples <- tibble::tribble(
#'   ~location_code, ~date, ~chem_name, ~concentration, ~prefix, ~sample_type,
#'   "MW01", as.Date("2024-01-15"), "Benzene", 5.2, "=", "Normal",
#'   "MW01", as.Date("2024-01-15"), "Benzene", 6.1, "=", "Field_D",
#'   "MW02", as.Date("2024-01-15"), "Benzene", 1.0, "<", "Normal"
#' )
#'
#' # The Field_D row wins for MW01; MW02 passes through
#' select_max_concentration(samples)
#'
#' # Triplicates too
#' select_max_concentration(
#'   samples,
#'   duplicate_types = c("Field_D", "Interlab_D", "Field_T")
#' )
#'
#' # Typical workflow
#' \dontrun{
#' data_processor("my_file.xlsx") %>%
#'   select_max_concentration() %>%
#'   half_lor(lor_multiplier = 0.5)
#' }
#'
#' @importFrom dplyr group_by mutate filter slice_max select ungroup any_of
#' @importFrom tibble tribble
#' @importFrom rlang enquo !!
select_max_concentration <- function(
  data,
  location_col = location_code,
  date_col = date,
  chem_name_col = chem_name,
  concentration_col = concentration,
  prefix_col = prefix,
  sample_type_col = sample_type,
  duplicate_types = c("Field_D", "Interlab_D")
) {
  location_col <- rlang::enquo(location_col)
  date_col <- rlang::enquo(date_col)
  chem_name_col <- rlang::enquo(chem_name_col)
  conc_col <- rlang::enquo(concentration_col)
  prefix_col <- rlang::enquo(prefix_col)
  sample_type_col <- rlang::enquo(sample_type_col)

  # sample_type is optional in a chemistry export, so a table without it
  # reaches here and would otherwise fail inside a mutate() with nothing but
  # "object 'sample_type' not found" to go on. With no sample types recorded
  # there are no duplicates to collapse, and the table is already the answer.
  type_name <- rlang::quo_name(sample_type_col)
  if (!type_name %in% names(data)) {
    warning(
      "No '",
      type_name,
      "' column, so no duplicate samples can be identified and `data` is ",
      "returned unchanged. Name the column holding sample types with ",
      "`sample_type_col`."
    )
    return(data)
  }

  data %>%
    dplyr::group_by(!!location_col, !!date_col, !!chem_name_col) %>%
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

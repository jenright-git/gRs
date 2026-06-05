#' data_processor
#'
#' @param myfile_path file path to data
#' @param sheet_pattern matching the excel sheet name for new or old esdat formats
#'
#' @returns uploaded file from esdat
#' @export
#' @examples
#' \dontrun{
#' data_processor('my_file_path')
#' }
#' @importFrom dplyr bind_rows filter mutate case_when rename
#' @importFrom readxl excel_sheets read_excel
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate rename arrange %>%
#' @importFrom lubridate floor_date
#' @importFrom glue glue
data_processor <- function(myfile_path, sheet_pattern = "Chem") {
  if (!file.exists(myfile_path)) {
    stop("File does not exist: ", myfile_path)
  }

  all_sheets <- readxl::excel_sheets(myfile_path)
  matching_sheets <- base::grep(sheet_pattern, all_sheets, value = TRUE)

  if (length(matching_sheets) == 0) {
    ar2_sheet <- NULL
    for (s in all_sheets) {
      peek <- tryCatch(
        readxl::read_excel(myfile_path, sheet = s, n_max = 0),
        error = function(e) NULL
      )
      if (!is.null(peek) && all(AR2_SIGNATURE_COLS %in% names(peek))) {
        ar2_sheet <- s
        break
      }
    }
    if (is.null(ar2_sheet)) {
      warning("No sheets matching pattern '", sheet_pattern, "' found")
      return(NULL)
    }
    sheet_name <- ar2_sheet
    skip_rows <- 0
  } else if ("LChem1_Chemistry" %in% matching_sheets) {
    sheet_name <- "LChem1_Chemistry"
    skip_rows <- 0
  } else if ("davLChem1_Chemistry" %in% matching_sheets) {
    sheet_name <- "davLChem1_Chemistry"
    skip_rows <- 1
  } else if ("Chemistry List" %in% matching_sheets) {
    sheet_name <- "Chemistry List"
    skip_rows <- 0
  } else {
    warning(
      "Found matching sheets but none of the expected types: ",
      paste(matching_sheets, collapse = ", ")
    )
    return(NULL)
  }

  raw_sw_data <- readxl::read_excel(
    myfile_path,
    sheet = sheet_name,
    skip = skip_rows
  )

  sw_data <- raw_sw_data %>%
    janitor::clean_names() %>%
    resolve_columns(COLUMN_ALIASES)

  if ("fraction" %in% names(sw_data) && is.logical(sw_data$fraction)) {
    sw_data <- sw_data %>%
      dplyr::mutate(fraction = ifelse(fraction, "F", "T"))
  }

  if (!"prefix" %in% names(sw_data) && "detect_flag" %in% names(sw_data)) {
    sw_data <- sw_data %>%
      dplyr::mutate(prefix = ifelse(detect_flag == "Y", "=", "<"))
  }

  if (!"detect_flag" %in% names(sw_data) && "prefix" %in% names(sw_data)) {
    sw_data <- sw_data %>%
      dplyr::mutate(detect_flag = ifelse(is.na(prefix), "Y", "N"))
  }

  missing <- setdiff(REQUIRED_COLUMNS, names(sw_data))
  if (length(missing) > 0) {
    stop(
      "Missing required columns after normalization: ",
      paste(missing, collapse = ", "),
      "\nColumns found: ",
      paste(names(sw_data), collapse = ", ")
    )
  }

  if (!"chem_group" %in% names(sw_data)) {
    warning(
      "Column 'chem_group' not found. ",
      "plot_by_analyte(), summary_stats(), establish_plotting_variables(), ",
      "and get_plotting_variables() require this column. ",
      "Add it manually after data_processor() returns."
    )
  }

  sw_data <- sw_data %>%
    dplyr::mutate(date = lubridate::floor_date(sampled_date_time, "day"))

  if (
    "fraction" %in%
      names(sw_data) &&
      any(!is.na(sw_data$fraction))
  ) {
    sw_data <- sw_data %>%
      dplyr::mutate(
        chem_name = ifelse(
          fraction == "F",
          yes = glue::glue("Dissolved {chem_name}"),
          no = chem_name
        )
      )
  }

  sw_data %>% dplyr::arrange(date)
}

AR2_SIGNATURE_COLS <- c("DETECT_FLAG", "REPORT_RESULT_VALUE", "SYS_LOC_CODE")

# Maps canonical column names to known aliases from different export formats.
# Add new aliases here when a new export format is encountered.
# Within each vector, earlier entries take priority over later ones.
COLUMN_ALIASES <- list(
  sampled_date_time = c(
    "sample_date_time",
    "date_time",
    "datetime",
    "sample_date",
    "collected_date_time",
    "sampled_date"
  ),
  concentration = c(
    "report_result_value",
    "reported_value",
    "result_numeric",
    "result"
  ),
  output_unit = c(
    "report_result_unit",
    "reported_unit",
    "result_unit",
    "unit",
    "units"
  ),
  site_id = c("site", "site_code", "facility_code"),
  location_code = c(
    "loc_code",
    "location",
    "monitoring_location",
    "sys_loc_code",
    "location_id"
  ),
  prefix = c("qualifier", "result_qualifier", "result_prefix"),
  chem_name = c(
    "chemical_name",
    "analyte",
    "analyte_name",
    "chemical"
  ),
  chem_group = c(
    "chemical_group",
    "analyte_group",
    "param_group",
    "mth_anl_group_member",
    "method_analyte_group_member",
    "method_analyte_group"
  ),
  fraction = c("total_or_filtered", "filtered", "sample_fraction"),
  sample_type = c("samp_type", "field_sample_type", "sample_type_code", "type"),
  detect_flag = c("detect")
)

REQUIRED_COLUMNS <- c(
  "sampled_date_time",
  "concentration",
  "output_unit",
  "site_id",
  "location_code",
  "prefix",
  "chem_name",
  "detect_flag"
)

#' Rename columns to canonical names based on an alias map
#'
#' @param df data frame to normalise
#' @param alias_map named list: canonical_name -> character vector of known aliases
#' @returns df with columns renamed to canonical names where a match is found
resolve_columns <- function(df, alias_map) {
  current_names <- names(df)
  for (canonical in names(alias_map)) {
    if (canonical %in% current_names) {
      next
    }
    matched <- intersect(alias_map[[canonical]], current_names)
    if (length(matched) > 1) {
      warning(
        "Multiple columns match canonical '",
        canonical,
        "': ",
        paste(matched, collapse = ", "),
        ". Using '",
        matched[[1]],
        "'."
      )
    }
    if (length(matched) >= 1) {
      df <- dplyr::rename(df, !!canonical := !!matched[[1]])
    }
  }
  df
}

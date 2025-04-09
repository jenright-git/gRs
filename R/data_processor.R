#' data_processor
#'
#' @param myfile_path
#' @param sheet_pattern
#'
#' @returns uploaded file from esdat
#' @export
#'
#' @examples data_processor('my_file_path')
#' @importFrom dplyr bind_rows filter mutate case_when
#' @importFrom base grep file.exists
#' @importFrom readxl excel_sheets read_excel
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate rename arrange %>%
#' @importFrom lubridate floor_date
#' @importFrom glue glue
data_processor <- function(myfile_path, sheet_pattern = "Chem") {
  # Input validation
  if (!file.exists(myfile_path)) {
    stop("File does not exist: ", myfile_path)
  }

  # Get sheet names
  all_sheets <- readxl::excel_sheets(myfile_path)

  # Find sheets containing the pattern
  matching_sheets <- base::grep(sheet_pattern, all_sheets, value = TRUE)

  if (length(matching_sheets) == 0) {
    warning("No sheets matching pattern '", sheet_pattern, "' found")
    return(NULL)
  }

  # Process based on sheet name
  if ("LChem1_Chemistry" %in% matching_sheets) {
    sheet_name <- "LChem1_Chemistry"
    skip_rows <- 0
    rename_cols <- FALSE
  } else if ("davLChem1_Chemistry" %in% matching_sheets) {
    sheet_name <- "davLChem1_Chemistry"
    skip_rows <- 1
    rename_cols <- TRUE
  } else {
    warning("Found matching sheets but none of the expected types: ", paste(matching_sheets, collapse = ", "))
    return(NULL)
  }

  # Read the data
  raw_sw_data <- readxl::read_excel(
    myfile_path,
    sheet = sheet_name,
    skip = skip_rows
  )

  # Process the data
  sw_data <- raw_sw_data %>%
    janitor::clean_names() %>%
    dplyr::mutate(date = lubridate::floor_date(sampled_date_time, "day"))

  # Rename columns if needed
  if (rename_cols) {
    sw_data <- sw_data %>%
      dplyr::rename(output_unit = result_unit,
                    concentration = result,
                    site_id = site)
  }

  # Process chemical names
if("total_or_filtered" %in% names(sw_data) & any(!is.na(sw_data$total_or_filtered))){
    sw_data <- sw_data %>%
    dplyr::mutate(chem_name = ifelse(total_or_filtered == "F",
                                     yes = glue::glue("Dissolved {chem_name}"),
                                     no = chem_name)) %>%
    dplyr::arrange(date)
}
  return(sw_data)
}

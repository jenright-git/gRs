#' Data Processor
#'
#' @param myfile_path path to excel file
#'
#' @return tible of processed data
#' @export
#'
#' @examples data_processor("sw_data.xlsx")
#' @importFrom readxl read_excel
#' @importFrom dplyr select mutate left_join arrange
#' @importFrom readr read_csv
data_processor <- function(myfile_path){


  raw_sw_data <- readxl::read_excel(myfile_path,
                                    sheet = "LChem1_Chemistry", col_types = c("numeric",
                                                                              "numeric", "numeric", "numeric",
                                                                              "text", "text", "text", "text",
                                                                              "text", "text", "numeric", "numeric",
                                                                              "numeric", "numeric", "numeric",
                                                                              "date", "text", "numeric", "text",
                                                                              "text", "text", "numeric", "text",
                                                                              "text", "text", "text", "text", "text",
                                                                              "numeric", "numeric", "numeric",
                                                                              "numeric", "numeric", "text", "text",
                                                                              "text", "text", "numeric", "text",
                                                                              "numeric", "numeric", "numeric",
                                                                              "numeric", "numeric", "numeric",
                                                                              "numeric", "numeric", "text",
                                                                              "text", "numeric", "numeric")
  )

  sw_data <- dplyr::select(raw_sw_data,
                           lat = Latitude,
                           lon = Longitude,
                           site = Site_ID,
                           location = Location_Code,
                           field = Field_ID,
                           sample_date = Sampled_Date_Time,
                           monitoring.round = Monitoring_Round,
                           zone = Monitoring_Zone,
                           analyte = ChemName,
                           filtered = Total_or_Filtered,
                           prefix = Prefix,
                           concentration = Concentration,
                           group = Chem_Group,
                           units = `Output Unit`,
                           location_type = Location_Type,
                           SampleComments,
                           sample_type = Sample_Type,
                           purpose = Purpose
  ) %>%
    dplyr::mutate(date = lubridate::floor_date(sample_date, "day"))

  criteria_file <- file.path("raw-data", "criteria.csv")

  if (file.exists(criteria_file)) {
    criteria <- readr::read_csv(criteria_file) %>%
      dplyr::select(source = ActionLevelSource,
                    analyte = ChemName,
                    criteria = Action_Level)

    sw_data <- dplyr::left_join(sw_data, criteria, by = "analyte") %>%
      dplyr::select(-source)
  }


  sw_data <- dplyr::mutate(sw_data,
                           analyte = ifelse(filtered == "F", yes = glue::glue("Dissolved {analyte}"), no = analyte)) %>%
    dplyr::arrange(date)

  return(sw_data)



}

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
#' @importFrom janitor clean_names
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

  sw_data <-
    #   dplyr::select(raw_sw_data,
    #                          lat = Latitude,
    #                          lon = Longitude,
    #                          site = Site_ID,
    #                          location = Location_Code,
    #                          field = Field_ID,
    #                          sample_date = Sampled_Date_Time,
    #                          monitoring_round = Monitoring_Round,
    #                          zone = Monitoring_Zone,
    #                          analyte = ChemName,
    #                          filtered = Total_or_Filtered,
  #                          prefix = Prefix,
  #                          concentration = Concentration,
  #                          group = Chem_Group,
  #                          units = `Output Unit`,
  #                          location_type = Location_Type,
  #                          SampleComments,
  #                          sample_type = Sample_Type,
  #                          purpose = Purpose
  # ) %>%
  raw_sw_data %>%
    janitor::clean_names() %>%
    dplyr::mutate(date = lubridate::floor_date(sampled_date_time, "day"))

  sw_data <-
    dplyr::mutate(sw_data,
                  analyte = ifelse(total_or_filtered == "F",
                                   yes = glue::glue("Dissolved {chem_name}"),
                                   no = chem_name)) %>%
    dplyr::arrange(date)

  return(sw_data)



}

#' Create an interactive data table using the package DT
#'
#' @param data a tibble or dataframe
#'
#' @return An interactive datatable using the DT package
#' @export
#'
#' @examples create_dt(data)
#' @importFrom DT datatable

create_dt <- function(data){
  data %>%
    DT::datatable(rownames = FALSE,
                  extensions = 'Buttons',
                  filter = list(position = "top"),
                  options = list(dom = 'Blfrtrip',
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                 lengthMenu = list(c(10,25,50,-1),
                                                   c(10,25,50,"All")),
                                 language = list(search = "Keyword look-up:"),
                                 digits = 1
                  ))
}

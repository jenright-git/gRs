#' Build a zero row tibble with names
#'
#' @param names vector of names for the tibble
#' @param base  should a standard data.frame be produced rather than a tibble?
#'
#' @return Tibble with column names and zero rows
#' @export
#'
#' @examples zero_row_tibble(c("A", "B", "C"))
zero_row_tibble <- function(names, base = FALSE) {

  # Create empty data frame with correct number of variables
  df <- data.frame(
    matrix(0, ncol = length(names), nrow = 0)
  ) %>%
    purrr::set_names(names)

  # To tibble
  if (!base) {
    df <- as_tibble(df)
  }

  return(df)

}

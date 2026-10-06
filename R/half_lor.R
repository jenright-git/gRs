#' Apply a multiplier to concentrations reported below the LOR
#'
#' A non-detect is reported at its limit of reporting, which overstates a
#' result that was not seen at all. This substitutes a fraction of the LOR -
#' conventionally half - for those results, leaving detected results alone.
#' Non-detects are the results whose `detect_flag` is anything but `"Y"`, as
#' [data_processor()] writes it.
#'
#' @param data A tibble with a `detect_flag` column and a concentration
#'   column. Typically the output of [data_processor()].
#' @param lor_multiplier Numeric. What to multiply non-detect concentrations
#'   by. Default 1 (leave the LOR as reported). Common values: 0 (zero
#'   substitution), 0.5 (half LOR), 1 (full LOR).
#' @param concentration_col Name of the column holding concentration values.
#'   Can be given with or without quotes. Default `concentration`.
#'
#' @returns `data` with substituted concentrations and a new
#'   `lor_multiplier_applied` column - the multiplier for non-detects, `NA`
#'   for detected results. `detect_flag` and `prefix` are left as they were,
#'   so a substituted result is still identifiable as a non-detect.
#' @export
#'
#' @seealso [mann_kendall_test()], which takes `lor_multiplier` directly and
#'   so does not need this called first.
#'
#' @examples
#' # Half LOR: a result reported as <4 becomes 2
#' half_lor(gRs_data, lor_multiplier = 0.5)
#'
#' # Zero substitution: <4 becomes 0
#' half_lor(gRs_data, lor_multiplier = 0)
#'
#' # Custom concentration column, with or without quotes
#' \dontrun{
#' half_lor(my_data, 0.5, concentration_col = result)
#' half_lor(my_data, 0.5, concentration_col = "result")
#' }
#' @importFrom dplyr mutate
#' @importFrom rlang enquo !! :=
half_lor <- function(
  data,
  lor_multiplier = 1,
  concentration_col = concentration
) {
  conc_col <- rlang::enquo(concentration_col)
  check_detect_flag(data, "half_lor")

  # The flag is read inside mutate(), so a grouped table is read group by
  # group rather than handed a vector the length of the whole table.
  data %>%
    dplyr::mutate(
      lor_multiplier_applied = ifelse(
        is_detect(detect_flag),
        NA_real_,
        lor_multiplier
      ),
      !!conc_col := ifelse(
        is_detect(detect_flag),
        !!conc_col,
        !!conc_col * lor_multiplier
      )
    )
}

#' Apply a multiplier to concentrations reported below the LOR
#'
#' A non-detect is reported at its limit of reporting, which overstates a
#' result that was not seen at all. This substitutes a fraction of the LOR -
#' conventionally half - for those results, leaving detected results alone.
#'
#' @param data A tibble with a prefix column and a concentration column.
#'   Typically the output of [data_processor()].
#' @param lor_multiplier Numeric. What to multiply non-detect concentrations
#'   by. Default 1 (leave the LOR as reported). Common values: 0 (zero
#'   substitution), 0.5 (half LOR), 1 (full LOR).
#' @param prefix_col Name of the column holding the detect qualifier
#'   (`"<"` = non-detect). Can be given with or without quotes. Default
#'   `prefix`.
#' @param concentration_col Name of the column holding concentration values.
#'   Can be given with or without quotes. Default `concentration`.
#'
#' @returns `data` with substituted concentrations and a new
#'   `lor_multiplier_applied` column - the multiplier for non-detects, `NA`
#'   for detected results. The prefix column is left as it was, so a
#'   substituted result is still identifiable as a non-detect.
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
#' # Custom column names, with or without quotes
#' \dontrun{
#' half_lor(my_data, 0.5, prefix_col = qualifier, concentration_col = result)
#' half_lor(my_data, 0.5, prefix_col = "qualifier")
#' }
#' @importFrom dplyr mutate
#' @importFrom tidyr replace_na
#' @importFrom rlang enquo !! :=
half_lor <- function(
  data,
  lor_multiplier = 1,
  prefix_col = prefix,
  concentration_col = concentration
) {
  prefix_col <- rlang::enquo(prefix_col)
  conc_col <- rlang::enquo(concentration_col)

  data %>%
    dplyr::mutate(
      !!prefix_col := tidyr::replace_na(as.character(!!prefix_col), "="),
      lor_multiplier_applied = ifelse(
        !!prefix_col == "<",
        lor_multiplier,
        NA_real_
      ),
      !!conc_col := ifelse(
        !!prefix_col == "<",
        !!conc_col * lor_multiplier,
        !!conc_col
      )
    )
}

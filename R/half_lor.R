#' Apply multiplier to concentrations that are < LOR
#'
#' @param data A tibble with columns for prefix and concentration
#' @param multiplier Numeric value to multiply LOR concentrations by. Default is 1 (no change).
#'   Common values: 0 (zero substitution), 0.5 (half LOR), 1 (full LOR value)
#' @param prefix_col Name of the column containing prefix indicators (e.g., "<", "=").
#'   Can be provided with or without quotes. Default is prefix
#' @param concentration_col Name of the column containing concentration values.
#'   Can be provided with or without quotes. Default is concentration
#'
#' @return A tibble with modified concentrations and a new `lor_multiplier_applied`
#'   column (the multiplier value for non-detects, NA for detected results).
#'   The prefix column is preserved unchanged to retain non-detect context.
#' @export
#'
#' @examples
#' # Using default column names (no quotes needed)
#' # Half LOR: <4 becomes =2
#' half_lor(data, multiplier = 0.5)
#'
#' # Zero substitution: <4 becomes =0
#' half_lor(data, multiplier = 0)
#'
#' # No change (default): <4 remains =4
#' half_lor(data, multiplier = 1)
#'
#' # Using custom column names WITHOUT quotes
#' half_lor(data, multiplier = 0.5,
#'          prefix_col = qualifier,
#'          concentration_col = result_value)
#'
#' # Using custom column names WITH quotes also works
#' half_lor(data, multiplier = 0.5,
#'          prefix_col = "qualifier",
#'          concentration_col = "result_value")
#'
#' @importFrom dplyr mutate
#' @importFrom tidyr replace_na
#' @importFrom rlang enquo !! :=
half_lor <- function(
  data,
  multiplier = 1,
  prefix_col = prefix,
  concentration_col = concentration
) {
  # Quote the column name arguments
  prefix_col <- rlang::enquo(prefix_col)
  conc_col <- rlang::enquo(concentration_col)

  modified_data <- data %>%
    dplyr::mutate(
      !!prefix_col := tidyr::replace_na(as.character(!!prefix_col), "="),
      lor_multiplier_applied = ifelse(!!prefix_col == "<", multiplier, NA_real_),
      !!conc_col := ifelse(
        !!prefix_col == "<",
        !!conc_col * multiplier,
        !!conc_col
      )
    )

  return(modified_data)
}

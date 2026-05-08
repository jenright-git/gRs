#' Mann Kendall Function to loop through entire dataset
#'
#' @param data tibble processed with the data_processor function
#' @param traditional perform the standard analysis and not analyse for "Probably" or "Stable" trends
#' @param lor_multiplier Numeric value to multiply LOR concentrations by. Default is 1 (no change).
#'   Common values: 0 (zero substitution), 0.5 (half LOR), 1 (full LOR value).
#'   Set to NULL to use concentrations as-is without adjustment.
#' @param nd_threshold Optional numeric (0–1). Location-analyte combinations where the proportion of
#'   non-detects (prefix == "<") exceeds this value are excluded before analysis. Excluded
#'   combinations are reported to the console. Default is NULL (no filtering).
#' @param location_col Name of the column containing location codes.
#'   Can be provided with or without quotes. Default is location_code
#' @param analyte_col Name of the column containing chemical/analyte names.
#'   Can be provided with or without quotes. Default is chem_name
#' @param concentration_col Name of the column containing concentration values.
#'   Can be provided with or without quotes. Default is concentration
#' @param date_col Name of the column containing sample dates.
#'   Can be provided with or without quotes. Default is date
#' @param prefix_col Name of the column containing prefix indicators (e.g., "<", "=").
#'   Can be provided with or without quotes. Default is prefix
#'
#' @return A nested tibble of trends as well as the original nested data
#' @export
#'
#' @examples
#' # Default: no LOR adjustment, default column names
#' mann_kendall_test(data)
#'
#' # Half LOR method with default column names
#' mann_kendall_test(data, lor_multiplier = 0.5)
#'
#' # Zero substitution
#' mann_kendall_test(data, lor_multiplier = 0)
#'
#' # Traditional trend categories with half LOR
#' mann_kendall_test(data, traditional = TRUE, lor_multiplier = 0.5)
#'
#' # Custom column names WITHOUT quotes
#' mann_kendall_test(data,
#'                   location_col = site_code,
#'                   analyte_col = parameter,
#'                   concentration_col = result_value,
#'                   date_col = sample_date,
#'                   prefix_col = qualifier)
#'
#' # Custom column names WITH quotes also works
#' mann_kendall_test(data,
#'                   location_col = "site_code",
#'                   analyte_col = "parameter")
#'
#' @importFrom dplyr bind_rows filter mutate case_when select arrange
#' @importFrom tidyr tibble nest unnest drop_na
#' @importFrom purrr map map_dbl
#' @importFrom rlang enquo quo_name !! :=
mann_kendall_test <- function(
  data,
  traditional = FALSE,
  lor_multiplier = 1,
  nd_threshold = NULL,
  location_col = location_code,
  analyte_col = chem_name,
  concentration_col = concentration,
  date_col = date,
  prefix_col = prefix
) {
  # Quote the column name arguments
  location_col <- rlang::enquo(location_col)
  analyte_col <- rlang::enquo(analyte_col)
  conc_col <- rlang::enquo(concentration_col)
  date_col <- rlang::enquo(date_col)
  prefix_col <- rlang::enquo(prefix_col)

  # Convert to strings for passing to mk_analysis
  conc_name <- rlang::quo_name(conc_col)
  date_name <- rlang::quo_name(date_col)
  prefix_name <- rlang::quo_name(prefix_col)

  df <- data %>%
    tidyr::drop_na(!!conc_col) %>%
    tidyr::nest(.by = c(!!location_col, !!analyte_col)) %>%
    dplyr::mutate(n_samples = purrr::map(data, nrow)) %>%
    tidyr::unnest(n_samples) %>%
    dplyr::filter(n_samples > 3) %>% # filter out entries with less than 4 data points
    dplyr::select(-n_samples)

  # Make sure there is at least one set of results to be analysed
  base::stopifnot("No data with more than 3 samples" = nrow(df) > 0)

  if (!is.null(nd_threshold)) {
    df <- df %>%
      dplyr::mutate(
        nd_pct = purrr::map_dbl(data, function(d) {
          nd <- !is.na(d[[prefix_name]]) & d[[prefix_name]] == "<"
          mean(nd)
        })
      )

    excluded <- df %>% dplyr::filter(nd_pct > nd_threshold)

    if (nrow(excluded) > 0) {
      loc_name <- rlang::quo_name(location_col)
      ana_name <- rlang::quo_name(analyte_col)
      excl_lines <- paste(
        sprintf(
          "  - %s / %s (%.1f%% non-detects)",
          excluded[[loc_name]],
          excluded[[ana_name]],
          excluded$nd_pct * 100
        ),
        collapse = "\n"
      )
      message(sprintf(
        "Excluded %d location-analyte combination(s) with >%.0f%% non-detects:\n%s",
        nrow(excluded),
        nd_threshold * 100,
        excl_lines
      ))
    }

    df <- df %>%
      dplyr::filter(nd_pct <= nd_threshold) %>%
      dplyr::select(-nd_pct)

    base::stopifnot(
      "No data remaining after non-detect threshold filter" = nrow(df) > 0
    )
  }

  df <- df %>%
    dplyr::mutate(
      results = purrr::map(
        data,
        ~ mk_analysis(
          .x,
          lor_multiplier = lor_multiplier,
          concentration_col = conc_name,
          date_col = date_name,
          prefix_col = prefix_name
        )
      )
    ) %>%
    tidyr::unnest(results)

  if (traditional) {
    nested_df <- df %>%
      dplyr::mutate(
        trend = dplyr::case_when(
          p_value < 0.05 & tau_statistic > 0 ~ "Increasing",
          p_value < 0.05 & tau_statistic < 0 ~ "Decreasing",
          p_value > 0.05 ~ "No Significant Trend",
          TRUE ~ "No Significant Trend"
        )
      )
  } else {
    nested_df <- df %>%
      dplyr::mutate(
        trend = dplyr::case_when(
          p_value < 0.05 & tau_statistic > 0 ~ "Increasing",
          p_value >= 0.05 &
            p_value <= 0.1 &
            tau_statistic > 0 ~ "Probably Increasing",
          p_value > 0.1 & tau_statistic > 0 ~ "No Significant Trend",
          p_value > 0.1 &
            tau_statistic <= 0 &
            COV >= 1 ~ "No Significant Trend",
          p_value > 0.1 & tau_statistic <= 0 & COV < 1 ~ "Stable",
          p_value >= 0.05 &
            p_value <= 0.1 &
            tau_statistic < 0 ~ "Probably Decreasing",
          p_value < 0.05 & tau_statistic < 0 ~ "Decreasing",
          is.na(p_value) & SD == 0 & COV == 0 ~ "Stable"
        )
      ) # if all <LOR results then p_value comes back as NA..
  }

  return(nested_df)
}


#' Mann_Kendall Test returning test result and stats
#'
#' @param data filtered tibble with column of "concentration"
#' @param lor_multiplier Numeric value to multiply LOR concentrations by. Default is 1 (no change).
#'   Common values: 0 (zero substitution), 0.5 (half LOR), 1 (full LOR value).
#'   Set to NULL to use concentrations as-is without adjustment.
#' @param concentration_col Name of the column containing concentration values (as character string)
#' @param date_col Name of the column containing sample dates (as character string)
#' @param prefix_col Name of the column containing prefix indicators (as character string)
#'
#' @return tibble with result and stats
#' @export
#'
#' @examples
#' # Typically called from mann_kendall_test(), not directly by users
#' # Default: no LOR adjustment
#' mk_analysis(df)
#'
#' # Half LOR method
#' mk_analysis(df, lor_multiplier = 0.5)
#'
#' # Zero substitution
#' mk_analysis(df, lor_multiplier = 0)
#'
#' @importFrom trend mk.test
#' @importFrom dplyr arrange mutate
#' @importFrom tidyr drop_na tibble
#' @importFrom rlang sym !! :=

mk_analysis <- function(
  data,
  lor_multiplier = 1,
  concentration_col = "concentration",
  date_col = "date",
  prefix_col = "prefix"
) {
  # Convert strings to symbols for tidy evaluation
  date_sym <- rlang::sym(date_col)
  conc_sym <- rlang::sym(concentration_col)
  prefix_sym <- rlang::sym(prefix_col)

  data <- data %>% dplyr::arrange(!!date_sym)

  # Apply LOR multiplier if specified
  if (!is.null(lor_multiplier)) {
    data <- data %>%
      dplyr::mutate(
        !!prefix_sym := tidyr::replace_na(as.character(!!prefix_sym), "="),
        !!conc_sym := ifelse(
          !!prefix_sym == "<",
          !!conc_sym * lor_multiplier,
          !!conc_sym
        ),
        !!prefix_sym := "="
      )
  }

  # Drop NA and perform Mann-Kendall test
  data <- data %>% tidyr::drop_na(!!conc_sym)

  # Extract concentration values for mk.test
  conc_values <- data[[concentration_col]]
  result <- trend::mk.test(conc_values)

  mk_result <- tidyr::tibble(
    p_value = result$p.value,
    tau_statistic = result$estimates[3],
    S_statistic = result$estimates[1],
    sample_mean = base::mean(conc_values, na.rm = TRUE),
    SD = stats::sd(conc_values, na.rm = TRUE),
    COV = ifelse(
      is.na(
        stats::sd(conc_values, na.rm = TRUE) /
          base::mean(conc_values, na.rm = TRUE)
      ),
      0,
      stats::sd(conc_values, na.rm = TRUE) /
        base::mean(conc_values, na.rm = TRUE)
    )
  )

  return(mk_result)
}

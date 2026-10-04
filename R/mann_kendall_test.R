#' Mann Kendall Function to loop through entire dataset
#'
#' Tests one series per location and analyte - and per `output_unit`, where
#' `data` carries one. A result in mg/L and one in ug/L are different numbers
#' for the same amount, so a location whose analyte was reported in two units
#' is tested as two series rather than one series of numbers that cannot be
#' compared. The locations this happens at are named in a message; convert
#' the results to a single unit beforehand to test them as one series. Each
#' series still needs four samples of its own, so a split series can drop out
#' where the whole would have been tested. Results with no unit recorded are
#' a series of their own too, named as `"no unit"`.
#'
#' @param data tibble processed with the data_processor function
#' @param traditional perform the standard analysis and not analyse for "Probably" or "Stable" trends
#' @param lor_multiplier Numeric value to multiply LOR concentrations by. Default is 1 (no change).
#'   Common values: 0 (zero substitution), 0.5 (half LOR), 1 (full LOR value).
#'   Set to NULL to use concentrations as-is without adjustment.
#' @param nd_threshold Optional numeric (0–1). Location-analyte combinations where the proportion of
#'   non-detects (prefix == "<") exceeds this value are excluded before analysis. Excluded
#'   combinations are reported to the console. Default is NULL (no filtering).
#'   Cannot be used together with \code{min_detects}.
#' @param min_detects Optional positive integer. Location-analyte combinations with fewer detected
#'   results than this value are excluded before analysis. A detect is any sample where prefix is
#'   not "<" (including NA). Excluded combinations are reported to the console.
#'   Default is NULL (no filtering). Cannot be used together with \code{nd_threshold}.
#' @param location_col Name of the column containing location codes.
#'   Can be provided with or without quotes. Default is location_code
#' @param chem_name_col Name of the column containing chemical/analyte names.
#'   Can be provided with or without quotes. Default is chem_name
#' @param concentration_col Name of the column containing concentration values.
#'   Can be provided with or without quotes. Default is concentration
#' @param date_col Name of the column containing sample dates.
#'   Can be provided with or without quotes. Default is date
#' @param prefix_col Name of the column containing prefix indicators (e.g., "<", "=").
#'   Can be provided with or without quotes. Default is prefix
#'
#' @return A nested tibble of trends as well as the original nested data, one
#'   row per location, analyte and - where `data` carries it - `output_unit`.
#'   The nested data is as reported; `lor_multiplier` is kept in the
#'   `"lor_multiplier"` attribute, so [mka_to_excel()] can summarise the
#'   series with the same substitution the test made.
#' @export
#'
#' @examples
#' # Six trend categories, LOR results left as reported
#' trends <- mann_kendall_test(gRs_data)
#' dplyr::select(trends, location_code, chem_name, p_value, trend)
#'
#' # Three categories only
#' mann_kendall_test(gRs_data, traditional = TRUE)
#'
#' # Half-LOR substitution before testing
#' mann_kendall_test(gRs_data, lor_multiplier = 0.5)
#'
#' # Drop combinations that are mostly non-detects
#' mann_kendall_test(gRs_data, nd_threshold = 0.75)
#'
#' # Custom column names, with or without quotes
#' \dontrun{
#' mann_kendall_test(
#'   my_data,
#'   location_col = site_code,
#'   chem_name_col = parameter,
#'   concentration_col = result_value
#' )
#' }
#'
#' @importFrom dplyr bind_rows filter mutate case_when select arrange
#' @importFrom tidyr tibble nest unnest drop_na
#' @importFrom purrr map map_dbl map_int
#' @importFrom rlang enquo quo_name !! :=
mann_kendall_test <- function(
  data,
  traditional = FALSE,
  lor_multiplier = 1,
  nd_threshold = NULL,
  min_detects = NULL,
  location_col = location_code,
  chem_name_col = chem_name,
  concentration_col = concentration,
  date_col = date,
  prefix_col = prefix
) {
  # Quote the column name arguments
  location_col <- rlang::enquo(location_col)
  chem_name_col <- rlang::enquo(chem_name_col)
  conc_col <- rlang::enquo(concentration_col)
  date_col <- rlang::enquo(date_col)
  prefix_col <- rlang::enquo(prefix_col)

  # Convert to strings for passing to mk_analysis
  conc_name <- rlang::quo_name(conc_col)
  date_name <- rlang::quo_name(date_col)
  prefix_name <- rlang::quo_name(prefix_col)

  loc_name <- rlang::quo_name(location_col)
  ana_name <- rlang::quo_name(chem_name_col)

  data <- tidyr::drop_na(data, !!conc_col)
  report_mixed_units(data, loc_name, ana_name)

  # Nested by unit as well, so a series never mixes numbers in mg/L with
  # numbers in ug/L.
  df <- data %>%
    tidyr::nest(
      .by = c(!!location_col, !!chem_name_col, dplyr::any_of("output_unit"))
    ) %>%
    dplyr::mutate(n_samples = purrr::map(data, nrow)) %>%
    tidyr::unnest(n_samples) %>%
    dplyr::filter(n_samples > 3) %>% # filter out entries with less than 4 data points
    dplyr::select(-n_samples)

  # Make sure there is at least one set of results to be analysed
  base::stopifnot("No data with more than 3 samples" = nrow(df) > 0)

  if (!is.null(nd_threshold) && !is.null(min_detects)) {
    stop(
      "Only one of `nd_threshold` or `min_detects` may be specified, not both."
    )
  }

  # How each series is named in the exclusion messages: with its unit where
  # its analyte comes in more than one, so the two series can be told apart.
  df$.series <- paste(
    df[[loc_name]],
    unit_labelled(df[[ana_name]], df[["output_unit"]]),
    sep = " / "
  )

  if (!is.null(nd_threshold)) {
    if (
      !is.numeric(nd_threshold) ||
        length(nd_threshold) != 1 ||
        nd_threshold < 0 ||
        nd_threshold > 1
    ) {
      stop(
        "`nd_threshold` must be a single numeric value between 0 and 1 (e.g., 0.75 for 75%)."
      )
    }

    df <- df %>%
      dplyr::mutate(
        nd_pct = purrr::map_dbl(data, function(d) {
          nd <- !is.na(d[[prefix_name]]) & d[[prefix_name]] == "<"
          mean(nd)
        })
      )

    excluded <- df %>% dplyr::filter(nd_pct > nd_threshold)

    if (nrow(excluded) > 0) {
      excl_lines <- paste(
        sprintf(
          "  - %s (%.1f%% non-detects)",
          excluded$.series,
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

    if (nrow(df) == 0) {
      stop(sprintf(
        "No data remaining after applying nd_threshold = %.2f. Consider raising the threshold.",
        nd_threshold
      ))
    }
  }

  if (!is.null(min_detects)) {
    if (
      !is.numeric(min_detects) ||
        length(min_detects) != 1 ||
        min_detects < 1 ||
        min_detects != as.integer(min_detects)
    ) {
      stop("`min_detects` must be a single positive integer.")
    }

    df <- df %>%
      dplyr::mutate(
        n_samples = purrr::map_int(data, nrow),
        n_detects = purrr::map_int(data, function(d) {
          sum(is.na(d[[prefix_name]]) | d[[prefix_name]] != "<")
        })
      )

    excluded <- df %>% dplyr::filter(n_detects < min_detects)

    if (nrow(excluded) > 0) {
      excl_lines <- paste(
        sprintf(
          "  - %s (%d detect(s) out of %d sample(s))",
          excluded$.series,
          excluded$n_detects,
          excluded$n_samples
        ),
        collapse = "\n"
      )
      message(sprintf(
        "Excluded %d location-analyte combination(s) with fewer than %d detect(s):\n%s",
        nrow(excluded),
        min_detects,
        excl_lines
      ))
    }

    df <- df %>%
      dplyr::filter(n_detects >= min_detects) %>%
      dplyr::select(-n_samples, -n_detects)

    if (nrow(df) == 0) {
      stop(sprintf(
        "No data remaining after applying min_detects = %d. Consider lowering the threshold.",
        min_detects
      ))
    }
  }

  df$.series <- NULL

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

  # mk_analysis() makes no substitution for NULL, which is the full LOR.
  attr(nested_df, "lor_multiplier") <- if (is.null(lor_multiplier)) {
    1
  } else {
    lor_multiplier
  }

  return(nested_df)
}


#' Name the series whose analyte was reported in more than one unit
#'
#' Each unit is tested as a series of its own, which is right but easy to
#' miss: a location that was one series is now two, and either may be too
#' short to test. So the split is said out loud, with the way to avoid it.
#'
#' @param data the results being tested
#' @param loc_name,ana_name names of the location and analyte columns
#' @noRd
report_mixed_units <- function(data, loc_name, ana_name) {
  if (!"output_unit" %in% names(data) || nrow(data) == 0) {
    return(invisible(NULL))
  }
  unit <- as.character(data$output_unit)
  key <- paste(data[[loc_name]], data[[ana_name]], sep = " / ")
  # A result with no unit is nested apart from the rest, so it counts as a
  # unit of its own here too.
  units <- tapply(unit, key, function(u) sort(unique(u), na.last = TRUE))
  mixed <- units[lengths(units) > 1]
  if (length(mixed) == 0) {
    return(invisible(NULL))
  }
  lines <- paste0(
    "  - ",
    names(mixed),
    " (",
    vapply(
      mixed,
      function(u) paste(ifelse(is.na(u), NO_UNIT, u), collapse = ", "),
      character(1)
    ),
    ")"
  )
  message(
    length(mixed),
    " location/analyte series hold results in more than one unit, so each ",
    "unit is tested as a separate trend:\n",
    paste(utils::head(lines, 10), collapse = "\n"),
    if (length(lines) > 10) paste0("\n  ... and ", length(lines) - 10, " more"),
    "\nConvert the results to one unit first to test each as a single series."
  )
}


#' Name an analyte with its unit where it is reported in more than one
#'
#' A trend is tested per unit, so an analyte reported in two units has two
#' trends at a location, and laid out by analyte - the columns of
#' [mka_to_excel()], the rows of the heatmaps - the two would land in one
#' cell. Those analytes carry their unit in the name, e.g. `"Zinc (mg/L)"`.
#' An analyte reported in a single unit keeps its name unchanged, so a table
#' that never mixes units reads as it always has. A missing unit is tested
#' as a series of its own, so it counts as a unit here: `"Zinc (no unit)"`.
#'
#' @param chem analyte names
#' @param unit the unit of each, or `NULL` where the table carries none
#' @returns character vector of labels, one per element of `chem`
#' @noRd
unit_labelled <- function(chem, unit) {
  chem <- as.character(chem)
  if (is.null(unit) || length(chem) == 0) {
    return(chem)
  }
  unit <- as.character(unit)
  n_units <- tapply(unit, chem, function(u) length(unique(u)))
  mixed <- chem %in% names(n_units)[n_units > 1]
  shown <- ifelse(is.na(unit), NO_UNIT, unit)
  chem[mixed] <- paste0(chem[mixed], " (", shown[mixed], ")")
  chem
}

# How a missing unit is named where an analyte also comes in a real one.
NO_UNIT <- "no unit"


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
#' # Normally reached through mann_kendall_test(), which calls it once per
#' # location/analyte combination. Called directly it takes one such subset.
#' one_series <- dplyr::filter(
#'   gRs_data,
#'   location_code == gRs_data$location_code[1],
#'   chem_name == gRs_data$chem_name[1]
#' )
#'
#' mk_analysis(one_series)
#' mk_analysis(one_series, lor_multiplier = 0.5)
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

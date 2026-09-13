#' The results that exceeded their guideline
#'
#' Every result in a monitoring round that exceeded the guideline joined onto
#' it by [join_action_levels()], one row per exceeding result, worst first
#' within each analyte. This is the record behind the counts
#' [analyte_summary()] reports and the flags [historical_range()] sets: all
#' three read the same `exceedance` column, so no table can report an
#' exceedance another does not.
#'
#' Whether a non-detect whose limit of reporting sits above the guideline
#' counts as an exceedance was settled once by [join_action_levels()], through
#' its `lor_as_exceedance` argument. It is not re-decided here. Those results
#' are worth reporting in their own right, though - a limit of reporting too
#' high to demonstrate compliance is a finding, not a pass - so
#' `include_lor = TRUE` adds them, told apart by the `lor_above_criteria`
#' column. Doing so means the row count no longer matches
#' [analyte_summary()]'s `n_exceedances`, which counts exceedances alone.
#'
#' A named list of the exceeding locations for each analyte is attached as a
#' `"locations"` attribute, for writing inline text. Every analyte that had a
#' guideline in the round appears in it, so an analyte that exceeded nothing
#' gives `character(0)` rather than `NULL` and inline code does not have to
#' guard against a missing name.
#'
#' @param data chemistry tibble with a guideline set joined on by
#'   [join_action_levels()].
#' @param round_col name of the column naming the monitoring round. Can be
#'   given with or without quotes. By default the first of `monitoring_round`,
#'   `task_code`, `date` or `sampled_date_time` that `data` carries.
#' @param round the round to report on. Default `NULL`, the latest round
#'   present.
#' @param criteria_col name of the column holding the guideline value, matching
#'   the `value_col` it was joined under by [join_action_levels()]. Can be
#'   given with or without quotes. Default `criteria`.
#' @param include_lor also return the non-detects whose limit of reporting sits
#'   above the guideline. Default `FALSE`.
#' @param save_path full file path, including filename, to write the rows to.
#'   The directory is created if it does not exist.
#' @param quiet suppress the messages naming the round and counting what was
#'   found.
#'
#' @returns `data`'s exceeding rows, with the reporting columns moved to the
#'   front and all others kept, ordered by analyte and then by
#'   `exceedance_ratio` descending. A round in which nothing exceeded returns a
#'   table with no rows rather than `NULL`. The round is attached as a
#'   `"round"` attribute and the per-analyte exceeding locations as a
#'   `"locations"` attribute.
#' @export
#'
#' @examples
#' \dontrun{
#' chem <- data_processor("davLChem1_Chemistry.xlsx")
#' anzg <- action_level_processor("ANZG 95 Marine.xlsx", name = "ANZG 95%")
#' compared <- join_action_levels(chem, anzg)
#'
#' exceeded <- exceedance_summary(compared)
#' exceeded %>% create_gt()
#'
#' # for inline text: "PFOS exceeded the guideline at MW01, MW06 and MW07."
#' locs <- attr(exceeded, "locations")
#' locs[["Perfluorooctane sulfonic acid (PFOS)"]]
#'
#' # limits of reporting too high to demonstrate compliance, reported with them
#' exceedance_summary(compared, include_lor = TRUE)
#'
#' # one guideline set at a time where several are stacked
#' compared %>%
#'   criteria_long() %>%
#'   dplyr::filter(criteria_set == "criteria_99") %>%
#'   exceedance_summary()
#' }
#' @seealso [analyte_summary()] for the per-analyte counts of the same
#'   exceedances, [join_action_levels()] for the comparison this reads, and
#'   [create_gt()] to format the result.
#' @importFrom dplyr relocate any_of
#' @importFrom rlang enquo as_label abort caller_env
exceedance_summary <- function(
  data,
  round_col = NULL,
  round = NULL,
  criteria_col = criteria,
  include_lor = FALSE,
  save_path = NULL,
  quiet = FALSE
) {
  if (is.null(data)) {
    return(NULL)
  }

  value_name <- quo_column_name(rlang::enquo(criteria_col))
  cmp <- comparison_columns(value_name)

  prepared <- prepare_round_summary(
    data,
    required = c(
      "chem_name",
      "location_code",
      "concentration",
      "detect_flag"
    ),
    round_col = quo_column_name(rlang::enquo(round_col)),
    round = round,
    value_name = value_name,
    criteria_required = TRUE,
    quiet = quiet
  )
  picked <- prepared$picked

  current <- data[picked$is_current, , drop = FALSE]
  crit <- suppressWarnings(as.numeric(current[[value_name]]))

  exceed <- exceedance_verdict(current, value_name, cmp)
  lor <- lor_verdict(current, value_name, cmp)

  hit <- !is.na(exceed) & exceed
  if (include_lor) {
    hit <- hit | (!is.na(lor) & lor)
  }

  out <- current[hit, , drop = FALSE]

  # Worst first within each analyte, which is the order the rows are read in.
  ratio <- if (cmp[["ratio"]] %in% names(out)) {
    suppressWarnings(as.numeric(out[[cmp[["ratio"]]]]))
  } else {
    suppressWarnings(as.numeric(out$concentration)) /
      suppressWarnings(as.numeric(out[[value_name]]))
  }
  out <- out[order(out$chem_name, -ratio, out$location_code), , drop = FALSE]

  out <- dplyr::relocate(
    out,
    dplyr::any_of(c(
      "location_code",
      "date",
      "chem_name",
      "criteria_set",
      "prefix",
      "concentration",
      "output_unit",
      value_name,
      paste0(value_name, "_name"),
      unname(cmp)
    ))
  )

  attr(out, "round") <- list(column = picked$col, round = picked$value)
  attr(out, "locations") <- exceeding_locations(out, current$chem_name[!is.na(crit)])

  if (!quiet) {
    report_exceedances(out, picked, include_lor)
  }

  write_summary(out, save_path)

  out
}


#' The exceeding locations for each analyte that had a guideline
#'
#' Keyed on every analyte the round held a guideline for, so an analyte that
#' exceeded nothing gives `character(0)` rather than `NULL` and inline text
#' does not have to guard against a missing name. Where several guideline sets
#' have been stacked by [criteria_long()], their locations are pooled - filter
#' to one set first if that matters.
#'
#' @param out the exceeding rows
#' @param compared chem_name for every result that had a guideline
#' @returns a named list of sorted, de-duplicated location vectors
#' @noRd
exceeding_locations <- function(out, compared) {
  analytes <- sort(unique(stats::na.omit(as.character(compared))))
  locs <- lapply(analytes, function(nm) {
    sort(unique(as.character(out$location_code[out$chem_name == nm])))
  })
  stats::setNames(locs, analytes)
}


#' Say what the round turned up
#'
#' @param out the exceeding rows
#' @param picked the resolved round
#' @param include_lor whether limits of reporting were included
#' @noRd
report_exceedances <- function(out, picked, include_lor) {
  if (nrow(out) == 0) {
    message(
      "exceedance_summary(): nothing in ",
      round_label(picked),
      " exceeded its guideline."
    )
    return(invisible(NULL))
  }
  message(
    nrow(out),
    if (include_lor) {
      " results exceeded their guideline or were reported above it"
    } else {
      " results exceeded their guideline"
    },
    " in ",
    round_label(picked),
    ", across ",
    length(unique(out$location_code)),
    " locations and ",
    length(unique(out$chem_name)),
    " analytes."
  )
}

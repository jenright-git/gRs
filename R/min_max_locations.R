#' The highest and lowest results of a monitoring round, and where they came
#' from
#'
#' The two ends of each analyte's range in a single monitoring round, one row
#' per selected result, carrying the location, the sample and everything else
#' the result was reported with. Where [analyte_summary()] reduces each analyte
#' to a single row and names only the location the maximum came from, this
#' returns the results themselves, as many from each end as are asked for.
#'
#' The conventions are the ones [analyte_summary()] follows, so the two tables
#' cannot disagree about what the highest result of a round was:
#'
#' * The **maximum** is the highest *detected* result. Where nothing was
#'   detected there is no detection to report, so the highest result stands in
#'   and carries its `<` through in `prefix` rather than the row coming out
#'   blank.
#' * The **minimum** is the lowest result of the round, detected or not, so a
#'   non-detect can legitimately be reported as one.
#'
#' Results are ranked within each analyte, and within `output_unit` and
#' `criteria_set` where the table carries them, so results reported in two
#' units are never ranked against each other. `rank` is `1` for the highest
#' result of the `"Maximum"` rows and the lowest of the `"Minimum"` rows;
#' results of equal concentration share a rank.
#'
#' Asking for more results than an analyte holds returns what it has, so a
#' group with a single result reports that result at both ends of its range.
#' That is the range the record shows rather than an error, but it is worth
#' reading `n_max` and `n_min` as a ceiling rather than a promise.
#'
#' @param data chemistry tibble from [data_processor()].
#' @param n_max how many results to report from the top of each analyte's
#'   range. Default 1. `0` reports the minima alone.
#' @param n_min how many results to report from the bottom. Default 1. `0`
#'   reports the maxima alone.
#' @param round_col name of the column naming the monitoring round. Can be
#'   given with or without quotes. By default the first of `monitoring_round`,
#'   `task_code`, `date` or `sampled_date_time` that `data` carries.
#' @param round the round to report on. Default `NULL`, the latest round
#'   present - resolved by the latest date sampled within each round where the
#'   round is recorded as text, so `"2025 Q10"` is not read as earlier than
#'   `"2025 Q9"`.
#' @param group_vars character vector of further columns to rank within, e.g.
#'   `"monitoring_zone"` for the highest and lowest result of each zone.
#'   Analytes are always grouped by `chem_name`, and by `output_unit` and
#'   `criteria_set` where those are present.
#' @param with_ties keep every result tied with the last one selected, so an
#'   analyte whose two highest results are equal reports both. Default `FALSE`,
#'   which breaks the tie on `location_code` and returns the number asked for.
#' @param save_path full file path, including filename, to write the rows to,
#'   e.g. `"output/round-extremes.xlsx"`. The directory is created if it does
#'   not exist.
#' @param quiet suppress the messages naming the round and counting what was
#'   found.
#'
#' @returns `data`'s selected rows, with `extreme` (`"Maximum"` or
#'   `"Minimum"`) and `rank` added, the reporting columns moved to the front
#'   and all others kept. Ordered by analyte, then maxima before minima, then
#'   by rank. The round reported on is attached as a `"round"` attribute.
#'   Format it for a report with [create_gt()].
#' @export
#'
#' @examples
#' # the highest and the lowest result of each analyte, and where each was
#' min_max_locations(gRs_data)
#'
#' # the peak, and the four cleanest locations behind it
#' min_max_locations(gRs_data, n_max = 1, n_min = 4)
#'
#' \dontrun{
#' chem <- data_processor("davLChem1_Chemistry.xlsx")
#'
#' # the highest and lowest result of each zone, formatted for a report
#' chem %>%
#'   min_max_locations(group_vars = "monitoring_zone") %>%
#'   create_gt()
#' }
#' @seealso [analyte_summary()] for the per-analyte summary these results are
#'   the ends of, [historical_range()] to compare the round against the record
#'   behind it, and [create_gt()] to format the result.
#' @importFrom dplyr group_by group_modify ungroup across all_of any_of
#'   relocate
#' @importFrom rlang enquo as_label abort caller_env
min_max_locations <- function(
  data,
  n_max = 1,
  n_min = 1,
  round_col = NULL,
  round = NULL,
  group_vars = NULL,
  with_ties = FALSE,
  save_path = NULL,
  quiet = FALSE
) {
  if (is.null(data)) {
    return(NULL)
  }

  require_columns(
    data,
    c("chem_name", "location_code", "concentration", "detect_flag")
  )

  counts <- lapply(list(n_max = n_max, n_min = n_min), read_count)
  bad <- names(counts)[vapply(counts, is.na, logical(1))]
  if (length(bad) > 0) {
    stop(
      "`",
      bad[[1]],
      "` must be a single non-negative whole number."
    )
  }
  n_max <- counts$n_max
  n_min <- counts$n_min
  if (n_max == 0 && n_min == 0) {
    stop(
      "`n_max` and `n_min` are both 0, so there is nothing to report. Ask ",
      "for at least one result from one end of the range."
    )
  }

  prepared <- prepare_round_summary(
    data,
    required = character(0),
    round_col = quo_column_name(rlang::enquo(round_col)),
    round = round,
    group_vars = group_vars,
    quiet = quiet
  )
  picked <- prepared$picked
  groups <- prepared$groups

  current <- data[picked$is_current, , drop = FALSE]

  out <- current %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
    dplyr::group_modify(~ extreme_rows(.x, n_max, n_min, with_ties)) %>%
    dplyr::ungroup()

  out <- dplyr::relocate(
    out,
    dplyr::any_of(c(
      "chem_name",
      "criteria_set",
      "extreme",
      "rank",
      "location_code",
      "field_id",
      "date",
      "prefix",
      "concentration",
      "output_unit"
    ))
  )

  attr(out, "round") <- list(column = picked$col, round = picked$value)

  if (!quiet) {
    report_min_max(out, picked, n_max, n_min)
  }

  write_summary(out, save_path)

  out
}


#' Both ends of one analyte's range
#'
#' @param df the group's results, without its grouping columns
#' @param n_max how many results to take from the top
#' @param n_min how many to take from the bottom
#' @param with_ties keep results tied with the last one taken
#' @returns the selected rows, with `extreme` and `rank` added
#' @noRd
extreme_rows <- function(df, n_max, n_min, with_ties) {
  g <- group_vectors(df)
  conc <- g$conc
  usable <- which(g$usable)
  tie_break <- as.character(df$location_code)

  # The same rule analyte_summary() reports the maximum under, so the two
  # tables cannot disagree about the highest result of a round.
  pool <- which(detected_pool(g$detect, g$usable) & g$usable)

  top <- pick_extreme(conc, pool, n_max, TRUE, with_ties, tie_break)
  bottom <- pick_extreme(conc, usable, n_min, FALSE, with_ties, tie_break)

  out <- df[c(top, bottom), , drop = FALSE]
  out$extreme <- rep(c("Maximum", "Minimum"), c(length(top), length(bottom)))
  out$rank <- c(rank_within(conc[top], TRUE), rank_within(conc[bottom], FALSE))
  out
}


#' Take the n most extreme results from a pool
#'
#' @param conc the group's concentrations
#' @param pool row positions eligible to be picked
#' @param n how many to take
#' @param decreasing take the highest rather than the lowest
#' @param with_ties keep every result tied with the nth
#' @param tie_break values ordering results of equal concentration
#' @returns row positions, most extreme first
#' @noRd
pick_extreme <- function(conc, pool, n, decreasing, with_ties, tie_break) {
  if (n == 0 || length(pool) == 0) {
    return(integer(0))
  }
  ord <- pool[order(
    if (decreasing) -conc[pool] else conc[pool],
    tie_break[pool]
  )]
  if (n >= length(ord)) {
    return(ord)
  }
  if (!with_ties) {
    return(ord[seq_len(n)])
  }
  # ord is sorted, so everything tied with the nth sits beside it
  ord[seq_along(ord) <= n | conc[ord] == conc[[ord[[n]]]]]
}


#' Rank selected results, ties sharing a rank
#'
#' @param x the selected concentrations
#' @param decreasing rank the highest first
#' @returns an integer vector
#' @noRd
rank_within <- function(x, decreasing) {
  if (length(x) == 0) {
    return(integer(0))
  }
  match(x, sort(unique(x), decreasing = decreasing))
}


#' Read a count argument
#'
#' Returns `NA` rather than erroring, so the argument that was wrong is named
#' by the function the user called rather than by this one.
#'
#' @param n the value given
#' @returns a single non-negative integer, or `NA_integer_`
#' @noRd
read_count <- function(n) {
  if (length(n) == 0) {
    return(NA_integer_)
  }
  n <- suppressWarnings(as.numeric(n))[[1]]
  if (is.na(n) || n < 0 || n != round(n)) {
    return(NA_integer_)
  }
  as.integer(n)
}


#' Say what the round's extremes came to
#'
#' @param out the selected rows
#' @param picked the resolved round
#' @param n_max how many results were asked for from the top
#' @param n_min how many from the bottom
#' @noRd
report_min_max <- function(out, picked, n_max, n_min) {
  if (nrow(out) == 0) {
    message(
      "min_max_locations(): no readable concentration in ",
      round_label(picked),
      "."
    )
    return(invisible(NULL))
  }
  message(
    nrow(out),
    " results reported across ",
    length(unique(out$chem_name)),
    " analytes in ",
    round_label(picked),
    ": the highest ",
    n_max,
    " and the lowest ",
    n_min,
    " of each."
  )
}

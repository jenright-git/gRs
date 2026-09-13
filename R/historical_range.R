#' Compare a monitoring round against the record behind it
#'
#' One row per location and analyte, holding the historical range of results
#' and the result from the round being reported on, with flags marking the
#' ones that set a new maximum or minimum. The screening check behind a
#' "New Maximums" section, and the per-location historical range table that
#' usually sits beside it, are the same function with different arguments -
#' `keep` chooses between them.
#'
#' Three conventions decide what the table says. They are not symmetrical, on
#' purpose:
#'
#' * The historical **minimum** is taken over all prior results, detected or
#'   not, so a non-detect can legitimately set a new minimum.
#' * The historical **maximum** is taken over prior **detections** only. Limits
#'   of reporting fall over the life of a monitoring program, and an old
#'   `<0.5` would otherwise stand as a maximum that was never measured.
#' * The **current** result is the *maximum* of the round, not whichever result
#'   happened to sort first. Where a group holds more than one result for the
#'   round - a field duplicate that was never collapsed - that is a data
#'   quality problem, so the groups are named in a warning rather than one
#'   result being picked silently. [select_max_concentration()] is the fix.
#'   The warning only applies where the grouping identifies a single sampling
#'   point: grouped across locations, several results in a round is the
#'   ordinary case.
#'
#' A new **maximum** therefore requires a detection on both sides: a `<0.5`
#' reported this round can never be called a new maximum however high the limit
#' of reporting of the day happened to be.
#'
#' `max_ratio` is the current result as a multiple of the previous detected
#' maximum, and `spike` marks the ones above `spike_factor`. A jump that large
#' is more often a transcription or a unit problem than a real change, so it is
#' worth checking against the laboratory report before any text is written
#' around it.
#'
#' History is everything sampled **before** the round, so a round taken from
#' the middle of the record is compared against what preceded it rather than
#' against results that had not been collected yet. Results sampled after the
#' round are left out altogether, `n_samples` included.
#'
#' @param data chemistry tibble from [data_processor()], holding the whole
#'   record and not just the round of interest.
#' @param group_vars character vector of columns to group by within each
#'   analyte. Default `"location_code"`. Pass `character(0)` to summarise each
#'   analyte across all locations. Analytes are always grouped by `chem_name`,
#'   and by `output_unit` and `criteria_set` where those are present.
#' @param round_col name of the column naming the monitoring round. Can be
#'   given with or without quotes. By default the first of `monitoring_round`,
#'   `task_code`, `date` or `sampled_date_time` that `data` carries.
#' @param round the round to report on. Default `NULL`, the latest round
#'   present.
#' @param date_col name of the date column that orders the record. Can be
#'   given with or without quotes. Default `date`, which [data_processor()]
#'   always returns. Where it is absent, every result outside the round is
#'   treated as history and a warning says so.
#' @param spike_factor a new maximum this many times the previous detected
#'   maximum is flagged in `spike`. Default 10. `NULL` switches the check off.
#' @param keep which rows to return. `"all"` (default) returns every group;
#'   `"new_max"` and `"spike"` cut it to the screening list, ordered by
#'   `max_ratio` descending; `"exceedance"` cuts it to the groups whose current
#'   result exceeded its guideline, and needs a guideline joined on.
#' @param criteria_col name of the column holding the guideline value, matching
#'   the `value_col` it was joined under by [join_action_levels()]. Can be
#'   given with or without quotes. Default `criteria`.
#' @param include_criteria include the guideline and whether the current result
#'   exceeded it. Default `NULL`, which includes them when `criteria_col` is
#'   present.
#' @param save_path full file path, including filename, to write the table to.
#'   The directory is created if it does not exist.
#' @param quiet suppress the messages naming the round and reporting an empty
#'   result.
#'
#' @returns A tibble with one row per group, carrying `n_samples` (results held
#'   up to and including the round), `n_current`, `hist_min_prefix`,
#'   `hist_min_conc`, `hist_max_prefix`, `hist_max_conc`, `current_prefix`,
#'   `current_conc`, `new_max`, `new_min`, `max_ratio` and `spike`, plus -
#'   with `include_criteria` - `criteria` and `current_exceedance`. A group
#'   with no history reports `NA` rather than the `Inf` an empty `min()` gives;
#'   a group not sampled this round reports `NA` for the current result. The
#'   round is attached as a `"round"` attribute. Format it with [create_gt()].
#' @export
#'
#' @examples
#' # every location and analyte, latest round against its own history
#' historical_range(gRs_data)
#'
#' # the screening list: this round's detections well above anything before
#' historical_range(gRs_data, keep = "spike")
#'
#' \dontrun{
#' # one analyte, per location, as a report table
#' chem %>%
#'   dplyr::filter(chem_name == "Copper") %>%
#'   historical_range() %>%
#'   create_gt()
#'
#' # only where the current result exceeded its guideline
#' compared %>% historical_range(keep = "exceedance")
#' }
#' @seealso [analyte_summary()] for the per-analyte summary of the same round,
#'   [select_max_concentration()] to collapse duplicates beforehand, and
#'   [create_gt()] to format the result.
#' @importFrom dplyr group_by group_modify ungroup across all_of arrange desc
#' @importFrom tibble tibble
#' @importFrom rlang enquo as_label abort caller_env
historical_range <- function(
  data,
  group_vars = "location_code",
  round_col = NULL,
  round = NULL,
  date_col = date,
  spike_factor = 10,
  keep = c("all", "new_max", "spike", "exceedance"),
  criteria_col = criteria,
  include_criteria = NULL,
  save_path = NULL,
  quiet = FALSE
) {
  if (is.null(data)) {
    return(NULL)
  }
  keep <- match.arg(keep)

  value_name <- quo_column_name(rlang::enquo(criteria_col))
  cmp <- comparison_columns(value_name)

  require_columns(data, c("chem_name", "concentration", "detect_flag"))

  if (!is.null(spike_factor)) {
    spike_factor <- as.numeric(spike_factor)[[1]]
    if (is.na(spike_factor) || spike_factor <= 0) {
      stop("`spike_factor` must be a positive number, or NULL to switch the ",
           "check off.")
    }
  } else if (identical(keep, "spike")) {
    stop(
      "`keep = \"spike\"` needs a `spike_factor` to compare against, but it ",
      "is NULL."
    )
  }

  prepared <- prepare_round_summary(
    data,
    required = character(0),
    round_col = quo_column_name(rlang::enquo(round_col)),
    round = round,
    group_vars = group_vars,
    value_name = value_name,
    include_criteria = include_criteria,
    quiet = quiet
  )
  picked <- prepared$picked
  groups <- prepared$groups
  include_criteria <- prepared$include_criteria

  if (identical(keep, "exceedance") && !include_criteria) {
    stop(
      "`keep = \"exceedance\"` needs a guideline to compare against. Join one ",
      "with join_action_levels() first."
    )
  }

  date_name <- quo_column_name(rlang::enquo(date_col))

  work <- data
  work$.current <- picked$is_current

  # History is what came before the round. A round taken from the middle of
  # the record is compared against what preceded it, not against results that
  # had not been collected when it was reported.
  if (date_name %in% names(work)) {
    dates <- work[[date_name]]
    round_start <- suppressWarnings(min(dates[work$.current], na.rm = TRUE))
    prior <- !work$.current & !is.na(dates) & dates < round_start
    work <- work[prior | work$.current, , drop = FALSE]
  } else {
    warning(
      "`data` has no '",
      date_name,
      "' column, so every result outside ",
      round_label(picked),
      " is treated as history - including any collected after it. Name the ",
      "date column with `date_col`."
    )
  }

  if (include_criteria) {
    work$.criteria <- suppressWarnings(as.numeric(work[[value_name]]))
    work$.exceedance <- exceedance_verdict(work, value_name, cmp)
  }

  out <- work %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
    dplyr::group_modify(
      ~ historical_row(.x, .y, spike_factor, include_criteria)
    ) %>%
    dplyr::ungroup()

  report_duplicate_rounds(out, groups, picked)

  out <- switch(
    keep,
    all = out,
    new_max = out[out$new_max, , drop = FALSE],
    spike = out[out$spike, , drop = FALSE],
    exceedance = out[
      !is.na(out$current_exceedance) & out$current_exceedance, ,
      drop = FALSE
    ]
  )

  # The screening lists are read worst-first; the full table keeps group order.
  if (keep %in% c("new_max", "spike")) {
    out <- dplyr::arrange(out, dplyr::desc(max_ratio))
  }

  if (nrow(out) == 0 && !quiet) {
    message(
      "historical_range(): nothing in ",
      round_label(picked),
      " met keep = \"",
      keep,
      "\"",
      if (identical(keep, "spike")) {
        paste0(" (", spike_factor, "x the previous detected maximum)")
      } else {
        ""
      },
      "."
    )
  }

  if (include_criteria) {
    out <- rename_criteria_columns(out, value_name)
  }

  attr(out, "round") <- list(column = picked$col, round = picked$value)
  # Carried so create_gt() can say what the shading means without being told
  # the factor a second time.
  attr(out, "spike_factor") <- spike_factor

  write_summary(out, save_path)

  out
}


#' Reduce one group's record to its historical range and current result
#'
#' @param df the group's results, without its grouping columns, carrying the
#'   `.current` flag and - where asked for - `.criteria` and `.exceedance`
#' @param key one-row tibble of the group's grouping values
#' @param spike_factor multiple of the previous detected maximum that counts as
#'   a spike, or NULL
#' @param include_criteria add the guideline columns
#' @returns a one-row tibble
#' @noRd
historical_row <- function(df, key, spike_factor, include_criteria) {
  g <- group_vectors(df)
  conc <- g$conc
  detect <- g$detect
  usable <- g$usable
  prefix <- g$prefix
  cur <- df$.current
  hist <- !cur

  pick <- function(mask, choose) pick_within(conc, usable, mask, choose)
  min_i <- pick(hist, which.min)
  max_i <- pick(hist & detect, which.max)
  cur_i <- pick(cur, which.max)

  at <- value_at

  hist_min <- at(conc, min_i, NA_real_)
  hist_max <- at(conc, max_i, NA_real_)
  current <- at(conc, cur_i, NA_real_)
  current_detect <- length(cur_i) > 0 && detect[[cur_i]]

  new_max <- current_detect && !is.na(current) && !is.na(hist_max) &&
    current > hist_max
  new_min <- !is.na(current) && !is.na(hist_min) && current < hist_min
  max_ratio <- if (new_max && hist_max > 0) current / hist_max else NA_real_
  spike <- !is.null(spike_factor) && new_max && !is.na(max_ratio) &&
    max_ratio > spike_factor

  out <- tibble::tibble(
    n_samples = nrow(df),
    n_current = sum(cur),
    hist_min_prefix = at(prefix, min_i, NA_character_),
    hist_min_conc = hist_min,
    hist_max_prefix = at(prefix, max_i, NA_character_),
    hist_max_conc = hist_max,
    current_prefix = at(prefix, cur_i, NA_character_),
    current_conc = current,
    new_max = new_max,
    new_min = new_min,
    max_ratio = max_ratio,
    spike = spike
  )

  if (!include_criteria) {
    return(out)
  }

  # The guideline does not change from round to round, so it is read across
  # the whole record; the verdict is the one join_action_levels() reached for
  # the result actually being reported.
  out$criteria <- single_criteria(df$.criteria, group_label(key))
  out$current_exceedance <- at(df$.exceedance, cur_i, NA)
  out
}


#' Name the groups holding more than one result for the round
#'
#' The raw feed is known to carry field duplicates that were never collapsed,
#' so this says which groups were reduced rather than picking one silently.
#'
#' Only meaningful where the group identifies a single sampling point. Grouped
#' across locations - `group_vars = character(0)`, or by zone - a round holding
#' several results is the ordinary case and not worth a word.
#'
#' @param out the summarised table
#' @param groups the grouping columns
#' @param picked the resolved round
#' @noRd
report_duplicate_rounds <- function(out, groups, picked) {
  if (!"location_code" %in% groups) {
    return(invisible(NULL))
  }
  dup <- which(out$n_current > 1)
  if (length(dup) == 0) {
    return(invisible(NULL))
  }
  labels <- do.call(
    paste,
    c(lapply(out[dup, groups, drop = FALSE], as.character), sep = " / ")
  )
  labels <- unique(labels)
  warning(
    length(dup),
    " groups hold more than one result in ",
    round_label(picked),
    "; the maximum was reported: ",
    paste(utils::head(labels, 8), collapse = ", "),
    if (length(labels) > 8) ", ..." else "",
    ".\nCollapse field duplicates with select_max_concentration() first.",
    call. = FALSE
  )
}

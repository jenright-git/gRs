#' Per-analyte summary of a monitoring round
#'
#' One row per analyte for a single monitoring round: how many samples were
#' taken, how many detected the analyte, the range of concentrations, where the
#' maximum came from, and - where a guideline set has been joined on by
#' [join_action_levels()] - which locations exceeded it.
#'
#' Distinct from [summary_stats()], which is one row per location *and*
#' chemical across the whole record, with percentiles. This is the
#' "what did this round find, and where did it exceed" table.
#'
#' Two conventions are worth stating, because they decide what the table says:
#'
#' * The **maximum** reported is the highest *detected* result. Where nothing
#'   was detected there is no detection to report, so the highest result is
#'   used instead and its `<` is carried through in `max_prefix` rather than
#'   the row coming out blank. The **minimum** is the lowest result of the
#'   round, detected or not.
#' * An **exceedance** is whatever [join_action_levels()] decided it was. That
#'   function settles once - through its `lor_as_exceedance` argument - whether
#'   a non-detect whose limit of reporting sits above the guideline counts, and
#'   the verdict is counted here as it stands. A `criteria` column added by
#'   hand carries no verdict, and falls back to detected results above the
#'   guideline.
#'
#' Where an analyte has no guideline at all, the exceedance columns are `NA`
#' rather than `0`: a `0` means a guideline applies and nothing exceeded it.
#'
#' @param data chemistry tibble from [data_processor()], usually with a
#'   guideline set joined on by [join_action_levels()].
#' @param round_col name of the column naming the monitoring round. Can be
#'   given with or without quotes. By default the first of `monitoring_round`,
#'   `task_code`, `date` or `sampled_date_time` that `data` carries.
#' @param round the round to report on. Default `NULL`, the latest round
#'   present - resolved by the latest date sampled within each round where the
#'   round is recorded as text, so `"2025 Q10"` is not read as earlier than
#'   `"2025 Q9"`.
#' @param group_vars character vector of further columns to group by, e.g.
#'   `"monitoring_zone"`. Analytes are always grouped by `chem_name`, and by
#'   `output_unit` and `criteria_set` where those are present, so results
#'   reported in two units are never pooled.
#' @param criteria_col name of the column holding the guideline value, matching
#'   the `value_col` it was joined under by [join_action_levels()]. Can be
#'   given with or without quotes. Default `criteria`.
#' @param include_criteria include the guideline and the exceedance columns.
#'   Default `NULL`, which includes them when `criteria_col` is present.
#' @param save_path full file path, including filename, to write the table to,
#'   e.g. `"output/round-summary.xlsx"`. The directory is created if it does
#'   not exist.
#' @param quiet suppress the message naming the round that was reported on.
#'
#' @returns A tibble with one row per analyte (and per `group_vars`,
#'   `output_unit` and `criteria_set` where present), carrying `n_samples`,
#'   `n_detects`, `pct_detects`, `min_prefix`, `min_conc`, `max_prefix`,
#'   `max_conc` and `max_location`, plus - with `include_criteria` - the
#'   guideline value, `n_exceedances`, `n_exceedance_locations` and
#'   `exceedance_locations`. The round reported on is attached as a `"round"`
#'   attribute. Format it for a report with [create_gt()].
#' @export
#'
#' @examples
#' analyte_summary(gRs_data)
#'
#' \dontrun{
#' chem <- data_processor("davLChem1_Chemistry.xlsx")
#' anzg <- action_level_processor("ANZG 95 Marine.xlsx", name = "ANZG 95%")
#' compared <- join_action_levels(chem, anzg)
#'
#' analyte_summary(compared)
#' analyte_summary(compared, group_vars = "monitoring_zone")
#'
#' # one row per analyte per guideline set
#' compared %>%
#'   join_action_levels(anzg_99, value_col = "criteria_99") %>%
#'   criteria_long() %>%
#'   analyte_summary()
#'
#' # formatted for a report
#' analyte_summary(compared) %>% create_gt()
#' }
#' @seealso [summary_stats()] for per-location statistics across the whole
#'   record, [join_action_levels()] for the guideline join this reads, and
#'   [create_gt()] to format the result.
#' @importFrom dplyr group_by group_modify ungroup across all_of bind_cols
#' @importFrom tibble tibble
#' @importFrom rlang enquo as_label abort caller_env
analyte_summary <- function(
  data,
  round_col = NULL,
  round = NULL,
  group_vars = NULL,
  criteria_col = criteria,
  include_criteria = NULL,
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
    group_vars = group_vars,
    value_name = value_name,
    include_criteria = include_criteria,
    quiet = quiet
  )
  picked <- prepared$picked
  groups <- prepared$groups
  include_criteria <- prepared$include_criteria

  if (include_criteria && !quiet && !"criteria_set" %in% names(data)) {
    joined <- criteria_sets(data)
    if (length(joined) > 1) {
      message(
        "`data` carries ",
        length(joined),
        " guideline sets (",
        paste(joined, collapse = ", "),
        "); summarising '",
        value_name,
        "'. criteria_long() stacks them so all are summarised at once."
      )
    }
  }

  current <- data[picked$is_current, , drop = FALSE]

  if (include_criteria) {
    current$.criteria <- suppressWarnings(as.numeric(current[[value_name]]))
    current$.exceedance <- exceedance_verdict(current, value_name, cmp)
  }

  out <- current %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
    dplyr::group_modify(~ analyte_row(.x, .y, include_criteria)) %>%
    dplyr::ungroup()

  if (include_criteria) {
    out <- rename_criteria_columns(out, value_name)
  }

  attr(out, "round") <- list(column = picked$col, round = picked$value)

  write_summary(out, save_path)

  out
}


#' Summarise one analyte group into a single row
#'
#' @param df the group's results, without its grouping columns
#' @param key one-row tibble of the group's grouping values
#' @param include_criteria add the guideline and exceedance columns
#' @returns a one-row tibble
#' @noRd
analyte_row <- function(df, key, include_criteria) {
  g <- group_vectors(df)
  conc <- g$conc
  detect <- g$detect
  usable <- g$usable
  prefix <- g$prefix

  # The minimum is the lowest result of the round, detected or not; the
  # maximum is the highest detection, standing in from the readable results
  # where nothing was detected.
  min_i <- pick_within(conc, usable, TRUE, which.min)
  max_i <- pick_within(conc, usable, detected_pool(detect, usable), which.max)
  at <- value_at

  out <- tibble::tibble(
    n_samples = nrow(df),
    n_detects = sum(detect),
    pct_detects = round(sum(detect) / nrow(df) * 100, 1),
    min_prefix = at(prefix, min_i, NA_character_),
    min_conc = at(conc, min_i, NA_real_),
    max_prefix = at(prefix, max_i, NA_character_),
    max_conc = at(conc, max_i, NA_real_),
    max_location = at(as.character(df$location_code), max_i, NA_character_)
  )

  if (!include_criteria) {
    return(out)
  }

  dplyr::bind_cols(
    out,
    exceedance_cells(
      df$.criteria,
      df$.exceedance,
      df$location_code,
      group_label(key)
    )
  )
}


#' The guideline value and what exceeded it, for one analyte group
#'
#' `NA` rather than `0` where the group has no guideline at all: a `0` says a
#' guideline applies and nothing exceeded it, which is a different statement
#' and the one a reader will take from the table.
#'
#' @param criteria the group's guideline values
#' @param exceedance the group's exceedance verdicts
#' @param location_code the group's locations
#' @param label the group, named in any warning
#' @returns a one-row tibble
#' @noRd
exceedance_cells <- function(criteria, exceedance, location_code, label) {
  value <- single_criteria(criteria, label)

  if (is.na(value)) {
    return(tibble::tibble(
      criteria = NA_real_,
      n_exceedances = NA_integer_,
      n_exceedance_locations = NA_integer_,
      exceedance_locations = NA_character_
    ))
  }

  hit <- !is.na(exceedance) & exceedance
  locs <- sort(unique(as.character(location_code[hit])))

  tibble::tibble(
    criteria = value,
    n_exceedances = sum(hit),
    n_exceedance_locations = length(locs),
    exceedance_locations = paste(locs, collapse = ", ")
  )
}


#' Name a group for a warning
#'
#' @param key one-row tibble of grouping values
#' @returns a single string, or NULL where there is nothing to name it by
#' @noRd
group_label <- function(key) {
  if (is.null(key) || ncol(key) == 0) {
    return(NULL)
  }
  paste(
    vapply(key, function(x) as.character(x)[[1]], character(1)),
    collapse = " / "
  )
}



#' Format a summary table for a report
#'
#' Turns a table from [analyte_summary()] - or any table sharing its column
#' names - into a formatted `gt` table ready to print in a Quarto or R Markdown
#' document. The counterpart of [create_dt()], which produces an interactive
#' table for a web page.
#'
#' Everything is decided from the column names, so no arguments are needed for
#' the common case:
#'
#' * every `<x>_prefix` / `<x>_conc` pair is merged into a single cell, so a
#'   non-detect reads `<0.05` rather than filling two columns, and a minimum
#'   and maximum are then merged into the range they span - see `merge_range`;
#' * concentration columns are formatted to `decimals` places with trailing
#'   zeros dropped, count columns (`n_*`) as integers, and percentage (`pct_*`)
#'   and ratio (`*_ratio`) columns to one place;
#' * missing values are blanked, or replaced with `missing_text`;
#' * columns are labelled from a shared dictionary - `chem_name` becomes
#'   "Analyte", `n_samples` becomes "Samples (n)", and so on - and everything
#'   is right-aligned;
#' * the monitoring round the table reports on is added as a source note,
#'   where the table carries one;
#' * the findings are marked on the cell they describe - see `highlight`.
#'
#' Read down a column of `TRUE`s and `0`s, a new maximum or an exceedance does
#' not stand out. Said on the value itself it does, so `highlight` styles the
#' cell rather than leaving the reader to find the flag that describes it:
#'
#' * a **new maximum** is bold, a **new minimum** italic and underlined, and a
#'   **spike** shaded, all on the current concentration, with a footnote saying
#'   what the shading means. The `new_max`, `new_min` and `spike` columns are
#'   then hidden, having been said - whether or not the round tripped them, so
#'   the table keeps one shape from round to round;
#' * an **exceedance** is bold, on the count or verdict itself, for every
#'   guideline set the table carries. Those columns are kept: how many exceeded
#'   is worth reading, not just that some did.
#'
#' A guideline set joined under its own `value_col` names its columns after
#' itself - `criteria_99`, `criteria_99_n_exceedances` - and those are read as
#' the set's columns and labelled as though it had been joined under the
#' default name. Where a table carries two sets - two summaries bound side by
#' side, or two sets joined onto the same results - each set's columns are put
#' under a spanner naming the set, so "Criteria" appearing twice is not
#' ambiguous. Name the sets by labelling their value columns, e.g.
#' `labels = c(criteria = "ANZG 95%", criteria_99 = "ANZG 99%")`.
#'
#' Requires the `gt` package, which is not installed with gRs.
#'
#' @param data a tibble, typically from [analyte_summary()].
#' @param decimals decimal places for concentration columns. Default 5, with
#'   trailing zeros dropped.
#' @param missing_text text shown in place of a missing value. Default `""`.
#' @param labels named character vector of column labels, e.g.
#'   `c(max_location = "Where")`, merged over the built-in dictionary. Where a
#'   prefix/concentration pair has been merged, label the `_prefix` column -
#'   that is the name the merged column keeps.
#' @param width table width passed to `gt::tab_options(table.width = )`, e.g.
#'   `1000` or `gt::pct(100)`. Default `NULL`, leaving it to `gt`.
#' @param merge_range read a `<x>min_prefix` / `<x>max_prefix` pair as the one
#'   range it describes, so `historical_range()`'s two history columns become
#'   `0.5 - 8` under "Historical Range" and `analyte_summary()`'s become
#'   "Concentration Range". Default `TRUE`. `FALSE` reports the two ends in
#'   columns of their own. Name the merged column by labelling the minimum,
#'   which is the name it keeps. Where only one end has anything to report -
#'   a group holding a single result, or one whose only history is a
#'   non-detect - the cell reads that end alone rather than trailing a
#'   separator across nothing.
#' @param highlight mark the findings on the cells they describe, and hide the
#'   flag columns that have been said. Default `TRUE`. `FALSE` leaves the table
#'   unstyled, with the flags shown as columns. Only the rules whose columns
#'   are present are applied, so this is safe on any table.
#' @param round_note add a source note naming the monitoring round the table
#'   reports on, read from the `"round"` attribute [analyte_summary()] and
#'   [historical_range()] attach. Default `TRUE`; ignored where the table
#'   carries no such attribute.
#'
#' @returns A `gt_tbl`.
#' @export
#'
#' @examples
#' \dontrun{
#' analyte_summary(compared) %>% create_gt()
#'
#' analyte_summary(compared) %>%
#'   create_gt(decimals = 3, labels = c(max_location = "Peak Location"))
#'
#' # new maximums bold, new minimums underlined, spikes shaded
#' historical_range(compared) %>% create_gt()
#'
#' # the same table with the flags left as columns and nothing styled
#' historical_range(compared) %>% create_gt(highlight = FALSE)
#'
#' # the history reported as two columns rather than one range
#' historical_range(compared) %>% create_gt(merge_range = FALSE)
#'
#' # two guideline sets bound side by side: each block under a spanner
#' dplyr::left_join(
#'   analyte_summary(compared),
#'   analyte_summary(compared, criteria_col = criteria_99),
#'   by = c(
#'     "chem_name", "output_unit", "n_samples", "n_detects", "pct_detects",
#'     "min_prefix", "min_conc", "max_prefix", "max_conc", "max_location"
#'   )
#' ) %>%
#'   create_gt(labels = c(criteria = "ANZG 95%", criteria_99 = "ANZG 99%"))
#' }
#' @seealso [create_dt()] for an interactive table, [analyte_summary()] for the
#'   table this formats.
#' @importFrom rlang check_installed
create_gt <- function(
  data,
  decimals = 5,
  missing_text = "",
  labels = NULL,
  width = NULL,
  round_note = TRUE,
  highlight = TRUE,
  merge_range = TRUE
) {
  rlang::check_installed("gt", reason = "to format a table with create_gt().")

  # A range is written into its own cell here rather than merged by gt, which
  # has no way to drop the separator on the rows that need no range: a single
  # result would read "5 - " or "5 - 5" instead of "5".
  ranges <- if (isTRUE(merge_range)) range_pairs(names(data)) else list()
  body <- data
  for (pair in ranges) {
    body <- collapse_range(body, pair, decimals)
  }

  nms <- names(body)
  numeric_cols <- nms[vapply(body, is.numeric, logical(1))]
  # `(^|_)n_` and not `^n_`, so a set joined under its own name keeps its
  # counts as counts - `criteria_99_n_exceedances` is not a concentration.
  # rank is a position, not a measurement - min_max_locations() writes it.
  count_cols <- unique(c(
    grep("(^|_)n_", numeric_cols, value = TRUE),
    intersect("rank", numeric_cols)
  ))
  pct_cols <- grep("^pct_", numeric_cols, value = TRUE)
  # A ratio is read as "about three times" - five decimal places on it is
  # noise, whatever the concentrations beside it need.
  ratio_cols <- grep("_ratio$", numeric_cols, value = TRUE)
  value_cols <- setdiff(numeric_cols, c(count_cols, pct_cols, ratio_cols))

  pairs <- prefix_value_pairs(nms)

  # gt renders a blanked cell as a line break, and cols_merge() would then put
  # that break in front of the number - every detected result reading as an
  # empty line above its own value. An absent prefix is an empty string, so it
  # is written as one here rather than left for sub_missing() to blank.
  for (pair in pairs) {
    prefix <- body[[pair[[1]]]]
    body[[pair[[1]]]] <- ifelse(is.na(prefix), "", as.character(prefix))
  }

  tbl <- gt::gt(body)

  if (length(value_cols) > 0) {
    tbl <- gt::fmt_number(
      tbl,
      columns = value_cols,
      decimals = decimals,
      drop_trailing_zeros = TRUE
    )
  }
  if (length(count_cols) > 0) {
    tbl <- gt::fmt_integer(tbl, columns = count_cols)
  }
  if (length(pct_cols) > 0) {
    tbl <- gt::fmt_number(tbl, columns = pct_cols, decimals = 1)
  }
  if (length(ratio_cols) > 0) {
    tbl <- gt::fmt_number(tbl, columns = ratio_cols, decimals = 1)
  }

  # Blanked before the merge, so a missing concentration leaves the cell empty
  # rather than merging the word "NA" onto its prefix.
  tbl <- gt::sub_missing(tbl, missing_text = missing_text)

  # The merged column keeps the prefix column's name and label.
  for (pair in pairs) {
    tbl <- gt::cols_merge(tbl, columns = pair, pattern = "{1}{2}")
  }


  stems <- criteria_stems(nms)
  spanned <- length(stems) > 1

  lab <- resolve_labels(nms, range_labels(ranges, labels), stems, spanned)
  if (length(lab) > 0) {
    tbl <- gt::cols_label(tbl, .list = as.list(lab))
  }

  # Two sets side by side both label their value column "Criteria", so the
  # set each block belongs to is said once above it rather than repeated in
  # every heading.
  if (spanned) {
    for (stem in stems) {
      tbl <- gt::tab_spanner(
        tbl,
        label = stem_label(stem, labels),
        columns = intersect(criteria_block(stem), nms)
      )
    }
  }

  tbl <- gt::cols_align(tbl, align = "right")

  if (isTRUE(highlight)) {
    marked <- apply_highlights(tbl, data, nms, stems)
    tbl <- marked$tbl
    # A flag that has been said by the styling is not said again as a column.
    if (length(marked$spent) > 0) {
      tbl <- gt::cols_hide(tbl, columns = marked$spent)
    }
  }

  if (isTRUE(round_note)) {
    note <- round_note_text(data)
    if (!is.null(note)) {
      tbl <- gt::tab_source_note(tbl, note)
    }
  }

  if (!is.null(width)) {
    tbl <- gt::tab_options(tbl, table.width = width)
  }

  tbl
}


#' Find the prefix/concentration column pairs in a table
#'
#' A pair is an `<x>_prefix` column with an `<x>_conc` column beside it, which
#' is how [analyte_summary()] reports a value that may carry a `<`.
#'
#' @param nms column names
#' @returns a list of two-element character vectors, prefix first
#' @noRd
prefix_value_pairs <- function(nms) {
  stems <- sub("_prefix$", "", grep("_prefix$", nms, value = TRUE))
  stems <- stems[paste0(stems, "_conc") %in% nms]
  lapply(stems, function(s) c(paste0(s, "_prefix"), paste0(s, "_conc")))
}


#' Column labels used by create_gt()
#'
#' Stated once here rather than in every function that builds a table.
#'
#' @param labels named character vector merged over the dictionary
#' @returns a named character vector
#' @noRd
table_labels <- function(labels = NULL) {
  out <- TABLE_LABELS
  if (!is.null(labels)) {
    labels <- stats::setNames(as.character(labels), names(labels))
    out[names(labels)] <- labels
  }
  out
}

# Both halves of a merged pair carry the same label, so the column reads the
# same whether or not the pair was merged.
TABLE_LABELS <- c(
  location_code = "Location",
  monitoring_zone = "Zone",
  monitoring_round = "Round",
  site_id = "Site",
  chem_name = "Analyte",
  chem_group = "Analyte Group",
  chem_code = "Chemical Code",
  output_unit = "Unit",
  fraction = "Fraction",
  criteria = "Criteria",
  criteria_set = "Criteria Set",
  criteria_name = "Criteria",
  criteria_unit = "Criteria Unit",
  n_samples = "Samples (n)",
  n_detects = "Detects (n)",
  n_non_detects = "Non-detects (n)",
  pct_detects = "Detects (%)",
  min_prefix = "Minimum Concentration",
  min_conc = "Minimum Concentration",
  max_prefix = "Maximum Concentration",
  max_conc = "Maximum Concentration",
  max_location = "Maximum Concentration Location",
  n_exceedances = "Exceedances (n)",
  n_exceedance_locations = "Exceeding Locations (n)",
  exceedance_locations = "Exceeding Locations",
  current_exceedance = "Exceeds Criteria",
  exceedance_ratio = "x Criteria",
  # historical_range()
  n_current = "Current Results (n)",
  hist_min_prefix = "Historical Minimum",
  hist_min_conc = "Historical Minimum",
  hist_max_prefix = "Historical Maximum",
  hist_max_conc = "Historical Maximum",
  current_prefix = "Current Concentration",
  current_conc = "Current Concentration",
  new_max = "New Maximum",
  new_min = "New Minimum",
  max_ratio = "x Previous Maximum",
  spike = "Spike",
  # min_max_locations()
  extreme = "Extreme",
  rank = "Rank"
)


# The columns a guideline set writes beside its value column. A set joined
# under the default name writes them unprefixed - `n_exceedances`, not
# `criteria_n_exceedances` - so the default set's block is the bare names and
# every other set's is those names prefixed with the name it was joined under.
CRITERIA_SUFFIXES <- c(
  "n_exceedances",
  "n_exceedance_locations",
  "exceedance_locations",
  "current_exceedance",
  "exceedance_ratio",
  "exceedance",
  "lor_above_criteria"
)


#' Find the guideline sets a summary table carries
#'
#' A set is a value column with at least one of the columns
#' [analyte_summary()] and [historical_range()] write beside it. The default
#' set is `criteria` with those columns unprefixed; every other set is a column
#' carrying them prefixed with its own name, as `rename_criteria_columns()`
#' leaves them.
#'
#' Matching on the columns beside it, rather than on the name alone, keeps a
#' `criteria` column added by hand - carrying no counts - from being read as a
#' set and spanned over nothing.
#'
#' @param nms column names
#' @returns character vector of value column names, the default set first
#' @noRd
criteria_stems <- function(nms) {
  named <- setdiff(nms, "criteria")
  named <- named[vapply(
    named,
    function(s) any(paste0(s, "_", CRITERIA_SUFFIXES) %in% nms),
    logical(1)
  )]
  default <- "criteria" %in% nms && any(CRITERIA_SUFFIXES %in% nms)
  c(if (default) "criteria", named)
}


#' The columns belonging to one guideline set
#'
#' @param stem the set's value column name
#' @returns character vector of column names, present or not
#' @noRd
criteria_block <- function(stem) {
  if (identical(stem, "criteria")) {
    c("criteria", CRITERIA_SUFFIXES)
  } else {
    c(stem, paste0(stem, "_", CRITERIA_SUFFIXES))
  }
}


#' Label every column of a table
#'
#' A set joined under its own name is labelled as though it had been joined
#' under the default one - `criteria_99_n_exceedances` reads "Exceedances (n)"
#' - because which set it belongs to is said by the spanner above it rather
#' than repeated in every heading.
#'
#' @param nms column names
#' @param labels named character vector supplied by the caller
#' @param stems the guideline sets present
#' @param spanned whether the sets are being spanned, in which case a label
#'   given for a set's value column names the spanner instead of the column
#' @returns a named character vector, holding only the columns it could label
#' @noRd
resolve_labels <- function(nms, labels, stems, spanned) {
  dict <- table_labels(labels)
  named <- setdiff(stems, "criteria")

  out <- vapply(
    nms,
    function(nm) {
      if (nm %in% stems) {
        # The set's own name is carried by the spanner, so the column beneath
        # it says what the value is rather than repeating which set it is -
        # including the default set, which would otherwise be the one heading
        # to name itself twice.
        if (!spanned && nm %in% names(dict)) {
          return(dict[[nm]])
        }
        return(TABLE_LABELS[["criteria"]])
      }
      if (nm %in% names(dict)) {
        return(dict[[nm]])
      }
      stem <- named[startsWith(nm, paste0(named, "_"))]
      if (length(stem) == 0) {
        return(NA_character_)
      }
      # The longest match, so a set named as another's prefix is not stripped
      # back to the shorter one.
      stem <- stem[[which.max(nchar(stem))]]
      suffix <- substring(nm, nchar(stem) + 2)
      if (suffix %in% names(TABLE_LABELS)) {
        TABLE_LABELS[[suffix]]
      } else {
        NA_character_
      }
    },
    character(1)
  )

  out[!is.na(out)]
}


#' Name a guideline set for its spanner
#'
#' @param stem the set's value column name
#' @param labels named character vector supplied by the caller
#' @returns a single string
#' @noRd
stem_label <- function(stem, labels) {
  if (!is.null(labels) && stem %in% names(labels)) {
    return(as.character(labels[[stem]])[[1]])
  }
  humanise(stem)
}


#' Turn a column name into something to print
#'
#' Sentence case, not title case: only the first word is capitalised, and only
#' where it does not already carry a capital of its own - so a set named after
#' a guideline keeps the guideline's own capitalisation.
#'
#' @param x a single column name
#' @returns a single string
#' @noRd
humanise <- function(x) {
  parts <- strsplit(as.character(x)[[1]], "_", fixed = TRUE)[[1]]
  if (length(parts) == 0) {
    return("")
  }
  if (grepl("^[a-z]", parts[[1]])) {
    parts[[1]] <- paste0(
      toupper(substring(parts[[1]], 1, 1)),
      substring(parts[[1]], 2)
    )
  }
  paste(parts, collapse = " ")
}


#' Name the round a summary table reports on
#'
#' [analyte_summary()] and [historical_range()] resolve the round and attach
#' it, but a printed table has not said which round it describes. The column
#' the round was read from is named too, since a table reporting on a date is
#' saying something different from one reporting on a monitoring round.
#'
#' @param data the summary table
#' @returns a single string, or NULL where the table carries no round
#' @noRd
round_note_text <- function(data) {
  info <- attr(data, "round")
  if (!is.list(info) || is.null(info$round) || length(info$round) == 0) {
    return(NULL)
  }
  value <- info$round[[1]]
  if (is.na(value)) {
    return(NULL)
  }
  column <- if (is.null(info$column)) {
    "round"
  } else {
    as.character(info$column)[[1]]
  }
  paste0(humanise(column), ": ", format(value))
}


#' Mark the findings a reader is scanning for
#'
#' [historical_range()] reports a new maximum, a new minimum and a spike as
#' logical columns, and [analyte_summary()] reports exceedances as a count.
#' Read down a column of `TRUE`s and `0`s, none of those stand out; said on the
#' value itself they do. So the flags style the cell they describe and are then
#' hidden, which is the shape the table was drafted in.
#'
#' A guideline set joined under its own name has its own exceedance columns, so
#' the emphasis follows each set rather than only the default one.
#'
#' @param tbl the `gt` table so far
#' @param data the table it was built from, holding the flags
#' @param nms `data`'s column names
#' @param stems the guideline sets present
#' @returns list(tbl, spent); `spent` names the flag columns the styling has
#'   made redundant
#' @noRd
apply_highlights <- function(tbl, data, nms, stems) {
  spent <- character(0)

  mark <- function(tbl, column, rows, style) {
    if (length(rows) == 0) {
      return(tbl)
    }
    gt::tab_style(
      tbl,
      style = style,
      locations = gt::cells_body(columns = column, rows = rows)
    )
  }
  true_rows <- function(column) {
    if (!column %in% nms) {
      return(integer(0))
    }
    v <- suppressWarnings(as.logical(data[[column]]))
    which(!is.na(v) & v)
  }
  over_zero <- function(column) {
    v <- suppressWarnings(as.numeric(data[[column]]))
    which(!is.na(v) & v > 0)
  }

  # historical_range(): the current result is the cell every flag describes.
  # Styling it needs the column to have survived the prefix/concentration
  # merge, which is why the prefix half is the one addressed.
  if ("current_prefix" %in% nms) {
    tbl <- mark(
      tbl,
      "current_prefix",
      true_rows("new_max"),
      gt::cell_text(weight = "bold")
    )
    tbl <- mark(
      tbl,
      "current_prefix",
      true_rows("new_min"),
      gt::cell_text(style = "italic", decorate = "underline")
    )
    tbl <- mark(
      tbl,
      "current_prefix",
      true_rows("spike"),
      gt::cell_fill(color = "#f8d7da")
    )
    # Hidden whether or not this round happened to trip them, so the table
    # keeps one shape across rounds.
    spent <- intersect(c("new_max", "new_min", "spike"), nms)

    if (length(true_rows("spike")) > 0) {
      tbl <- gt::tab_footnote(
        tbl,
        footnote = spike_footnote(data),
        locations = gt::cells_column_labels(columns = "current_prefix")
      )
    }
  }

  # The exceedance columns of every set: a count above zero, or a verdict of
  # its own where the table carries one instead.
  counts <- exceedance_columns(nms, stems, "n_exceedances")
  for (column in counts) {
    tbl <- mark(tbl, column, over_zero(column), gt::cell_text(weight = "bold"))
  }
  verdicts <- exceedance_columns(nms, stems, "current_exceedance")
  for (column in verdicts) {
    tbl <- mark(tbl, column, true_rows(column), gt::cell_text(weight = "bold"))
  }

  list(tbl = tbl, spent = spent)
}


#' Name one of every guideline set's columns
#'
#' @param nms column names
#' @param stems the guideline sets present
#' @param suffix the column wanted, as the default set names it
#' @returns the columns present, one per set
#' @noRd
exceedance_columns <- function(nms, stems, suffix) {
  cols <- vapply(
    stems,
    function(stem) {
      if (identical(stem, "criteria")) suffix else paste0(stem, "_", suffix)
    },
    character(1)
  )
  unname(intersect(cols, nms))
}


#' Say what the shading means
#'
#' The factor is read from the attribute [historical_range()] attaches, so the
#' note cannot disagree with the check that set the flag.
#'
#' @param data the summary table
#' @returns a single string
#' @noRd
spike_footnote <- function(data) {
  factor <- attr(data, "spike_factor")
  paste0(
    "Shaded: a new maximum",
    if (is.null(factor) || length(factor) == 0 || is.na(factor[[1]])) {
      ""
    } else {
      paste0(" more than ", factor[[1]], "x the previous detected maximum")
    },
    " - verify against the lab report."
  )
}


#' Find the minimum/maximum column pairs in a table
#'
#' A pair is a `<x>min_prefix` column with a `<x>max_prefix` column beside it,
#' which is how [analyte_summary()] and [historical_range()] report the two
#' ends of a range. The prefix halves are the ones addressed because they are
#' what survives the prefix/concentration merge, carrying the value with them.
#'
#' @param nms column names
#' @returns a list of two-element character vectors, minimum first
#' @noRd
range_pairs <- function(nms) {
  stems <- sub("min_prefix$", "", grep("min_prefix$", nms, value = TRUE))
  stems <- stems[paste0(stems, "max_prefix") %in% nms]
  lapply(stems, function(s) c(paste0(s, "min_prefix"), paste0(s, "max_prefix")))
}


#' Write a minimum and a maximum into the one range they describe
#'
#' Done in the data because `gt::cols_merge_range()` writes its separator on
#' every row, whether or not there is a second value to put after it. A group
#' holding one result would read "5 - 5", and one whose only history is a
#' non-detect - which has a minimum but no detected maximum to report - would
#' read "5 - ". Both say the range is wider than the record shows.
#'
#' So the two ends are rendered here and compared as they will be read: where
#' they say the same thing, one of them is enough.
#'
#' @param body the table being formatted
#' @param pair the minimum and maximum prefix columns
#' @param decimals decimal places for the concentrations
#' @returns `body`, with the range in the minimum's column and the three
#'   columns it consumed dropped
#' @noRd
collapse_range <- function(body, pair, decimals) {
  concs <- sub("_prefix$", "_conc", pair)
  if (!all(concs %in% names(body))) {
    return(body)
  }

  side <- function(prefix_col, conc_col) {
    prefix <- body[[prefix_col]]
    prefix <- ifelse(is.na(prefix), "", as.character(prefix))
    value <- format_concentration(body[[conc_col]], decimals)
    ifelse(is.na(value), NA_character_, paste0(prefix, value))
  }
  low <- side(pair[[1]], concs[[1]])
  high <- side(pair[[2]], concs[[2]])

  # The two ends reading the same is one value, not a range across nothing.
  both <- !is.na(low) & !is.na(high) & low != high
  body[[pair[[1]]]] <- ifelse(
    both,
    paste0(low, " - ", high),
    ifelse(!is.na(low), low, ifelse(!is.na(high), high, ""))
  )
  body[, setdiff(names(body), c(pair[[2]], concs)), drop = FALSE]
}


#' Print a concentration the way gt would
#'
#' Matches `gt::fmt_number(decimals =, drop_trailing_zeros = TRUE)`, which the
#' concentrations outside a range are still formatted by, so a range does not
#' read differently from the columns beside it.
#'
#' @param x concentrations
#' @param decimals decimal places
#' @returns a character vector, `NA` where `x` is
#' @noRd
format_concentration <- function(x, decimals) {
  x <- suppressWarnings(as.numeric(x))
  out <- formatC(x, format = "f", digits = decimals, big.mark = ",")
  # Trailing zeros only after the decimal point - "1,000" keeps its own.
  out <- sub("(\\.[0-9]*[1-9])0+$", "\\1", out)
  out <- sub("\\.0*$", "", out)
  out[is.na(x)] <- NA_character_
  out
}


#' Label a merged range for what it now holds
#'
#' The merged column keeps the minimum's name, so left alone a range spanning
#' both ends would be headed "Minimum Concentration". The caller's own labels
#' still win, so a table can name its range whatever it needs to.
#'
#' @param ranges the range pairs being merged
#' @param labels named character vector supplied by the caller
#' @returns `labels`, with a label for each merged range added
#' @noRd
range_labels <- function(ranges, labels) {
  if (length(ranges) == 0) {
    return(labels)
  }
  begins <- vapply(ranges, function(pair) pair[[1]], character(1))
  added <- vapply(
    begins,
    function(nm) {
      if (nm %in% names(RANGE_LABELS)) {
        RANGE_LABELS[[nm]]
      } else {
        trimws(paste(humanise(sub("_?min_prefix$", "", nm)), "Range"))
      }
    },
    character(1)
  )
  c(added[setdiff(names(added), names(labels))], labels)
}

# What a merged minimum/maximum pair is called once it holds both ends.
RANGE_LABELS <- c(
  min_prefix = "Concentration Range",
  hist_min_prefix = "Historical Range"
)

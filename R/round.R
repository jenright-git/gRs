# ---------------------------------------------------------------------------
# Monitoring round resolution
#
# The reporting functions all ask the same question of a chemistry table:
# which of these results belong to the round being reported on? A chemistry
# export may or may not carry a column naming the round - ESDAT writes
# `monitoring_round`, EQuIS writes `task_code`, and plenty of exports carry
# neither - so the column is found rather than assumed, and the round itself
# defaults to the latest one present.
# ---------------------------------------------------------------------------

# Columns that can name a monitoring round, best first. The date columns are
# last: a date is a poor stand-in for a round, since a round spans several
# days, but it is better than refusing to summarise a table that has no round
# column at all.
ROUND_COLUMNS <- c(
  "monitoring_round",
  "task_code",
  "date",
  "sampled_date_time"
)

#' Fold a round column into the form the round is compared in
#'
#' A datetime is compared as a calendar date, so a round given as `date`
#' collects every result sampled that day rather than only those sharing a
#' timestamp to the second. A factor is compared as text.
#'
#' @param x the round column
#' @noRd
round_values <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (is.factor(x)) {
    return(as.character(x))
  }
  x
}

#' Find the date column a character round column can be ordered by
#'
#' @param data chemistry tibble
#' @param round_col name of the round column, excluded from the search
#' @returns a Date/POSIXct vector, or NULL
#' @noRd
round_ordering_dates <- function(data, round_col) {
  for (nm in setdiff(c("date", "sampled_date_time"), round_col)) {
    if (nm %in% names(data) && inherits(data[[nm]], c("Date", "POSIXt"))) {
      return(data[[nm]])
    }
  }
  NULL
}

#' Pick the latest round present
#'
#' A round recorded as a date or a number orders itself. One recorded as text
#' does not - `"2025 Q10"` sorts below `"2025 Q9"` - so it is ordered by the
#' latest date sampled within it, and only falls back to sorting the text when
#' the table carries no date at all.
#'
#' @param values the folded round column
#' @param data chemistry tibble
#' @param round_col name of the round column
#' @returns a single round value
#' @noRd
latest_round <- function(values, data, round_col) {
  if (inherits(values, "Date") || is.numeric(values)) {
    if (all(is.na(values))) {
      stop("`", round_col, "` holds no readable value, so no round can be ",
           "found in `data`. Name a round column with `round_col`.")
    }
    return(max(values, na.rm = TRUE))
  }

  chr <- as.character(values)
  known <- !is.na(chr)
  if (!any(known)) {
    stop("`", round_col, "` holds no readable value, so no round can be ",
         "found in `data`. Name a round column with `round_col`.")
  }

  dates <- round_ordering_dates(data, round_col)
  if (!is.null(dates)) {
    per_round <- tapply(as.numeric(dates[known]), chr[known], max, na.rm = TRUE)
    per_round <- per_round[is.finite(per_round)]
    if (length(per_round) > 0) {
      return(names(per_round)[[which.max(per_round)]])
    }
  }

  # Nothing to order the rounds by but their own text, which is a guess rather
  # than a reading of the data - so say so.
  levels <- sort(unique(chr[known]))
  warning(
    "`", round_col, "` is text and `data` carries no date to order its ",
    "rounds by, so the last in alphabetical order (\"", levels[[length(levels)]],
    "\") was taken as the latest. Name the round with `round`."
  )
  levels[[length(levels)]]
}

#' Resolve which results belong to the round being reported on
#'
#' Shared by the reporting functions so that every table in a report agrees on
#' what "this round" means.
#'
#' @param data chemistry tibble from [data_processor()]
#' @param round_col name of the round column, or NULL to find one
#' @param round the round to report on, or NULL for the latest present
#' @param quiet suppress the message naming the round that was picked
#' @returns list(col, value, is_current)
#' @noRd
resolve_round <- function(data, round_col = NULL, round = NULL, quiet = FALSE) {
  if (is.null(round_col)) {
    found <- intersect(ROUND_COLUMNS, names(data))
    if (length(found) == 0) {
      stop(
        "`data` carries no column naming a monitoring round. Looked for: ",
        paste(ROUND_COLUMNS, collapse = ", "),
        ". Name the column holding the round with `round_col`."
      )
    }
    round_col <- found[[1]]
  } else {
    round_col <- as.character(round_col)[[1]]
    if (!round_col %in% names(data)) {
      stop(
        "`data` has no '", round_col, "' column. Columns present: ",
        paste(utils::head(names(data), 20), collapse = ", "),
        if (length(names(data)) > 20) ", ..." else ""
      )
    }
  }

  values <- round_values(data[[round_col]])

  picked <- is.null(round)
  if (picked) {
    round <- latest_round(values, data, round_col)
    if (!quiet) {
      message(
        "Reporting on the latest round in `data`: ",
        round_col, " = ", format(round), "."
      )
    }
  } else {
    round <- round_values(round)[[1]]
  }

  is_current <- !is.na(values) & values == round
  if (!any(is_current)) {
    stop(
      "No result in `data` belongs to ", round_col, " = ", format(round), ". ",
      "Rounds present: ",
      paste(utils::head(sort(unique(as.character(values))), 10),
            collapse = ", "),
      "."
    )
  }

  list(col = round_col, value = round, is_current = is_current)
}

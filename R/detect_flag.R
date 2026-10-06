#' Derive a detect flag from a result prefix
#'
#' [data_processor()] is the one place a flag is made from `prefix`, for the
#' exports (ESDAT's) that carry no detect flag of their own; everything after
#' it reads `detect_flag`. Only a `"<"` marks a non-detect. A missing prefix
#' is an ordinary detect, and so is a `">"` - a result above the top of the
#' lab's range, such as `>2000` for coliforms, was detected.
#'
#' @param prefix the prefix of each result
#' @returns `"N"` where the prefix is `"<"`, otherwise `"Y"`
#' @noRd
detect_flag_from_prefix <- function(prefix) {
  ifelse(as.character(prefix) %in% "<", "N", "Y")
}

#' Which results were detected
#'
#' Only a `"Y"` is a detect, as [join_action_levels()] decides it. A result
#' with no flag is not counted as detected.
#'
#' @param detect_flag the `detect_flag` of each result
#' @returns a logical vector, never `NA`
#' @noRd
is_detect <- function(detect_flag) {
  as.character(detect_flag) %in% "Y"
}

#' The prefix to show beside a result
#'
#' Summaries write `"<"` beside a non-detect, read from `detect_flag` rather
#' than passed through from `prefix`, which a raw EQuIS export fills with
#' `"="` for every detect. A detect shows nothing, unless its `prefix` is
#' `">"`: a result above the top of the lab's range, such as `>2000` for
#' coliforms, is reported as more than its number.
#'
#' @param detect_flag the `detect_flag` of each result
#' @param prefix the `prefix` of each result, or `NULL` where there is none
#' @returns `"<"` for a non-detect, `">"` for a detect above the lab's range,
#'   otherwise `NA`
#' @noRd
display_prefix <- function(detect_flag, prefix = NULL) {
  detected <- is_detect(detect_flag)
  shown <- ifelse(detected, NA_character_, "<")
  if (!is.null(prefix)) {
    shown[detected & as.character(prefix) %in% ">"] <- ">"
  }
  shown
}

#' Stop where a table carries no detect flag
#'
#' @param data the table a function was given
#' @param fn the function's name, for the message
#' @noRd
check_detect_flag <- function(data, fn) {
  if (!"detect_flag" %in% names(data)) {
    stop(
      "`data` has no `detect_flag` column, so a detected result cannot be ",
      "told from a non-detect. Read the results in with data_processor(), ",
      "which adds it, before calling ",
      fn,
      "().",
      call. = FALSE
    )
  }
  invisible(data)
}

# ---------------------------------------------------------------------------
# Reading a spreadsheet export
#
# data_processor() and action_level_processor() both identify their sheet by
# its contents rather than its name, and both normalise the column names they
# find to the package's canonical set. Those mechanics live here rather than
# inside either reader, because neither owns them.
# ---------------------------------------------------------------------------

#' Read only the header row of a worksheet
#'
#' @param myfile_path path to the workbook
#' @param sheet sheet name
#' @param skip rows to skip before the header
#' @returns character vector of cleaned column names, or NULL if unreadable
#' @noRd
peek_names <- function(myfile_path, sheet, skip = 0) {
  peek <- tryCatch(
    suppressMessages(suppressWarnings(
      readxl::read_excel(myfile_path, sheet = sheet, skip = skip, n_max = 0)
    )),
    error = function(e) NULL
  )
  if (is.null(peek) || ncol(peek) == 0) {
    return(NULL)
  }
  # AR2 detection matches on the raw uppercase names, so return both forms.
  # Peeking with an offset of 1 can turn a row of data into the header, so
  # janitor is silenced here - it has nothing useful to say about a candidate
  # that is about to be rejected anyway.
  c(names(peek), names(suppressWarnings(janitor::clean_names(peek))))
}


#' Resolve a vector of column names to their canonical equivalents
#'
#' @param nms character vector of (cleaned) column names
#' @param alias_map named list: canonical_name -> character vector of aliases
#' @returns character vector of canonical names present in `nms`
#' @noRd
canonical_names <- function(nms, alias_map) {
  resolved <- nms
  for (canonical in names(alias_map)) {
    if (canonical %in% nms) {
      next
    }
    if (any(alias_map[[canonical]] %in% nms)) {
      resolved <- c(resolved, canonical)
    }
  }
  unique(resolved)
}


#' Rename columns to canonical names based on an alias map
#'
#' @param df data frame to normalise
#' @param alias_map named list: canonical_name -> character vector of known aliases
#' @returns df with columns renamed to canonical names where a match is found
#' @noRd
resolve_columns <- function(df, alias_map) {
  current_names <- names(df)
  for (canonical in names(alias_map)) {
    if (canonical %in% current_names) {
      next
    }
    matched <- intersect(alias_map[[canonical]], current_names)
    if (length(matched) > 1) {
      warning(
        "Multiple columns match canonical '",
        canonical,
        "': ",
        paste(matched, collapse = ", "),
        ". Using '",
        matched[[1]],
        "'."
      )
    }
    if (length(matched) >= 1) {
      df <- dplyr::rename(df, !!canonical := !!matched[[1]])
    }
  }
  df
}


#' Find the first sheet in a workbook whose columns a reader recognises
#'
#' Sheets are identified by their contents rather than by their names, so the
#' ESDAT matrix-specific exports, the EQuIS reports on an unnamed `Sheet1`,
#' and the gauging and guideline exports are all found without listing each
#' one. Every candidate is peeked at with a header offset of 0 and then 1,
#' because the `dav` exports and the ESDAT gauging reports carry a source/URL
#' banner on the first row.
#'
#' @param myfile_path path to the workbook
#' @param candidates sheet names to try, in order of preference
#' @param alias_map the alias dictionary the reader normalises against
#' @param accepts a function of the canonical names, returning `TRUE` for a
#'   sheet this reader can read
#' @param by_candidate try each candidate at every offset before moving to the
#'   next, rather than every candidate at each offset in turn. `TRUE` for the
#'   chemistry reader, whose candidates are ordered - the sheets matching
#'   `sheet_pattern` first - so the preferred sheet should win even when it
#'   needs the banner row skipped and a later sheet would not. `FALSE`, the
#'   default, for the readers whose candidates carry no preference.
#' @returns list(sheet, skip); `sheet` is `NULL` when nothing matched
#' @noRd
locate_sheet <- function(
  myfile_path,
  candidates,
  alias_map,
  accepts,
  by_candidate = FALSE
) {
  skips <- c(0, 1)
  pairs <- if (by_candidate) {
    expand.grid(skip = skips, sheet = candidates, stringsAsFactors = FALSE)
  } else {
    expand.grid(sheet = candidates, skip = skips, stringsAsFactors = FALSE)
  }

  for (i in seq_len(nrow(pairs))) {
    s <- pairs$sheet[[i]]
    skip <- pairs$skip[[i]]
    peek <- peek_names(myfile_path, s, skip = skip)
    if (is.null(peek)) {
      next
    }
    if (isTRUE(accepts(canonical_names(peek, alias_map)))) {
      return(list(sheet = s, skip = skip))
    }
  }
  list(sheet = NULL, skip = NULL)
}

#' Read an ESDAT action level (guideline) export
#'
#' ESDAT exports a set of action levels as a flat table of one row per
#' analyte, e.g. the ANZG marine water toxicant default guideline values.
#' The export carries no column naming the guideline set itself, so the name
#' is supplied by `name` (or taken from the file name).
#'
#' The exported value and its unit share one cell (`"0.6 µg/L"`), so they are
#' split into `criteria` and `criteria_unit`. Where the unit records the
#' basis of measurement - `"0.006 µg Sn/L"` for tributyltin as tin, `"as N"`
#' for ammonia - the basis is stripped into `criteria_basis` and the unit is
#' left as the plain concentration unit so it can be converted.
#'
#' `leached`, `total` and `filtered` record which result types the guideline
#' applies to, and [join_action_levels()] uses them to decide whether a given
#' result is comparable: a filtered (dissolved) result is only compared
#' against a guideline with `filtered = TRUE`, a leachate result (`REG` vs
#' `LEACHED_REG` in an ESDAT soil export) only against `leached = TRUE`.
#'
#' @param myfile_path file path to an action level export. More than one path
#'   may be given, in which case the results are stacked.
#' @param name name for the guideline set, written to `criteria_name`, e.g.
#'   `"ANZG 95% Marine"`. Defaults to the file name without its extension.
#'   Recycled against `myfile_path`.
#' @param sheet optional sheet name. By default the sheet is found from its
#'   contents, with and without a leading banner row.
#'
#' @returns A tibble with one row per action level and the columns
#'   `criteria_name`, `chem_code`, `chem_name`, `matrix_code`, `criteria`,
#'   `criteria_unit`, `criteria_basis`, `criteria_text`, `leached`, `total`,
#'   `filtered`, `conditions` and `comments`, carrying a `"report_type"`
#'   attribute of `"action_level"`.
#' @export
#' @examples
#' \dontrun{
#' anzg <- action_level_processor(
#'   "ANZG Marine Water Toxicant DGVs LOSP 95_ (March 2026).xlsx",
#'   name = "ANZG 95% Marine"
#' )
#'
#' # several guideline sets stacked, then used one at a time
#' levels <- action_level_processor(
#'   c("ANZG 95.xlsx", "ANZG 99.xlsx"),
#'   name = c("ANZG 95%", "ANZG 99%")
#' )
#' }
#' @importFrom dplyr filter relocate bind_rows any_of
#' @importFrom readxl excel_sheets read_excel
#' @importFrom janitor clean_names
action_level_processor <- function(myfile_path, name = NULL, sheet = NULL) {
  if (length(myfile_path) > 1) {
    if (is.null(name)) {
      name <- rep(list(NULL), length(myfile_path))
    } else {
      name <- rep_len(as.list(name), length(myfile_path))
    }
    out <- dplyr::bind_rows(Map(
      function(p, n) action_level_processor(p, name = n, sheet = sheet),
      myfile_path,
      name
    ))
    attr(out, "report_type") <- "action_level"
    return(out)
  }

  if (!file.exists(myfile_path)) {
    stop("File does not exist: ", myfile_path)
  }

  if (length(name) > 1) {
    stop(
      "`name` holds ", length(name), " names but `myfile_path` is one file. ",
      "Give one name per file."
    )
  }

  if (is.null(name) || is.na(name) || !nzchar(name)) {
    name <- sub("\\.[^.]*$", "", basename(myfile_path))
  }

  all_sheets <- readxl::excel_sheets(myfile_path)
  target <- if (is.null(sheet)) {
    locate_action_level_sheet(myfile_path, all_sheets)
  } else {
    list(sheet = sheet, skip = 0)
  }

  if (is.null(target$sheet)) {
    warning(
      "No action level sheet found in '",
      basename(myfile_path),
      "'.\n- action level: no sheet carrying a chemical column and an action ",
      "level column (or a recognised alias of each).",
      "\nSheets present: ",
      paste(all_sheets, collapse = ", ")
    )
    return(NULL)
  }

  raw <- suppressMessages(readxl::read_excel(
    myfile_path,
    sheet = target$sheet,
    skip = target$skip
  ))

  out <- process_action_levels(raw, name = name)
  attr(out, "report_type") <- "action_level"
  out
}


#' Join action levels onto a chemistry table
#'
#' Adds a guideline value to every result in a chemistry table returned by
#' [data_processor()], converted into the unit each result is reported in, so
#' concentrations and guidelines can be compared directly. The added
#' `criteria` column is the one [summary_stats()] reads when
#' `include_criteria = TRUE` and the one [timeseries_plot()] draws when passed
#' as `criteria_col`.
#'
#' Analytes are matched on `chem_code` - the CAS number - which is distinct
#' and unique per analyte and is never reduced, stripped or otherwise
#' collapsed to force a join. A code carrying a disambiguating suffix names a
#' different analyte from the bare code, so `91-20-3` (naphthalene by SVOC)
#' and `91-20-3_VOC` (naphthalene by VOC) keep their separate guidelines and
#' neither stands in for the other. A code that finds no guideline is reported
#' as unmatched rather than being trimmed until it matches something.
#'
#' Where a result carries no code at all, `chem_name` is tried instead - case-
#' and whitespace-insensitively, ignoring the `Dissolved` prefix that
#' [data_processor()] adds to filtered results. This is what matches an EQuIS
#' export reporting names against codes the guideline set does not share. Pass
#' `match_name = FALSE` to match on the code alone.
#'
#' The report printed at the end says how many results matched on each.
#'
#' A result is only compared against a guideline that applies to it:
#'
#' * **Matrix** - `matrix_code` must agree, after normalising the ESDAT and
#'   EQuIS spellings (`WATER`/`Water`, `Soil`/`SEDIMENT`). A matrix left
#'   unrecorded on either side matches anything, so a guideline set carrying
#'   no `matrix_code` at all is warned about rather than quietly compared
#'   across matrices.
#' * **Fraction** - a filtered (dissolved) result needs `filtered = TRUE`, a
#'   total result needs `total = TRUE`. A result whose fraction is unrecorded
#'   (`NA`, or EQuIS's `"N"`) accepts either.
#' * **Leachate** - a `LEACHED_REG` result needs `leached = TRUE`, and an
#'   ordinary result needs `leached = FALSE`.
#' * **Units** - the guideline is converted into the result's own unit.
#'   Conversion is only attempted within a dimension (concentration in a
#'   liquid, concentration in a solid); `ppm` and `ppb` are read against
#'   whichever dimension the other unit belongs to. A guideline that cannot be
#'   converted is dropped rather than compared, and reported.
#'
#' A guideline carrying no unit at all is never joined and never assumed to
#' share the result's unit - the unit is missing from the source file and that
#' is a fault to be fixed there, not guessed at here. It is reported apart
#' from the guidelines that carried a unit that would not convert, since the
#' two call for different fixes. A guideline set with no units anywhere is an
#' error rather than a table of empty columns.
#'
#' Where more than one guideline still applies to a result, the lowest is
#' used and the ambiguity is reported.
#'
#' Only a detected result exceeds a guideline. A non-detect reported above the
#' guideline - an LOR too high to demonstrate compliance - is recorded
#' separately in `lor_above_criteria`, and counts as an exceedance only where
#' `lor_as_exceedance = TRUE`. That choice is made here, once, so that every
#' function reading `exceedance` afterwards agrees with it.
#'
#' @param chem_data chemistry tibble from [data_processor()].
#' @param action_levels action level tibble from [action_level_processor()].
#'   Must hold a single guideline set; filter it or call this function once
#'   per set (with a different `value_col`) to compare several.
#' @param value_col name of the column the guideline value is written to.
#'   Default `"criteria"`, which is what the rest of the package expects.
#'   Give a different name to hold a second guideline set alongside the first.
#' @param match_matrix compare `matrix_code` before matching. Default `TRUE`.
#' @param match_fraction compare fraction and leachate status before matching.
#'   Default `TRUE`.
#' @param match_name fall back to matching on `chem_name` where `chem_code`
#'   found nothing. Default `TRUE`.
#' @param lor_as_exceedance count a non-detect whose limit of reporting sits
#'   above the guideline as an exceedance. Default `FALSE` - a non-detect says
#'   the analyte was not seen, not that it was present at the LOR. Set `TRUE`
#'   where an LOR above the guideline is a failure in its own right. Either
#'   way the two are told apart by `lor_above_criteria`.
#' @param quiet suppress the matching report. Default `FALSE`.
#'
#' @returns `chem_data` with the guideline columns added: `<value_col>` (the
#'   guideline in the result's own unit), `<value_col>_name`,
#'   `<value_col>_unit`, `<value_col>_basis`, and the comparison columns
#'   `exceedance_ratio`, `exceedance` and `lor_above_criteria`. Joining a
#'   second set under a different `value_col` prefixes its comparison columns
#'   with that name (`criteria_99_exceedance` and so on) so the two sets sit
#'   side by side without overwriting each other; [criteria_long()] stacks
#'   them into one column. Row count and order are unchanged. The analytes
#'   that found no guideline are attached as an `"unmatched"` attribute.
#' @export
#' @examples
#' \dontrun{
#' chem <- data_processor("davLChem1_Chemistry.xlsx")
#' anzg <- action_level_processor("ANZG 95 Marine.xlsx", name = "ANZG 95%")
#'
#' compared <- join_action_levels(chem, anzg)
#'
#' # what did not match
#' attr(compared, "unmatched")
#'
#' # feeds straight into the rest of the package
#' summary_stats(compared, include_criteria = TRUE)
#' timeseries_plot(compared, criteria_col = criteria)
#'
#' # a second guideline set alongside the first, then stacked for analysis
#' compared <- compared %>%
#'   join_action_levels(anzg_99, value_col = "criteria_99")
#'
#' long <- criteria_long(compared)
#' }
#' @seealso [criteria_long()] to stack several joined sets into one column.
#' @importFrom dplyr inner_join
join_action_levels <- function(
  chem_data,
  action_levels,
  value_col = "criteria",
  match_matrix = TRUE,
  match_fraction = TRUE,
  match_name = TRUE,
  lor_as_exceedance = FALSE,
  quiet = FALSE
) {
  if (is.null(chem_data)) {
    return(NULL)
  }
  value_name <- as.character(value_col)[[1]]
  if (is.null(action_levels) || nrow(action_levels) == 0) {
    stop("`action_levels` is empty. Read one with action_level_processor().")
  }

  missing <- setdiff(c("chem_name", "concentration"), names(chem_data))
  if (length(missing) > 0) {
    stop(
      "`chem_data` is missing required columns: ",
      paste(missing, collapse = ", "),
      ". Pass a table from data_processor()."
    )
  }
  if (!"criteria" %in% names(action_levels)) {
    stop(
      "`action_levels` has no 'criteria' column. Pass a table from ",
      "action_level_processor()."
    )
  }

  # A guideline with no unit cannot be compared against anything, and guessing
  # that it shares the result's unit is how a mg/kg number ends up drawn
  # against a µg/L result. Where the whole set is unitless nothing could join,
  # so say so here rather than returning a table of empty columns.
  if (
    !"criteria_unit" %in% names(action_levels) ||
      all(is.na(action_levels$criteria_unit))
  ) {
    stop(
      "`action_levels` records no units, so no guideline can be compared ",
      "against a result. Add the unit to the guideline file - either in the ",
      "action level cell (\"80 µg/L\") or in a units column - and read ",
      "it again with action_level_processor()."
    )
  }

  sets <- unique(stats::na.omit(action_levels$criteria_name))
  if (length(sets) > 1) {
    stop(
      "`action_levels` holds more than one guideline set (",
      paste(sets, collapse = ", "),
      "). Join one at a time, using a different `value_col` for each."
    )
  }
  set_name <- if (length(sets) == 1) sets else NA_character_

  # An empty table is a normal outcome of filtering, and the rest of the
  # package selects the guideline columns by name - summary_stats() errors on
  # a missing `criteria`. So the columns are added rather than the table
  # being handed straight back without them.
  if (nrow(chem_data) == 0) {
    out <- empty_criteria_columns(chem_data, value_name)
    attr(out, "unmatched") <- unmatched_analytes(chem_data, logical(0))
    attr(out, "report_type") <- attr(chem_data, "report_type")
    return(out)
  }

  chem_keys <- action_level_chem_keys(chem_data)
  al_keys <- action_level_lookup(action_levels)

  cand <- dplyr::inner_join(
    chem_keys,
    al_keys,
    by = c("match_key", "key_type"),
    relationship = "many-to-many"
  )

  if (!match_name) {
    cand <- cand[cand$key_type != "name", , drop = FALSE]
  }
  if (match_matrix) {
    # A matrix left unrecorded on either side matches anything, so that a
    # guideline is not dropped for want of a column. Where the guideline set
    # records no matrix at all that permissiveness covers every result, and
    # `ppm`/`ppb`/`%` sit in both dimensions - so a solid guideline would
    # convert 1:1 onto a water result. Too quiet to leave unsaid.
    if (all(is.na(al_keys$matrix_al))) {
      warning(
        "`action_levels` records no matrix_code, so its guidelines are ",
        "compared against every matrix in `chem_data`. Set matrix_code on ",
        "the guidelines, or pass match_matrix = FALSE to accept this."
      )
    }
    cand <- cand[
      is.na(cand$matrix_chem) |
        is.na(cand$matrix_al) |
        cand$matrix_chem == cand$matrix_al,
      ,
      drop = FALSE
    ]
  }
  if (match_fraction) {
    cand <- cand[applies_to_fraction(cand), , drop = FALSE]
  }

  # Two different faults, kept apart: a guideline whose unit is missing (fix
  # the guideline file) and one whose unit will not convert into the result's
  # (fix one unit or the other). Neither is joined.
  cand$unitless <- is.na(cand$criteria_unit) |
    !nzchar(trimws(cand$criteria_unit))
  cand$factor <- unit_conversion_factor(cand$criteria_unit, cand$unit_chem)
  cand$factor[cand$unitless] <- NA_real_
  unconvertible <- cand[is.na(cand$factor), , drop = FALSE]
  cand <- cand[!is.na(cand$factor), , drop = FALSE]
  cand$value <- cand$criteria * cand$factor

  # Keep only the best tier each result reached: an exact code match beats an
  # exact name match, which beats a bare CAS match.
  cand$rank <- match(cand$key_type, MATCH_TIERS)
  if (nrow(cand) > 0) {
    best <- stats::ave(cand$rank, cand$.row, FUN = min)
    cand <- cand[cand$rank == best, , drop = FALSE]
  }

  # More than one guideline can still apply - take the most conservative.
  # Only guidelines that disagree are worth reporting; two entries for the
  # same analyte often carry the same value.
  distinct_pair <- !duplicated(paste(cand$.row, signif(cand$value, 12)))
  ambiguous <- unique(
    cand$.row[distinct_pair][duplicated(cand$.row[distinct_pair])]
  )
  cand <- cand[order(cand$.row, cand$value), , drop = FALSE]
  cand <- cand[!duplicated(cand$.row), , drop = FALSE]

  # Only worth reporting a units failure where nothing else matched.
  unconvertible <- unconvertible[
    !unconvertible$.row %in% cand$.row,
    ,
    drop = FALSE
  ]

  out <- chem_data
  idx <- match(seq_len(nrow(out)), cand$.row)

  out[[value_name]] <- cand$value[idx]
  out[[paste0(value_name, "_name")]] <- ifelse(
    is.na(idx),
    NA_character_,
    set_name
  )
  # The guideline is converted into the result's own unit, so that is the
  # unit it is now expressed in.
  out[[paste0(value_name, "_unit")]] <- ifelse(
    is.na(idx),
    NA_character_,
    if ("output_unit" %in% names(chem_data)) {
      as.character(chem_data$output_unit)
    } else {
      NA_character_
    }
  )
  out[[paste0(value_name, "_basis")]] <- cand$criteria_basis[idx]

  crit <- out[[value_name]]
  conc <- suppressWarnings(as.numeric(chem_data$concentration))
  detected <- if ("detect_flag" %in% names(chem_data)) {
    !is.na(chem_data$detect_flag) & chem_data$detect_flag == "Y"
  } else {
    rep(TRUE, nrow(chem_data))
  }

  # Whether an LOR above the guideline counts as an exceedance is settled
  # here, once, rather than by each function that later reads `exceedance`.
  cmp <- comparison_columns(value_name)
  above <- !is.na(crit) & conc > crit
  out[[cmp[["ratio"]]]] <- conc / crit
  out[[cmp[["exceedance"]]]] <- above & (detected | lor_as_exceedance)
  out[[cmp[["lor"]]]] <- above & !detected
  out[[cmp[["exceedance"]]]][is.na(crit)] <- NA
  out[[cmp[["lor"]]]][is.na(crit)] <- NA

  unmatched <- unmatched_analytes(chem_data, is.na(idx))
  attr(out, "unmatched") <- unmatched
  attr(out, "report_type") <- attr(chem_data, "report_type")

  if (!quiet) {
    report_action_level_join(
      out[[value_name]],
      unmatched,
      unconvertible,
      ambiguous,
      set_name,
      cand$key_type
    )
  }

  out
}


#' Stack several joined guideline sets into one criteria column
#'
#' [join_action_levels()] puts each guideline set in its own column, which is
#' the shape to read a table in but the wrong one to analyse across sets. This
#' stacks them: every result appears once per guideline set, with a single
#' `criteria` column and a single `exceedance` column whichever set it came
#' from, so one `group_by(criteria_set)` covers the lot.
#'
#' The sets are found from the columns [join_action_levels()] writes - a value
#' column with matching `_name`, `_unit` and `_basis` columns beside it - so
#' no bookkeeping is needed beyond having joined them.
#'
#' @param data chemistry tibble with one or more guideline sets joined onto it
#'   by [join_action_levels()].
#' @param sets names of the value columns to stack, e.g.
#'   `c("criteria", "criteria_99")`. Defaults to every set found.
#' @param keep_unmatched keep the rows where a set found no guideline. Default
#'   `TRUE`, so a result missing from one set is still visible as a gap rather
#'   than dropping out of the table. `FALSE` returns comparisons only.
#'
#' @returns `data` with the per-set columns replaced by `criteria_set` (the
#'   column the guideline came from), `criteria_name` (the guideline set's
#'   name), `criteria`, `criteria_unit`, `criteria_basis`,
#'   `exceedance_ratio`, `exceedance` and `lor_above_criteria`. Row count is
#'   `nrow(data)` times the number of sets, less any dropped by
#'   `keep_unmatched = FALSE`.
#' @export
#' @examples
#' \dontrun{
#' compared <- chem %>%
#'   join_action_levels(anzg_95) %>%
#'   join_action_levels(anzg_99, value_col = "criteria_99")
#'
#' long <- criteria_long(compared)
#'
#' long %>%
#'   dplyr::group_by(criteria_set, chem_name) %>%
#'   dplyr::summarise(exceedances = sum(exceedance, na.rm = TRUE))
#' }
#' @seealso [join_action_levels()], which produces the wide form this reads.
#' @importFrom dplyr bind_rows
criteria_long <- function(data, sets = NULL, keep_unmatched = TRUE) {
  if (is.null(data)) {
    return(NULL)
  }
  found <- criteria_sets(data)

  if (is.null(sets)) {
    sets <- found
  } else {
    sets <- as.character(sets)
    unknown <- setdiff(sets, found)
    if (length(unknown) > 0) {
      stop(
        "No guideline set joined under: ",
        paste(unknown, collapse = ", "),
        if (length(found) > 0) {
          paste0(". Sets present: ", paste(found, collapse = ", "), ".")
        } else {
          "."
        }
      )
    }
  }

  if (length(sets) == 0) {
    stop(
      "`data` carries no joined guideline set. Join one with ",
      "join_action_levels() first."
    )
  }

  # Every column belonging to any set comes out, including sets not being
  # stacked - leaving them behind would put a stale second criteria column
  # next to the stacked one.
  set_cols <- unlist(lapply(found, set_columns), use.names = FALSE)
  base <- data[, setdiff(names(data), set_cols), drop = FALSE]

  parts <- lapply(sets, function(s) {
    cmp <- comparison_columns(s)
    part <- base
    part$criteria_set <- s
    part$criteria_name <- as.character(data[[paste0(s, "_name")]])
    part$criteria <- as.numeric(data[[s]])
    part$criteria_unit <- as.character(data[[paste0(s, "_unit")]])
    part$criteria_basis <- as.character(data[[paste0(s, "_basis")]])
    part$exceedance_ratio <- as.numeric(data[[cmp[["ratio"]]]])
    part$exceedance <- as.logical(data[[cmp[["exceedance"]]]])
    part$lor_above_criteria <- as.logical(data[[cmp[["lor"]]]])
    part$.result_row <- seq_len(nrow(data))
    if (!keep_unmatched) {
      part <- part[!is.na(part$criteria), , drop = FALSE]
    }
    part
  })

  out <- dplyr::bind_rows(parts)
  # Each result's sets kept together, in the order they were asked for.
  out <- out[
    order(out$.result_row, match(out$criteria_set, sets)), ,
    drop = FALSE
  ]
  out$.result_row <- NULL
  attr(out, "report_type") <- attr(data, "report_type")
  out
}

#' Name every column belonging to one guideline set
#'
#' @param value_name name of the guideline value column
#' @returns character vector of column names
#' @noRd
set_columns <- function(value_name) {
  c(
    value_name,
    paste0(value_name, c("_name", "_unit", "_basis")),
    unname(comparison_columns(value_name))
  )
}

#' Find the guideline sets joined onto a chemistry table
#'
#' A set is a value column carrying the `_name`, `_unit` and `_basis` columns
#' [join_action_levels()] writes beside it. Matching on all three keeps a
#' chemistry column that merely happens to be called `criteria` from being
#' read as a set.
#'
#' @param data chemistry tibble
#' @returns character vector of value column names
#' @noRd
criteria_sets <- function(data) {
  nms <- names(data)
  suffixed <- grep("_name$", nms, value = TRUE)
  stems <- sub("_name$", "", suffixed)
  stems <- stems[nzchar(stems) & stems %in% nms]
  stems[
    paste0(stems, "_unit") %in% nms &
      paste0(stems, "_basis") %in% nms
  ]
}


# ---------------------------------------------------------------------------
# Reading
# ---------------------------------------------------------------------------

#' Locate an action level sheet within a workbook
#'
#' As with the chemistry and gauging sheets, each candidate is peeked at with
#' a header offset of 0 and then 1, because the ESDAT exports can carry a
#' source/URL banner on the first row.
#'
#' @param myfile_path path to the workbook
#' @param all_sheets character vector of sheet names
#' @returns list(sheet, skip); `sheet` is NULL when nothing matched
#' @noRd
locate_action_level_sheet <- function(myfile_path, all_sheets) {
  for (skip in c(0, 1)) {
    for (s in all_sheets) {
      peek <- peek_names(myfile_path, s, skip = skip)
      if (is.null(peek)) {
        next
      }
      canonical <- canonical_names(peek, ACTION_LEVEL_ALIASES)
      has_chem <- any(c("chem_code", "chem_name") %in% canonical)
      if (has_chem && "criteria" %in% canonical) {
        return(list(sheet = s, skip = skip))
      }
    }
  }
  list(sheet = NULL, skip = NULL)
}

#' Normalise a raw action level export
#'
#' @param raw data frame straight from readxl
#' @param name name for the guideline set
#' @returns normalised tibble
#' @noRd
process_action_levels <- function(raw, name) {
  al <- raw %>%
    janitor::clean_names() %>%
    resolve_columns(ACTION_LEVEL_ALIASES)

  if (!"criteria" %in% names(al)) {
    stop(
      "No action level column found. Expected one of: ",
      paste(c("criteria", ACTION_LEVEL_ALIASES$criteria), collapse = ", "),
      "\nColumns found: ",
      paste(names(al), collapse = ", ")
    )
  }

  # The value and its unit share a cell ("0.006 µg Sn/L") unless the export
  # carries a separate unit column, in which case that one wins.
  parsed <- parse_action_level(al$criteria)
  al$criteria_text <- as.character(al$criteria)
  al$criteria <- parsed$value
  if ("criteria_unit" %in% names(al)) {
    supplied <- parse_unit_basis(al$criteria_unit)
    al$criteria_unit <- ifelse(
      is.na(supplied$unit),
      parsed$unit,
      supplied$unit
    )
    al$criteria_basis <- ifelse(
      is.na(supplied$basis),
      parsed$basis,
      supplied$basis
    )
  } else {
    al$criteria_unit <- parsed$unit
    al$criteria_basis <- parsed$basis
  }

  for (flag in c("leached", "total", "filtered")) {
    al[[flag]] <- if (flag %in% names(al)) {
      normalise_logical(al[[flag]])
    } else {
      NA
    }
  }

  for (nm in c("chem_code", "chem_name", "matrix_code", "conditions",
               "comments")) {
    if (!nm %in% names(al)) {
      al[[nm]] <- NA_character_
    } else {
      al[[nm]] <- as.character(al[[nm]])
    }
  }

  al$criteria_name <- as.character(name)

  dropped <- is.na(al$criteria)
  if (any(dropped)) {
    # Naming the cells matters here: a range or a stray character is the
    # difference between a guideline that is absent and one that was
    # misread, and only the original text says which.
    shown <- unique(al$criteria_text[dropped & !is.na(al$criteria_text)])
    message(
      "Dropped ",
      sum(dropped),
      " action level rows with no readable value",
      if (length(shown) > 0) {
        paste0(
          ": ",
          paste(utils::head(shown, 8), collapse = ", "),
          if (length(shown) > 8) ", ..." else ""
        )
      } else {
        ""
      },
      "."
    )
  }

  al <- al %>% dplyr::filter(!is.na(criteria))

  # Flagged at the door rather than at the join, because the fix belongs in
  # the guideline file and the user is looking at it right now.
  no_unit <- is.na(al$criteria_unit) | !nzchar(trimws(al$criteria_unit))
  if (any(no_unit)) {
    shown <- unique(stats::na.omit(al$chem_name[no_unit]))
    if (length(shown) == 0) {
      shown <- unique(stats::na.omit(al$chem_code[no_unit]))
    }
    warning(
      sum(no_unit),
      " of ",
      nrow(al),
      " action levels carry no unit and cannot be joined to a result",
      if (length(shown) > 0) {
        paste0(
          ": ",
          paste(utils::head(shown, 8), collapse = ", "),
          if (length(shown) > 8) ", ..." else ""
        )
      } else {
        ""
      },
      ".\nAdd the unit to the action level cell (\"80 µg/L\") or to a ",
      "units column. The result's own unit is not assumed."
    )
  }

  al %>% dplyr::relocate(dplyr::any_of(ACTION_LEVEL_SCHEMA))
}


# ---------------------------------------------------------------------------
# Matching helpers
# ---------------------------------------------------------------------------

#' Build the per-result match keys for a chemistry table
#'
#' Each result contributes up to two rows - one keyed on its chemical code and
#' one on its chemical name - so a single join can try both and prefer the
#' code match afterwards. The code is carried whole: it identifies the analyte
#' and is never trimmed to make it meet a guideline.
#'
#' @param chem_data chemistry tibble from [data_processor()]
#' @returns long tibble of (.row, key_type, match_key) plus the columns the
#'   applicability rules need
#' @noRd
action_level_chem_keys <- function(chem_data) {
  n <- nrow(chem_data)
  base <- data.frame(
    .row = seq_len(n),
    matrix_chem = if ("matrix_code" %in% names(chem_data)) {
      normalise_matrix(chem_data$matrix_code)
    } else {
      NA_character_
    },
    fraction_chem = if ("fraction" %in% names(chem_data)) {
      normalise_fraction(chem_data$fraction)
    } else {
      NA_character_
    },
    leached_chem = if ("result_type" %in% names(chem_data)) {
      grepl("LEACH", toupper(as.character(chem_data$result_type)), fixed = TRUE)
    } else {
      FALSE
    },
    unit_chem = if ("output_unit" %in% names(chem_data)) {
      as.character(chem_data$output_unit)
    } else {
      NA_character_
    },
    stringsAsFactors = FALSE
  )
  base$leached_chem[is.na(base$leached_chem)] <- FALSE

  by_code <- base
  by_code$key_type <- "code"
  by_code$match_key <- if ("chem_code" %in% names(chem_data)) {
    normalise_code(chem_data$chem_code)
  } else {
    NA_character_
  }

  by_name <- base
  by_name$key_type <- "name"
  by_name$match_key <- normalise_chem_name(chem_data$chem_name)

  keys <- rbind(by_code, by_name)
  keys[!is.na(keys$match_key), , drop = FALSE]
}

#' Build the match lookup for an action level table
#'
#' @param action_levels action level tibble from [action_level_processor()]
#' @returns long tibble of (key_type, match_key) plus the guideline columns
#' @noRd
action_level_lookup <- function(action_levels) {
  base <- data.frame(
    criteria = as.numeric(action_levels$criteria),
    criteria_unit = as.character(action_levels$criteria_unit),
    criteria_basis = as.character(action_levels$criteria_basis),
    matrix_al = normalise_matrix(action_levels$matrix_code),
    leached_al = normalise_logical(action_levels$leached),
    total_al = normalise_logical(action_levels$total),
    filtered_al = normalise_logical(action_levels$filtered),
    stringsAsFactors = FALSE
  )

  by_code <- base
  by_code$key_type <- "code"
  by_code$match_key <- normalise_code(action_levels$chem_code)

  by_name <- base
  by_name$key_type <- "name"
  by_name$match_key <- normalise_chem_name(action_levels$chem_name)

  lookup <- rbind(by_code, by_name)
  lookup[!is.na(lookup$match_key) & !is.na(lookup$criteria), , drop = FALSE]
}

#' Decide whether each candidate guideline applies to its result
#'
#' A guideline flagged for one fraction is not comparable against the other.
#' Unrecorded flags - an export that leaves them blank, or a result whose
#' fraction was never captured (EQuIS reports `"N"` for a sample with no
#' fraction concept) - are permissive rather than exclusive, because the
#' alternative is silently dropping guidelines that do apply.
#'
#' @param cand joined candidate table
#' @returns logical vector, one per candidate row
#' @noRd
applies_to_fraction <- function(cand) {
  leach_ok <- ifelse(
    is.na(cand$leached_al),
    TRUE,
    cand$leached_chem == cand$leached_al
  )

  frac_ok <- rep(TRUE, nrow(cand))
  is_f <- !is.na(cand$fraction_chem) & cand$fraction_chem == "F"
  is_t <- !is.na(cand$fraction_chem) & cand$fraction_chem == "T"
  frac_ok[is_f] <- ifelse(
    is.na(cand$filtered_al[is_f]),
    TRUE,
    cand$filtered_al[is_f]
  )
  frac_ok[is_t] <- ifelse(
    is.na(cand$total_al[is_t]),
    TRUE,
    cand$total_al[is_t]
  )
  # Fraction unrecorded: either flag will do, so long as one is set.
  unknown <- !is_f & !is_t
  frac_ok[unknown] <- ifelse(
    is.na(cand$total_al[unknown]) & is.na(cand$filtered_al[unknown]),
    TRUE,
    (!is.na(cand$total_al[unknown]) & cand$total_al[unknown]) |
      (!is.na(cand$filtered_al[unknown]) & cand$filtered_al[unknown])
  )

  leach_ok & frac_ok
}

#' Name the comparison columns for a guideline set
#'
#' The default set keeps the bare names the rest of the package reads. A
#' second set joined under its own `value_col` takes that name as a prefix, so
#' joining it does not overwrite the first set's verdict - which is what makes
#' several sets stackable by [criteria_long()].
#'
#' @param value_name name of the guideline value column
#' @returns named character vector: ratio, exceedance, lor
#' @noRd
comparison_columns <- function(value_name) {
  stem <- if (identical(value_name, "criteria")) {
    ""
  } else {
    paste0(value_name, "_")
  }
  c(
    ratio = paste0(stem, "exceedance_ratio"),
    exceedance = paste0(stem, "exceedance"),
    lor = paste0(stem, "lor_above_criteria")
  )
}

#' Add the guideline columns to a chemistry table with no rows
#'
#' Typed rather than logical `NA`, so an empty result binds cleanly against a
#' populated one.
#'
#' @param chem_data zero-row chemistry tibble
#' @param value_name name of the guideline value column
#' @returns `chem_data` with the guideline columns added, still with no rows
#' @noRd
empty_criteria_columns <- function(chem_data, value_name) {
  cmp <- comparison_columns(value_name)
  chem_data[[value_name]] <- numeric(0)
  chem_data[[paste0(value_name, "_name")]] <- character(0)
  chem_data[[paste0(value_name, "_unit")]] <- character(0)
  chem_data[[paste0(value_name, "_basis")]] <- character(0)
  chem_data[[cmp[["ratio"]]]] <- numeric(0)
  chem_data[[cmp[["exceedance"]]]] <- logical(0)
  chem_data[[cmp[["lor"]]]] <- logical(0)
  chem_data
}

#' Summarise the analytes that found no guideline
#'
#' @param chem_data chemistry tibble
#' @param no_match logical vector, one per chemistry row
#' @returns tibble of the distinct unmatched analytes with a row count
#' @noRd
unmatched_analytes <- function(chem_data, no_match) {
  if (!any(no_match)) {
    return(chem_data[0, intersect(
      c("chem_code", "chem_name", "matrix_code", "fraction", "output_unit"),
      names(chem_data)
    ), drop = FALSE])
  }
  cols <- intersect(
    c("chem_code", "chem_name", "matrix_code", "fraction", "output_unit"),
    names(chem_data)
  )
  miss <- chem_data[no_match, cols, drop = FALSE]
  counts <- as.data.frame(table(
    do.call(paste, c(as.list(lapply(miss, as.character)), sep = "\r"))
  ), stringsAsFactors = FALSE)
  out <- unique(miss)
  key <- do.call(paste, c(as.list(lapply(out, as.character)), sep = "\r"))
  out$n_results <- counts$Freq[match(key, counts$Var1)]
  out[order(-out$n_results), , drop = FALSE]
}

#' Report what the join did
#'
#' @param values the joined guideline values
#' @param unmatched table of unmatched analytes
#' @param unconvertible candidate rows dropped on units
#' @param ambiguous row indices that matched more than one guideline
#' @param set_name name of the guideline set
#' @param tiers the tier each matched result was taken from
#' @noRd
report_action_level_join <- function(
  values,
  unmatched,
  unconvertible,
  ambiguous,
  set_name,
  tiers = character(0)
) {
  matched <- sum(!is.na(values))
  by_tier <- table(factor(tiers, levels = MATCH_TIERS))
  labels <- c(
    code = "chem_code",
    name = "chem_name"
  )
  message(
    "Matched ",
    matched,
    " of ",
    length(values),
    " results to ",
    if (is.na(set_name)) "the action levels" else set_name,
    if (matched > 0) {
      paste0(
        " (",
        paste(
          labels[names(by_tier)][by_tier > 0],
          by_tier[by_tier > 0],
          sep = ": ",
          collapse = ", "
        ),
        ")"
      )
    } else {
      ""
    },
    "."
  )

  if (nrow(unmatched) > 0) {
    shown <- utils::head(unmatched, 10)
    label <- if ("chem_name" %in% names(shown)) {
      shown$chem_name
    } else {
      shown$chem_code
    }
    message(
      "No guideline value for ",
      nrow(unmatched),
      " analyte/matrix combinations",
      if (nrow(unconvertible) > 0) {
        " (some of them matched, then were dropped on units - see below)"
      } else {
        ""
      },
      ": ",
      paste0(label, " (", shown$n_results, ")", collapse = ", "),
      if (nrow(unmatched) > 10) ", ..." else "",
      "\nSee attr(x, \"unmatched\") for the full list."
    )
  }

  # A missing unit and an incompatible one are different faults with different
  # fixes, so they are never pooled into one count.
  unitless <- unconvertible[unconvertible$unitless, , drop = FALSE]
  mismatched <- unconvertible[!unconvertible$unitless, , drop = FALSE]

  if (nrow(unitless) > 0) {
    analytes <- unique(stats::na.omit(unitless$match_key))
    message(
      "NOT JOINED - ",
      nrow(unitless),
      " matches whose guideline carries no unit, so nothing could be ",
      "compared: ",
      paste(utils::head(analytes, 8), collapse = ", "),
      if (length(analytes) > 8) ", ..." else "",
      "\nAdd the unit to the guideline file and read it again - it is not ",
      "assumed to be the unit the result was reported in."
    )
  }

  if (nrow(mismatched) > 0) {
    pairs <- unique(paste0(
      mismatched$criteria_unit,
      " -> ",
      mismatched$unit_chem
    ))
    message(
      "NOT JOINED - ",
      nrow(mismatched),
      " matches whose units could not be converted: ",
      paste(utils::head(pairs, 8), collapse = ", "),
      if (length(pairs) > 8) ", ..." else ""
    )
  }

  if (length(ambiguous) > 0) {
    message(
      length(ambiguous),
      " results matched more than one guideline; the lowest was used."
    )
  }
}


# ---------------------------------------------------------------------------
# Parsing and normalisation
# ---------------------------------------------------------------------------

#' Split an action level cell into value, unit and basis
#'
#' Handles `"80 µg/L"`, `"0.006 µg Sn/L"`, `"<0.1 mg/L"`, `"1,000 µg/L"` and a
#' bare number. A cell holding a range (`"6.5 - 8.5"`) has no single value to
#' compare a result against, so it is left unreadable and reported rather than
#' silently read as its lower bound.
#'
#' @param x character vector of action level cells
#' @returns list(value, unit, basis)
#' @noRd
parse_action_level <- function(x) {
  txt <- trimws(as.character(x))
  # A comma grouping three digits is a thousands separator, not the start of
  # the unit. Anchored that tightly so anything else keeps its comma.
  txt <- gsub("(?<=[0-9]),(?=[0-9]{3}(\\D|$))", "", txt, perl = TRUE)
  num <- "([0-9]*\\.?[0-9]+(?:[eE][+-]?[0-9]+)?)"
  pattern <- paste0("^[<>=~]*\\s*", num, "\\s*(.*)$")

  value <- suppressWarnings(as.numeric(sub(pattern, "\\1", txt)))
  unit_txt <- ifelse(grepl(pattern, txt), sub(pattern, "\\2", txt), NA)

  # What follows the number should be a unit. Where it opens with another
  # number the cell holds a range, and taking the first of the two would read
  # a pH band as an upper limit.
  ranged <- !is.na(unit_txt) & grepl("^[-–—]?\\s*[0-9]", unit_txt)
  value[ranged] <- NA_real_
  unit_txt[ranged] <- NA_character_

  parsed <- parse_unit_basis(unit_txt)

  list(value = value, unit = parsed$unit, basis = parsed$basis)
}

#' Split a unit string into the unit and its basis of measurement
#'
#' ESDAT records the basis inside the unit - `"µg Sn/L"` for tributyltin
#' expressed as tin, `"mg N/L"` for nitrogen species. The basis is pulled out
#' so the remaining unit converts like any other.
#'
#' @param x character vector of unit strings
#' @returns list(unit, basis)
#' @noRd
parse_unit_basis <- function(x) {
  txt <- trimws(as.character(x))
  txt[!nzchar(txt) | is.na(txt)] <- NA_character_

  # "(as N)" or "as N" trailing the unit
  basis <- rep(NA_character_, length(txt))
  trailing <- "^(.*?)\\s*\\(?as\\s+([A-Za-z0-9]+)\\)?\\s*$"
  has_trailing <- !is.na(txt) & grepl(trailing, txt, ignore.case = TRUE)
  basis[has_trailing] <- sub(
    trailing,
    "\\2",
    txt[has_trailing],
    ignore.case = TRUE
  )
  txt[has_trailing] <- sub(
    trailing,
    "\\1",
    txt[has_trailing],
    ignore.case = TRUE
  )

  # "µg Sn/L" - an element symbol sitting between the mass and the divisor.
  # Matched against the raw string, not the folded one, so the symbol keeps
  # its capitalisation.
  embedded <- "^\\s*([µμnumpk]?g)\\s+([A-Za-z]{1,3})\\s*/\\s*(.+)$"
  has_embedded <- !is.na(txt) & grepl(embedded, txt)
  if (any(has_embedded)) {
    hit <- txt[has_embedded]
    basis[has_embedded] <- ifelse(
      is.na(basis[has_embedded]),
      sub(embedded, "\\2", hit),
      basis[has_embedded]
    )
    txt[has_embedded] <- sub(embedded, "\\1/\\3", hit)
  }

  txt <- trimws(txt)
  txt[!is.na(txt) & !nzchar(txt)] <- NA_character_
  list(unit = txt, basis = basis)
}

#' Fold the assorted spellings of a unit into one form
#'
#' The micro sign and the Greek mu both appear in ESDAT exports, and EQuIS
#' lower-cases its litres (`mg/l`).
#'
#' @param x character vector of unit strings
#' @noRd
normalise_unit_chars <- function(x) {
  u <- trimws(tolower(as.character(x)))
  u <- gsub("µ|μ", "u", u)
  u <- gsub("\\s+", "", u)
  u
}

#' Convert between concentration units
#'
#' Returns the number a value in `from` is multiplied by to express it in
#' `to`. Conversion is only attempted within a dimension - concentration in a
#' liquid, concentration in a solid - so a soil guideline is never silently
#' compared against a water result. `ppm` and `ppb` belong to both dimensions
#' and take whichever one the other unit belongs to.
#'
#' @param from,to character vectors of unit strings, recycled together
#' @returns numeric vector of factors, `NA` where no conversion exists
#' @noRd
unit_conversion_factor <- function(from, to) {
  n <- max(length(from), length(to))
  from <- normalise_unit_chars(rep_len(from, n))
  to <- normalise_unit_chars(rep_len(to, n))

  out <- rep(NA_real_, n)
  same <- !is.na(from) & !is.na(to) & from == to
  out[same] <- 1

  todo <- which(!same & !is.na(from) & !is.na(to))
  if (length(todo) == 0) {
    return(out)
  }

  # A chemistry table carries a handful of distinct units against a handful of
  # guideline units, however many results there are, so the distinct pairs are
  # resolved once and matched back rather than walked row by row.
  pair <- paste(from[todo], to[todo], sep = "\r")
  first <- !duplicated(pair)
  from_u <- from[todo][first]
  to_u <- to[todo][first]

  factors <- rep(NA_real_, length(from_u))
  for (i in seq_along(from_u)) {
    f <- UNIT_FACTORS[[from_u[i]]]
    t <- UNIT_FACTORS[[to_u[i]]]
    if (is.null(f) || is.null(t)) {
      next
    }
    shared <- intersect(names(f), names(t))
    if (length(shared) == 0) {
      next
    }
    ratios <- unname(f[shared] / t[shared])
    if (length(unique(signif(ratios, 12))) == 1) {
      factors[i] <- ratios[[1]]
    }
  }

  out[todo] <- factors[match(pair, pair[first])]
  out
}

#' Fold matrix spellings into a single form
#'
#' EQuIS reports `WATER`, ESDAT reports `Water`. Sediment guidelines are
#' issued against the solid phase and ESDAT files sediment samples under
#' `Soil`, so the two are folded together.
#'
#' @param x character vector of matrix codes
#' @noRd
normalise_matrix <- function(x) {
  m <- toupper(trimws(as.character(x)))
  m[!is.na(m) & !nzchar(m)] <- NA_character_
  ifelse(
    is.na(m),
    NA_character_,
    ifelse(
      m %in% MATRIX_WATER,
      "WATER",
      ifelse(m %in% MATRIX_SOLID, "SOIL", m)
    )
  )
}

#' Fold fraction codes into "T", "F" or NA
#'
#' EQuIS uses `T`/`D`/`N` (total, dissolved, not applicable), ESDAT uses
#' `T`/`F`. `N` and blanks become `NA` - fraction unrecorded, rather than
#' fraction known to be neither.
#'
#' @param x character vector of fraction codes
#' @noRd
normalise_fraction <- function(x) {
  if (is.logical(x)) {
    return(ifelse(is.na(x), NA_character_, ifelse(x, "F", "T")))
  }
  f <- toupper(trimws(as.character(x)))
  ifelse(
    f %in% c("F", "D", "FILTERED", "DISSOLVED", "TRUE"),
    "F",
    ifelse(f %in% c("T", "TOTAL", "FALSE"), "T", NA_character_)
  )
}

#' Fold an ESDAT flag column into logical
#'
#' The exports write these as the strings `"true"`/`"false"`.
#'
#' @param x vector to normalise
#' @noRd
normalise_logical <- function(x) {
  if (is.logical(x)) {
    return(x)
  }
  if (is.numeric(x)) {
    return(ifelse(is.na(x), NA, x > 0))
  }
  f <- toupper(trimws(as.character(x)))
  ifelse(
    f %in% c("TRUE", "T", "Y", "YES", "1"),
    TRUE,
    ifelse(f %in% c("FALSE", "F", "N", "NO", "0"), FALSE, NA)
  )
}

#' Normalise a chemical code for matching
#' @param x character vector of chemical codes
#' @noRd
normalise_code <- function(x) {
  code <- toupper(trimws(as.character(x)))
  code <- gsub("\\s+", "", code)
  code[!nzchar(code) | code == "NA"] <- NA_character_
  code
}

#' Normalise a chemical name for matching
#'
#' Drops the `Dissolved` prefix [data_processor()] adds to filtered results,
#' so a dissolved copper result still finds the copper guideline.
#'
#' @param x character vector of chemical names
#' @noRd
normalise_chem_name <- function(x) {
  nm <- tolower(trimws(as.character(x)))
  nm <- sub("^dissolved\\s+", "", nm)
  nm <- gsub("\\s+", " ", nm)
  nm[!nzchar(nm) | nm == "na"] <- NA_character_
  nm
}


# ---------------------------------------------------------------------------
# Dictionaries
# ---------------------------------------------------------------------------

# Canonical names follow the columns the rest of the package already uses -
# `criteria` is the guideline value because that is what summary_stats() and
# timeseries_plot() read.
ACTION_LEVEL_ALIASES <- list(
  chem_code = c(
    "chemcode",
    "cas_rn",
    "cas_number",
    "casrn",
    "cas",
    "chemical_code",
    "analyte_code"
  ),
  chem_name = c(
    "chemname",
    "chemical_name",
    "analyte",
    "analyte_name",
    "chemical"
  ),
  matrix_code = c("matrix_type", "matrixtype", "matrix"),
  criteria = c(
    "action_level",
    "action_level_value",
    "actionlevel",
    "guideline",
    "guideline_value",
    "trigger_value",
    "screening_level",
    "criterion",
    "value"
  ),
  criteria_unit = c(
    "action_level_unit",
    "action_level_units",
    "guideline_unit",
    "unit",
    "units"
  ),
  leached = c("leachable", "is_leached"),
  total = c("is_total"),
  filtered = c("is_filtered", "dissolved"),
  conditions = c("condition", "conditional", "qualifier"),
  comments = c("comment", "remark", "remarks", "notes", "note")
)

# Column order of the normalised action level table.
ACTION_LEVEL_SCHEMA <- c(
  "criteria_name",
  "chem_code",
  "chem_name",
  "matrix_code",
  "criteria",
  "criteria_unit",
  "criteria_basis",
  "criteria_text",
  "leached",
  "total",
  "filtered",
  "conditions",
  "comments"
)

# Match tiers, best first. The chemical code identifies the analyte, so a name
# match is only ever used where the code found nothing.
MATCH_TIERS <- c("code", "name")

# Matrix spellings folded together by normalise_matrix().
MATRIX_WATER <- c(
  "WATER",
  "WG",
  "WS",
  "GROUNDWATER",
  "SURFACE WATER",
  "SURFACEWATER",
  "LIQUID",
  "LEACHATE",
  "AQUEOUS"
)
MATRIX_SOLID <- c("SOIL", "SO", "SD", "SEDIMENT", "SOLID", "SLUDGE")

# Unit conversion factors, per dimension, to that dimension's base unit
# (mg/L for a concentration in a liquid, mg/kg for one in a solid). Units
# carrying an entry for both dimensions - the dimensionless ratios - convert
# against whichever dimension the other unit belongs to.
UNIT_FACTORS <- list(
  "ng/l" = c(liquid = 1e-6),
  "ug/l" = c(liquid = 1e-3),
  "mg/l" = c(liquid = 1),
  "g/l" = c(liquid = 1e3),
  "kg/l" = c(liquid = 1e6),
  "ug/ml" = c(liquid = 1),
  "mg/ml" = c(liquid = 1e3),
  "ng/ml" = c(liquid = 1e-3),
  "ug/m3" = c(liquid = 1e-6),
  "mg/m3" = c(liquid = 1e-3),
  "ng/kg" = c(solid = 1e-6),
  "ug/kg" = c(solid = 1e-3),
  "mg/kg" = c(solid = 1),
  "g/kg" = c(solid = 1e3),
  "ug/g" = c(solid = 1),
  "mg/g" = c(solid = 1e3),
  "ppt" = c(liquid = 1e-6, solid = 1e-6),
  "ppb" = c(liquid = 1e-3, solid = 1e-3),
  "ppm" = c(liquid = 1, solid = 1),
  "%" = c(liquid = 1e4, solid = 1e4),
  "%w/w" = c(solid = 1e4),
  "%w/v" = c(liquid = 1e4)
)

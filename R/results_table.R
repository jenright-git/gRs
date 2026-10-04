#' Report table of every result against its guidelines
#'
#' Lays out a table of results the way a report presents them: one row per
#' sample, one column per analyte, with the guideline sets the results are
#' compared against written across the top. Returns a formatted `gt` table;
#' [results_table_to_excel()] writes the same table to a workbook.
#'
#' Across the top, each analyte's column is headed by its chemical group,
#' then its name and its unit, then one row per guideline set giving the
#' guideline value, shaded in that set's colour. Down the side, each row is
#' named by its location and by the `id_cols` - by default the sample date,
#' sample ID and lab report.
#'
#' In the body:
#'
#' * a **detected** result is bold;
#' * a **non-detect** is written as `<` and its limit of reporting, in grey;
#' * a result that **exceeds** a guideline is shaded in that guideline's
#'   colour. Where it exceeds more than one, it takes the colour of the
#'   *highest* guideline it exceeds - exceeding that implies exceeding the
#'   lower ones. Where two sets give the same guideline, the first set named
#'   in `criteria_col` wins;
#' * `-` marks an analyte the sample was not analysed for.
#'
#' Whether a result exceeds a guideline is read from the `exceedance` verdict
#' [join_action_levels()] wrote for that set, so this table agrees with every
#' other function reading it. A non-detect whose LOR sits above a guideline
#' has not been shown to comply, but neither was the analyte detected:
#' `highlight_lor` decides whether it is shaded.
#'
#' An analyte reported in more than one unit gets a column per unit. Where
#' a guideline set gives one analyte more than one guideline - a fraction or
#' unit apart - the header shows their range, and each result is still judged
#' against its own.
#'
#' Each cell holds one result. Where two different results share a cell - a
#' re-analysis, or samples `id_cols` no longer tells apart - the highest
#' detected result is shown (or the highest LOR, where none was detected),
#' with a warning. A result the export repeats word for word is shown once,
#' without one.
#'
#' The colours follow [mka_to_excel()] and [summary_stats_to_excel()]: white
#' headings on a deep green, and the location names set the same way. A
#' legend is added below the table as source notes.
#'
#' Requires the `gt` package, which is not installed with gRs.
#'
#' @param data chemistry tibble from [data_processor()], with any guideline
#'   sets to show joined on by [join_action_levels()] - one column per set,
#'   not stacked by [criteria_long()]. Filter it first to the samples and
#'   analytes the table should hold.
#' @param criteria_col the guideline set or sets to show, each the
#'   `value_col` it was joined under. One column, or several as
#'   `c(criteria_95, criteria_99)`, with or without quotes; `all_of()` and
#'   helpers such as `starts_with("criteria")` work too. Each set adds a row
#'   across the top, in the order named. Default `criteria`, the column
#'   [join_action_levels()] writes unless given another `value_col`, used
#'   where it is there; any other set joined alongside is named in a message
#'   rather than dropped unremarked. `NULL` shows no guidelines.
#' @param id_cols the columns written beside the location, as a character
#'   vector, in the order they are to appear. Rows are sorted by location and
#'   then by these, in this order. Remove a column by leaving it out, or add
#'   any column `data` holds - `c("date", "sample_code", "start_depth",
#'   "end_depth")` for soil, say, or `c("date", "sys_sample_code", "lab_sdg")`
#'   for an EQuIS export. Name an element to set its heading, e.g.
#'   `c("Date Sampled" = "date", "sample_code")`; an unnamed column takes a
#'   heading from a built-in dictionary, or failing that from its own name.
#'   Each should hold one value per sample: a column that varies by analyte
#'   splits a sample over several rows. Left at the default,
#'   `c("date", "sample_code", "lab_report_number")`, any of the three `data`
#'   lacks is left out with a message; a column named outright that `data`
#'   lacks is an error. `NULL` gives the location alone.
#' @param highlight_lor shade a non-detect whose limit of reporting is above a
#'   guideline, as an exceedance is - its LOR is too high to show the
#'   guideline was met. Default `FALSE`, which writes it as any other
#'   non-detect. Settled here whatever `lor_as_exceedance` was given to
#'   [join_action_levels()].
#' @param criteria_colours the fill for each guideline set, as hex codes or
#'   colour names. Name each by the set's column or by its name, e.g.
#'   `c(criteria_99 = "#FBD08A")` or `c("NEMP 95%" = "#F0A868")`, and only
#'   the sets named change. Unnamed, it must hold one colour per set, in
#'   `criteria_col` order. `NULL` (default) uses warm tints - amber, orange,
#'   rose, then blue and lilac.
#' @param include_zone write the monitoring zone as the first column, ahead of
#'   the location, and sort the rows by zone. Default `FALSE`.
#' @param zone_col Name of the column containing monitoring zones. Can be
#'   provided with or without quotes. Default is monitoring_zone. Locations
#'   with no zone recorded are left blank, and a location falling in more
#'   than one zone is an error.
#' @param zone_label heading for the zone column. Default `"Monitoring Zone"`.
#' @param location_label heading for the location column. Default
#'   `"Monitoring Well"`.
#' @param header_fill background colour for the headings and the location
#'   names. Default `"#008768"`, a deep green.
#' @param header_font text colour for those same cells. Default white.
#' @param location_fill background colour for the location names where a
#'   zone column sits to their left. Default `"#9BBEAF"`, a lighter sage.
#' @param location_font text colour for those same names. Defaults to
#'   `header_font`.
#'
#' @returns A `gt_tbl`.
#' @export
#'
#' @examples
#' if (requireNamespace("gt", quietly = TRUE)) {
#'   # One monitoring round, no guidelines
#'   gRs_data %>%
#'     dplyr::filter(monitoring_round == monitoring_round[[1]]) %>%
#'     results_table(id_cols = c("date", "sample_code"))
#' }
#'
#' \dontrun{
#' # Two guideline sets across the top, each shading its exceedances
#' gRs_data %>%
#'   join_action_levels(nemp_99, value_col = "criteria_99") %>%
#'   join_action_levels(nemp_95, value_col = "criteria_95") %>%
#'   results_table(criteria_col = c(criteria_99, criteria_95))
#'
#' # Non-detects with an LOR above a guideline shaded too
#' compared %>% results_table(highlight_lor = TRUE)
#'
#' # Soil: depths beside the location, under headings of your own
#' soil %>%
#'   results_table(
#'     id_cols = c("date", "sample_code", "From (m)" = "start_depth",
#'                 "To (m)" = "end_depth"),
#'     location_label = "Borehole"
#'   )
#' }
#' @seealso [results_table_to_excel()] for the same table as a workbook, and
#'   [join_action_levels()] to join the guideline sets it shows.
#' @importFrom rlang enquo quo_name check_installed
results_table <- function(
  data,
  criteria_col = criteria,
  id_cols = c("date", "sample_code", "lab_report_number"),
  highlight_lor = FALSE,
  criteria_colours = NULL,
  include_zone = FALSE,
  zone_col = monitoring_zone,
  zone_label = "Monitoring Zone",
  location_label = "Monitoring Well",
  header_fill = "#008768",
  header_font = "#FFFFFF",
  location_fill = "#9BBEAF",
  location_font = header_font
) {
  rlang::check_installed(
    "gt",
    reason = "to build a table with results_table()."
  )

  xtab <- results_crosstab(
    data,
    criteria_col = rlang::enquo(criteria_col),
    criteria_named = !missing(criteria_col),
    id_cols = id_cols,
    id_named = !missing(id_cols),
    highlight_lor = highlight_lor,
    criteria_colours = criteria_colours,
    include_zone = include_zone,
    zone_name = rlang::quo_name(rlang::enquo(zone_col)),
    zone_label = zone_label,
    location_label = location_label
  )

  results_gt(xtab, header_fill, header_font, location_fill, location_font)
}


#' Lay a table of results out as a sample-by-analyte crosstab
#'
#' Everything [results_table()] and [results_table_to_excel()] write, settled
#' once so the two cannot disagree: the rows, the columns, the result in each
#' cell, and the guideline set each cell is shaded for.
#'
#' @param data the results, as passed to [results_table()]
#' @param criteria_col the `criteria_col` argument, as a quosure
#' @param criteria_named whether `criteria_col` was passed at all
#' @param id_cols the `id_cols` argument
#' @param id_named whether `id_cols` was passed at all
#' @param highlight_lor,criteria_colours,include_zone,zone_label,location_label
#'   as for [results_table()]
#' @param zone_name the zone column's name, as a string
#' @returns a list:
#'   * `ids`: one row per table row - the zone, the location and the
#'     `id_cols` - as their own types, sorted;
#'   * `id_labels`: a heading for each of those columns;
#'   * `include_zone`: whether the first of them is the zone;
#'   * `analytes`: one row per analyte column, `chem_group`, `chem_name` and
#'     `output_unit`;
#'   * `conc`, `nd`: matrices of the result in each cell and whether it is a
#'     non-detect, `NA` where the sample was not analysed for the analyte;
#'   * `fill`: matrix of the guideline set each cell is shaded for, `NA` for
#'     none;
#'   * `sets`, `set_labels`, `set_colours`: each guideline set's column, the
#'     name it is shown under and its colour;
#'   * `guideline_value`, `guideline_text`: matrices, one row per set and one
#'     column per analyte, of the guideline as a number (`NA` where there is
#'     none, or a range) and as text (`""` where there is none);
#'   * `highlight_lor`.
#' @noRd
results_crosstab <- function(
  data,
  criteria_col,
  criteria_named,
  id_cols,
  id_named,
  highlight_lor,
  criteria_colours,
  include_zone,
  zone_name,
  zone_label,
  location_label
) {
  check_flag(highlight_lor, "highlight_lor")
  check_flag(include_zone, "include_zone")

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame of results from data_processor().")
  }
  # Stacked, every result appears once per set, and a cell could not hold
  # one result.
  if ("criteria_set" %in% names(data)) {
    stop(
      "`data` comes from a table stacked by criteria_long(), which holds ",
      "every result once per guideline set. Pass the table from ",
      "join_action_levels() instead, and name the sets to show in ",
      "`criteria_col`, e.g. criteria_col = c(criteria_95, criteria_99)."
    )
  }
  absent <- setdiff(c("location_code", "chem_name", "concentration"), names(data))
  if (length(absent) > 0) {
    stop(
      "`data` is missing required columns: ",
      toString(absent),
      ". Pass results from data_processor(), with any guideline sets joined ",
      "on by join_action_levels()."
    )
  }
  if (!any(c("detect_flag", "prefix") %in% names(data))) {
    stop(
      "`data` has neither a `detect_flag` nor a `prefix` column, so a ",
      "detected result cannot be told from a non-detect."
    )
  }
  if (nrow(data) == 0) {
    stop("`data` holds no results to tabulate.")
  }
  if (include_zone) {
    if (zone_name %in% c("location_code", "chem_name")) {
      stop(glue::glue(
        "`zone_col` names {zone_name}, which is already in use as the ",
        "location or analyte column."
      ))
    }
    if (!zone_name %in% names(data)) {
      stop(glue::glue(
        "`data` has no column {zone_name}. Name the column holding ",
        "monitoring zones with `zone_col`."
      ))
    }
  }

  ids <- resolve_id_cols(
    id_cols,
    id_named,
    data,
    exclude = c("location_code", if (include_zone) zone_name)
  )
  lead <- c(if (include_zone) zone_name, "location_code", unname(ids))

  sets <- criteria_columns(
    criteria_col,
    criteria_named,
    data[0, , drop = FALSE],
    holder = "`data`",
    when = ""
  )
  for (set in sets) {
    verdicts <- unname(comparison_columns(set)[c("exceedance", "lor")])
    lacking <- setdiff(verdicts, names(data))
    if (length(lacking) > 0) {
      stop(
        "Guideline set ", set, " has no ", toString(lacking), " column. ",
        "Join it with join_action_levels(), which writes them."
      )
    }
  }

  conc <- suppressWarnings(as.numeric(data$concentration))
  # As join_action_levels() decides it: only a "Y" is a detect.
  nd <- if ("detect_flag" %in% names(data)) {
    !(as.character(data$detect_flag) %in% "Y")
  } else {
    as.character(data$prefix) %in% "<"
  }

  # --- columns: one per analyte and unit, by group then name
  analyte <- data.frame(
    chem_group = column_or_na(data, "chem_group"),
    chem_name = as.character(data$chem_name),
    output_unit = column_or_na(data, "output_unit"),
    stringsAsFactors = FALSE
  )
  analytes <- unique(analyte)
  analytes <- analytes[
    order(
      is.na(analytes$chem_group),
      analytes$chem_group,
      analytes$chem_name,
      analytes$output_unit
    ),
    ,
    drop = FALSE
  ]
  rownames(analytes) <- NULL
  col_id <- match(row_keys(analyte), row_keys(analytes))

  # --- rows: one per location and id_cols combination
  keys <- dplyr::as_tibble(as.data.frame(data)[lead])
  if (include_zone) {
    keys[[zone_name]] <- tidyr::replace_na(as.character(keys[[zone_name]]), "")
    check_one_zone_per_location(
      keys$location_code,
      keys[[zone_name]],
      zone_name
    )
  }
  rows <- dplyr::distinct(keys)
  rows <- rows[do.call(order, unname(as.list(rows))), , drop = FALSE]
  row_id <- match(row_keys(keys), row_keys(rows))

  # --- one result per cell: the highest detect, else the highest LOR
  n_row <- nrow(rows)
  n_col <- nrow(analytes)
  cell <- (row_id - 1L) * n_col + col_id
  ord <- order(cell, nd, -conc)
  kept <- ord[!duplicated(cell[ord])]
  # A result repeated word for word in the export loses nothing by being
  # shown once. Only results that disagree are worth a warning.
  results <- unique(data.frame(cell = cell, conc = conc, nd = nd))
  shared <- length(unique(results$cell[duplicated(results$cell)]))
  if (shared > 0) {
    warning(
      shared,
      " cell(s) of the table hold results that differ - a re-analysis, or ",
      "samples `id_cols` does not tell apart. The highest detected result ",
      "is shown, or the highest LOR where none was detected. Add a column ",
      "such as \"sample_code\" or \"date\" to `id_cols` to give each sample ",
      "a row of its own.",
      call. = FALSE
    )
  }

  at <- cbind(row_id[kept], col_id[kept])
  conc_m <- matrix(NA_real_, n_row, n_col)
  nd_m <- matrix(NA, n_row, n_col)
  fill_m <- matrix(NA_character_, n_row, n_col)
  conc_m[at] <- conc[kept]
  nd_m[at] <- nd[kept]

  # --- guideline sets
  k <- length(sets)
  guideline_value <- matrix(NA_real_, k, n_col)
  guideline_text <- matrix("", k, n_col)
  set_labels <- character(0)

  if (k > 0) {
    crit <- set_matrix(data, sets, as.numeric)
    exceeds <- set_matrix(
      data,
      vapply(sets, function(s) comparison_columns(s)[["exceedance"]], ""),
      as.logical
    )
    lor_above <- set_matrix(
      data,
      vapply(sets, function(s) comparison_columns(s)[["lor"]], ""),
      as.logical
    )

    # A non-detect is shaded only where `highlight_lor` asks for it, whatever
    # lor_as_exceedance made of it in the join.
    hit <- (exceeds & !nd) | (highlight_lor & lor_above & nd)
    hit[is.na(hit)] <- FALSE
    winner <- highest_exceeded(hit, crit)
    fill_m[at] <- sets[winner[kept]]

    joined_lor <- sum(rowSums(exceeds[kept, , drop = FALSE] & nd[kept],
      na.rm = TRUE
    ) > 0)
    if (!highlight_lor && joined_lor > 0) {
      message(
        joined_lor,
        " non-detect(s) were joined as exceedances ",
        "(join_action_levels(lor_as_exceedance = TRUE)) but are not shaded, ",
        "being non-detects. Pass highlight_lor = TRUE to shade them."
      )
    }

    for (j in seq_len(k)) {
      by_column <- split(crit[, j], factor(col_id, levels = seq_len(n_col)))
      for (cc in seq_len(n_col)) {
        v <- by_column[[cc]]
        v <- v[!is.na(v)]
        if (length(v) == 0) {
          next
        }
        # Unit conversion leaves floating point noise on equal guidelines.
        if (length(unique(signif(v, 10))) == 1) {
          guideline_value[j, cc] <- v[[1]]
          guideline_text[j, cc] <- format_reported(v[[1]])
        } else {
          guideline_text[j, cc] <- paste(
            format_reported(min(v)),
            "-",
            format_reported(max(v))
          )
        }
      }
    }

    recorded <- recorded_set_names(list(data), sets)
    set_labels <- vapply(
      seq_len(k),
      function(j) criteria_set_label(recorded[[j]], sets[[j]], several = TRUE),
      character(1)
    )
    # Two sets joined under one name are told apart by their columns.
    clash <- duplicated(set_labels) | duplicated(set_labels, fromLast = TRUE)
    set_labels[clash] <- paste0(set_labels[clash], " (", sets[clash], ")")
  }

  list(
    ids = rows,
    id_labels = c(if (include_zone) zone_label, location_label, names(ids)),
    include_zone = include_zone,
    analytes = analytes,
    conc = conc_m,
    nd = nd_m,
    fill = fill_m,
    sets = sets,
    set_labels = set_labels,
    set_colours = unname(resolve_set_colours(criteria_colours, sets, set_labels)),
    guideline_value = guideline_value,
    guideline_text = guideline_text,
    highlight_lor = highlight_lor
  )
}


#' Settle the columns written beside the location
#'
#' @param id_cols the `id_cols` argument
#' @param named whether it was passed, rather than left at its default
#' @param data the results
#' @param exclude columns already written in a place of their own
#' @returns a character vector of column names, named by their headings
#' @noRd
resolve_id_cols <- function(id_cols, named, data, exclude) {
  if (is.null(id_cols) || length(id_cols) == 0) {
    return(stats::setNames(character(0), character(0)))
  }
  if (!is.character(id_cols)) {
    stop(
      "`id_cols` must be a character vector of column names, e.g. ",
      "c(\"date\", \"sample_code\")."
    )
  }

  headings <- names(id_cols)
  if (is.null(headings)) {
    headings <- rep("", length(id_cols))
  }
  headings[is.na(headings)] <- ""
  cols <- unname(id_cols)

  keep <- !is.na(cols) & nzchar(cols) & !cols %in% exclude & !duplicated(cols)
  cols <- cols[keep]
  headings <- headings[keep]

  absent <- setdiff(cols, names(data))
  if (length(absent) > 0) {
    if (named) {
      hints <- unique(unlist(lapply(absent, near_names, names(data))))
      stop(
        "`id_cols` names column(s) `data` does not have: ",
        toString(absent),
        ".",
        if (length(hints) > 0) {
          paste0(" Did you mean ", toString(hints), "?")
        } else {
          ""
        },
        call. = FALSE
      )
    }
    message(
      "Not in `data`, so left out of the table: ",
      toString(absent),
      ". Name the columns to show beside the location with `id_cols`."
    )
    present <- !cols %in% absent
    cols <- cols[present]
    headings <- headings[present]
  }

  auto <- !nzchar(headings)
  headings[auto] <- vapply(cols[auto], id_heading, character(1))
  stats::setNames(cols, headings)
}


#' The heading an id column is written under when not given one
#'
#' @param col a column name
#' @returns a single string
#' @noRd
id_heading <- function(col) {
  dict <- c(RESULTS_ID_LABELS, TABLE_LABELS)
  if (col %in% names(dict)) dict[[col]] else humanise(col)
}

# Headings for the columns a results table is most often keyed by, in the
# ESdat and EQuIS spellings. Kept apart from TABLE_LABELS so the summary
# tables create_gt() formats keep their own.
RESULTS_ID_LABELS <- c(
  date = "Sample Date",
  sampled_date_time = "Sample Date",
  sample_code = "Sample ID",
  sys_sample_code = "Sample ID",
  field_id = "Field ID",
  lab_report_number = "Lab Report",
  lab_sdg = "Lab Report",
  sdg = "SDG",
  sample_type = "Sample Type",
  matrix_code = "Matrix",
  start_depth = "Depth From",
  end_depth = "Depth To",
  sample_depth = "Depth"
)


#' Columns a misspelt name may have meant
#'
#' @param x the name given
#' @param choices the names available
#' @returns up to three of `choices`
#' @noRd
near_names <- function(x, choices) {
  utils::head(
    agrep(x, choices, max.distance = 0.2, ignore.case = TRUE, value = TRUE),
    3
  )
}


#' A key for each row of a data frame, for matching rows by value
#'
#' @param df a data frame
#' @returns a character vector, one per row; missing values match each other
#' @noRd
row_keys <- function(df) {
  parts <- lapply(df, function(x) {
    s <- as.character(x)
    s[is.na(x)] <- "\u001f"
    s
  })
  do.call(paste, c(unname(parts), sep = "\r"))
}


#' Columns of a data frame side by side as a matrix
#'
#' @param data a data frame
#' @param cols the columns to take
#' @param as the function each is coerced with
#' @returns a matrix with a row per row of `data` and a column per `cols`
#' @noRd
set_matrix <- function(data, cols, as) {
  matrix(
    unlist(
      lapply(cols, function(col) suppressWarnings(as(data[[col]]))),
      use.names = FALSE
    ),
    nrow = nrow(data)
  )
}


#' The guideline set each result is shaded for
#'
#' Of the sets a result exceeds, the one with the highest guideline:
#' exceeding it implies exceeding the lower ones. A tie goes to the set named
#' first.
#'
#' @param hit logical matrix, one row per result and one column per set
#' @param crit numeric matrix of the guidelines, the same shape
#' @returns an integer vector, the winning column for each result, `NA` where
#'   none was exceeded
#' @noRd
highest_exceeded <- function(hit, crit) {
  winner <- rep(NA_integer_, nrow(hit))
  best <- rep(-Inf, nrow(hit))
  for (j in seq_len(ncol(hit))) {
    higher <- hit[, j] & !is.na(crit[, j]) & crit[, j] > best
    winner[higher] <- j
    best[higher] <- crit[higher, j]
  }
  winner
}


# Default fills for the guideline sets, in criteria_col order: the warm tints
# of the trend palette first, then cooler ones. No greens, which the trend
# sheets use for a favourable result.
RESULTS_SET_FILLS <- c("#FBD08A", "#F0A868", "#E6A0A0", "#9DC3E6", "#C9A0DC")

# A non-detect's text: faded against the detects, darker than the "NC" grey
# of the trend sheets so it still reads.
RESULTS_ND_FONT <- "#808080"


#' Resolve each guideline set's fill colour
#'
#' @param colours the `criteria_colours` argument
#' @param sets the sets' columns
#' @param labels the names the sets are shown under
#' @returns a character vector of colours, one per set, named by set column
#' @noRd
resolve_set_colours <- function(colours, sets, labels) {
  k <- length(sets)
  if (k == 0) {
    return(stats::setNames(character(0), character(0)))
  }
  palette <- RESULTS_SET_FILLS
  out <- stats::setNames(palette[(seq_len(k) - 1) %% length(palette) + 1], sets)
  if (is.null(colours)) {
    if (k > length(palette)) {
      warning(
        "More than ", length(palette), " guideline sets, so the default ",
        "colours repeat. Give each set its own in `criteria_colours`.",
        call. = FALSE
      )
    }
    return(out)
  }

  colours <- unlist(colours, use.names = TRUE)
  if (!is.character(colours) || length(colours) == 0) {
    stop(
      "`criteria_colours` must be a character vector or list of colours, ",
      "or NULL."
    )
  }
  nms <- names(colours)
  if (is.null(nms) || any(is.na(nms) | !nzchar(nms))) {
    if (length(colours) != k) {
      stop(glue::glue(
        "`criteria_colours` is unnamed, so it must hold {k} colour(s), one ",
        "per guideline set in the order {toString(labels)}. Got ",
        "{length(colours)}. Name them instead to set only some."
      ))
    }
    out[] <- unname(colours)
    return(out)
  }

  where <- match(nms, sets)
  where[is.na(where)] <- match(nms[is.na(where)], labels)
  if (anyNA(where)) {
    stop(
      "`criteria_colours` names no guideline set in the table: ",
      toString(nms[is.na(where)]),
      ". Name a set by its column (",
      toString(sets),
      ") or by its name (",
      toString(labels),
      ")."
    )
  }
  out[where] <- unname(colours)
  out
}


#' Stop unless an argument is TRUE or FALSE
#'
#' @param x the argument's value
#' @param arg the argument's name
#' @returns `NULL`, invisibly
#' @noRd
check_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(NULL)
}


#' What kind of value an id column holds, for writing it
#'
#' A date-time with no time of day in it is a date: [data_processor()]'s
#' `date` is a date-time floored to midnight.
#'
#' @param x the column
#' @returns `"date"`, `"datetime"`, `"number"` or `"text"`
#' @noRd
id_kind <- function(x) {
  if (inherits(x, "Date")) {
    return("date")
  }
  if (inherits(x, "POSIXt")) {
    times <- format(x[!is.na(x)], "%H:%M:%S")
    return(if (all(times == "00:00:00")) "date" else "datetime")
  }
  if (is.numeric(x)) "number" else "text"
}


#' An id column as the text a table shows
#'
#' @param x the column
#' @returns a character vector, `""` where `x` is missing
#' @noRd
format_id_values <- function(x) {
  out <- switch(
    id_kind(x),
    date = format(x, "%d/%m/%Y"),
    datetime = format(x, "%d/%m/%Y %H:%M"),
    number = format_reported(x),
    as.character(x)
  )
  out[is.na(x)] <- ""
  out
}


#' Each result as the text a table shows
#'
#' @param conc,nd the `conc` and `nd` matrices of a crosstab
#' @returns a character matrix of the same shape: the result, `<` and the LOR
#'   for a non-detect, or `-` where there is no result
#' @noRd
result_cell_text <- function(conc, nd) {
  value <- format_reported(as.vector(conc))
  out <- ifelse(as.vector(nd) %in% TRUE, paste0("<", value), value)
  out[is.na(as.vector(conc))] <- STATS_NA
  matrix(out, nrow = nrow(conc), ncol = ncol(conc))
}


#' The key to a results table
#'
#' One row per thing a reader has to be told, shared by the source notes of
#' [results_table()] and the legend sheet of [results_table_to_excel()].
#'
#' @param xtab a crosstab from `results_crosstab()`
#' @returns a data frame of `key`, the text of the key cell; `meaning`;
#'   `fill`, the key cell's fill or `NA`; and `bold` and `grey`, its text
#' @noRd
results_legend <- function(xtab) {
  k <- length(xtab$sets)
  key <- c("Bold", "<LOR", STATS_NA)
  meaning <- c(
    "Detected result",
    "Not detected: less than the limit of reporting (LOR) shown",
    "Not analysed"
  )
  fill <- rep(NA_character_, 3)
  bold <- c(TRUE, FALSE, FALSE)
  grey <- c(FALSE, TRUE, TRUE)

  if (k > 0) {
    key <- c(key, xtab$set_labels)
    meaning <- c(
      meaning,
      paste0("Exceeds the ", xtab$set_labels, " guideline")
    )
    fill <- c(fill, xtab$set_colours)
    bold <- c(bold, rep(TRUE, k))
    grey <- c(grey, rep(FALSE, k))
  }
  if (k > 1) {
    key <- c(key, "")
    meaning <- c(
      meaning,
      paste(
        "Where a result exceeds more than one guideline, it is shaded in the",
        "colour of the highest guideline it exceeds."
      )
    )
    fill <- c(fill, NA_character_)
    bold <- c(bold, FALSE)
    grey <- c(grey, FALSE)
  }
  if (k > 0 && xtab$highlight_lor) {
    key <- c(key, "<LOR")
    meaning <- c(
      meaning,
      paste(
        "Not detected, but the LOR is above the guideline of that colour, so",
        "the result cannot be shown to meet it"
      )
    )
    fill <- c(fill, xtab$set_colours[[1]])
    bold <- c(bold, FALSE)
    grey <- c(grey, TRUE)
  }

  data.frame(
    key = key,
    meaning = meaning,
    fill = fill,
    bold = bold,
    grey = grey,
    stringsAsFactors = FALSE
  )
}


#' Escape text for writing into HTML
#'
#' @param x character vector
#' @returns `x` with `&`, `<` and `>` escaped
#' @noRd
escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}


#' Build the gt table from a crosstab
#'
#' The guideline sets are the first rows of the body, shaded in their
#' colours, since gt has no header row to hold a value per column. The
#' analyte's unit goes under its name in the column label.
#'
#' @param xtab a crosstab from `results_crosstab()`
#' @param header_fill,header_font,location_fill,location_font as for
#'   [results_table()]
#' @returns a `gt_tbl`
#' @noRd
results_gt <- function(
  xtab,
  header_fill,
  header_font,
  location_fill,
  location_font
) {
  n_lead <- length(xtab$id_labels)
  n_col <- ncol(xtab$conc)
  n_row <- nrow(xtab$conc)
  k <- length(xtab$sets)
  lead_names <- paste0(".id_", seq_len(n_lead))
  analyte_names <- paste0(".analyte_", seq_len(n_col))

  set_rows <- matrix("", k, n_lead)
  set_rows[, 1] <- xtab$set_labels
  ids <- matrix(
    unlist(lapply(xtab$ids, format_id_values), use.names = FALSE),
    nrow = n_row
  )
  body <- rbind(
    cbind(set_rows, xtab$guideline_text),
    cbind(ids, result_cell_text(xtab$conc, xtab$nd))
  )
  body <- as.data.frame(body, stringsAsFactors = FALSE)
  names(body) <- c(lead_names, analyte_names)

  tbl <- gt::gt(body)

  units <- xtab$analytes$output_unit
  analyte_labels <- paste0(
    escape_html(xtab$analytes$chem_name),
    ifelse(is.na(units), "", paste0("<br>", escape_html(units)))
  )
  tbl <- gt::cols_label(
    tbl,
    .list = c(
      stats::setNames(as.list(xtab$id_labels), lead_names),
      stats::setNames(lapply(analyte_labels, gt::html), analyte_names)
    )
  )

  # The groups are contiguous, the columns having been sorted by group.
  groups <- xtab$analytes$chem_group
  groups[is.na(groups)] <- ""
  runs <- rle(groups)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L
  spanned <- FALSE
  for (i in seq_along(runs$values)) {
    if (nzchar(runs$values[[i]])) {
      tbl <- gt::tab_spanner(
        tbl,
        label = runs$values[[i]],
        columns = analyte_names[starts[[i]]:ends[[i]]],
        id = paste0("chem_group_", i)
      )
      spanned <- TRUE
    }
  }

  tbl <- gt::cols_align(tbl, align = "left", columns = lead_names)
  tbl <- gt::cols_align(tbl, align = "right", columns = analyte_names)

  tbl <- gt::tab_options(
    tbl,
    table.font.size = gt::px(12),
    column_labels.background.color = header_fill,
    column_labels.font.weight = "bold",
    data_row.padding = gt::px(3),
    table_body.hlines.color = "#D9D9D9",
    table_body.vlines.style = "solid",
    table_body.vlines.color = "#D9D9D9",
    table_body.vlines.width = gt::px(1),
    source_notes.font.size = gt::px(11)
  )
  header_cells <- list(gt::cells_column_labels())
  if (spanned) {
    header_cells <- c(header_cells, list(gt::cells_column_spanners()))
  }
  tbl <- gt::tab_style(
    tbl,
    style = gt::cell_text(color = header_font, weight = "bold"),
    locations = header_cells
  )

  # --- the guideline rows
  for (j in seq_len(k)) {
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_fill(color = xtab$set_colours[[j]]),
      locations = gt::cells_body(rows = j)
    )
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = lead_names[[1]], rows = j)
    )
  }
  if (k > 0) {
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_borders(
        sides = "bottom",
        color = header_fill,
        weight = gt::px(2)
      ),
      locations = gt::cells_body(rows = k)
    )
  }

  if (n_row > 0) {
    result_rows <- k + seq_len(n_row)

    # --- the zone and location, as the house sheets set them
    id_style <- function(fill, font) {
      list(gt::cell_fill(color = fill), gt::cell_text(color = font, weight = "bold"))
    }
    if (xtab$include_zone) {
      tbl <- gt::tab_style(
        tbl,
        style = id_style(header_fill, header_font),
        locations = gt::cells_body(columns = lead_names[[1]], rows = result_rows)
      )
      tbl <- gt::tab_style(
        tbl,
        style = id_style(location_fill, location_font),
        locations = gt::cells_body(columns = lead_names[[2]], rows = result_rows)
      )
    } else {
      tbl <- gt::tab_style(
        tbl,
        style = id_style(header_fill, header_font),
        locations = gt::cells_body(columns = lead_names[[1]], rows = result_rows)
      )
    }

    # --- the results: bold detects, grey non-detects, shaded exceedances
    for (cc in seq_len(n_col)) {
      has <- !is.na(xtab$conc[, cc])
      nd <- xtab$nd[, cc] %in% TRUE
      column <- analyte_names[[cc]]
      detects <- which(has & !nd)
      faded <- which(!has | nd)
      if (length(detects) > 0) {
        tbl <- gt::tab_style(
          tbl,
          style = gt::cell_text(weight = "bold"),
          locations = gt::cells_body(columns = column, rows = k + detects)
        )
      }
      if (length(faded) > 0) {
        tbl <- gt::tab_style(
          tbl,
          style = gt::cell_text(color = RESULTS_ND_FONT),
          locations = gt::cells_body(columns = column, rows = k + faded)
        )
      }
      for (j in seq_len(k)) {
        shaded <- which(xtab$fill[, cc] %in% xtab$sets[[j]])
        if (length(shaded) > 0) {
          tbl <- gt::tab_style(
            tbl,
            style = gt::cell_fill(color = xtab$set_colours[[j]]),
            locations = gt::cells_body(columns = column, rows = k + shaded)
          )
        }
      }
    }
  }

  legend <- results_legend(xtab)
  for (i in seq_len(nrow(legend))) {
    key_style <- c(
      if (!is.na(legend$fill[[i]])) {
        paste0(
          "background-color:", legend$fill[[i]],
          ";padding:0 4px;border:1px solid #BFBFBF"
        )
      },
      if (legend$bold[[i]]) "font-weight:bold",
      if (legend$grey[[i]]) paste0("color:", RESULTS_ND_FONT)
    )
    note <- if (nzchar(legend$key[[i]])) {
      paste0(
        "<span style=\"", paste(key_style, collapse = ";"), "\">",
        escape_html(legend$key[[i]]),
        "</span>: ",
        escape_html(legend$meaning[[i]])
      )
    } else {
      escape_html(legend$meaning[[i]])
    }
    tbl <- gt::tab_source_note(tbl, gt::html(note))
  }

  tbl
}

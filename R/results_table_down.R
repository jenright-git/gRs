# The analytes-down layout of results_table() and results_table_to_excel():
# one row per analyte, grouped by chemical group, with the guideline sets as
# columns beside them, and the samples - then the statistics - across the
# top. Built from the same crosstab as the samples-down layout, so the two
# cannot disagree about a cell.


#' Write a crosstab to a worksheet, analytes down
#'
#' Across the top: a banner row naming each `group_by` group over its
#' columns; one row per side column (zone, location, date and so on), named
#' at the right of the left block, its values reading across in the heading
#' colours; and a heading row naming the left block's columns. Down the
#' side, each chemical group under a banner row, then its
#' analytes: name, unit and each set's guideline, then the results.
#'
#' @param wb the workbook to add to
#' @param xtab a crosstab from `results_crosstab()`
#' @param sheet the name to give the sheet
#' @param page the printed page, from `page_spec()`
#' @param title the `title` argument
#' @param notes the notes, from `results_notes()`
#' @param fit_to_width,header_fill,header_font,location_fill,location_font as
#'   for [results_table_to_excel()]
#' @returns `wb`, invisibly, modified in place
#' @noRd
add_results_sheet_down <- function(
  wb,
  xtab,
  sheet,
  page,
  title,
  notes,
  fit_to_width,
  header_fill,
  header_font,
  location_fill,
  location_font
) {
  n_lead <- length(xtab$id_labels)
  n_col <- ncol(xtab$conc)
  n_row <- nrow(xtab$conc)
  k <- length(xtab$sets)
  grouped <- !is.null(xtab$group)
  stats <- xtab$stats

  # --- where everything goes
  n_left <- 2L + k
  left_cols <- seq_len(n_left)
  axis <- xtab$axis[xtab$axis$kind != "banner", , drop = FALSE]
  entry_col <- n_left + seq_len(nrow(axis))
  last_col <- max(c(n_left, entry_col))
  is_sample <- axis$kind == "sample"
  sample_col <- integer(n_row)
  sample_col[axis$index[is_sample]] <- entry_col[is_sample]
  stat_col <- entry_col[!is_sample]
  stat_index <- axis$index[!is_sample]
  # A run of samples is broken by every statistic.
  runs <- unname(split(axis$index[is_sample], cumsum(!is_sample)[is_sample]))

  banner_row <- if (grouped) 1L else integer(0)
  id_row <- length(banner_row) + seq_len(n_lead)
  head_row <- length(banner_row) + n_lead + 1L
  top_row <- if (grouped) 1L else id_row[[1]]

  chem <- xtab$analytes$chem_group
  chem[is.na(chem)] <- "Other analytes"
  chem_runs <- rle(chem)
  chem_ends <- cumsum(chem_runs$lengths)
  chem_starts <- chem_ends - chem_runs$lengths + 1L
  # Each chemical group's banner, then its analytes.
  analyte_row <- head_row + seq_len(n_col) + rep(seq_along(chem_runs$lengths), chem_runs$lengths)
  chem_banner_row <- analyte_row[chem_starts] - 1L
  last_row <- max(c(head_row, analyte_row))

  openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)
  header_style <- header_row_style(header_fill, header_font, rotate = FALSE)

  # --- the banner over each group's columns
  if (grouped) {
    openxlsx::mergeCells(wb, sheet, cols = left_cols, rows = banner_row)
    openxlsx::addStyle(wb, sheet, header_style, rows = banner_row, cols = left_cols)
    banner_of <- axis$group
    spans <- rle(ifelse(is.na(banner_of), "", banner_of))
    span_ends <- cumsum(spans$lengths)
    span_starts <- span_ends - spans$lengths + 1L
    for (i in seq_along(spans$values)) {
      if (!nzchar(spans$values[[i]])) {
        next
      }
      cols <- entry_col[span_starts[[i]]:span_ends[[i]]]
      openxlsx::writeData(
        wb,
        sheet,
        spans$values[[i]],
        startCol = cols[[1]],
        startRow = banner_row
      )
      if (length(cols) > 1) {
        openxlsx::mergeCells(wb, sheet, cols = cols, rows = banner_row)
      }
      openxlsx::addStyle(
        wb,
        sheet,
        group_banner_style(location_fill, location_font),
        rows = banner_row,
        cols = cols,
        gridExpand = TRUE
      )
    }
  }

  # --- a row per side column: its name at the right of the left block, then
  # each sample's value
  id_frame <- excel_id_frame(xtab$ids)
  location_j <- if (xtab$include_zone) 2L else 1L
  for (j in seq_len(n_lead)) {
    r <- id_row[[j]]
    openxlsx::writeData(wb, sheet, xtab$id_labels[[j]], startCol = 1, startRow = r)
    openxlsx::mergeCells(wb, sheet, cols = left_cols, rows = r)
    openxlsx::addStyle(
      wb,
      sheet,
      down_label_style(header_fill, header_font),
      rows = r,
      cols = left_cols,
      gridExpand = TRUE
    )

    # Set as the other headings are; the location beside a zone in its
    # lighter fill, as on a samples_down sheet.
    beside_zone <- xtab$include_zone && j == location_j
    style <- down_id_style(
      if (beside_zone) location_fill else header_fill,
      if (beside_zone) location_font else header_font,
      num_fmt = id_number_format(xtab$ids[[j]])
    )
    for (samples in runs) {
      first_col <- sample_col[[samples[[1]]]]
      write_row(wb, sheet, id_frame[[j]][samples], row = r, col = first_col)
      if (xtab$merge_cells) {
        merge_row_runs(wb, sheet, xtab$merge_keys[[j]][samples], row = r, first_col = first_col)
      }
    }
    if (n_row > 0) {
      openxlsx::addStyle(wb, sheet, style, rows = r, cols = sample_col, gridExpand = TRUE)
    }
  }

  # --- the heading row: the left block's columns, a band over the samples
  write_row(wb, sheet, c("Analyte", "Unit", xtab$set_labels), row = head_row, col = 1)
  openxlsx::addStyle(wb, sheet, header_style, rows = head_row, cols = 1:2, gridExpand = TRUE)
  for (j in seq_len(k)) {
    openxlsx::addStyle(
      wb,
      sheet,
      down_set_heading_style(xtab$set_colours[[j]]),
      rows = head_row,
      cols = 2L + j
    )
  }
  if (n_row > 0) {
    openxlsx::addStyle(wb, sheet, header_style, rows = head_row, cols = sample_col, gridExpand = TRUE)
  }

  # --- each statistic's heading, upright down the header rows
  for (i in seq_along(stat_col)) {
    s <- stat_index[[i]]
    set <- stats$set[[s]]
    from <- if (is.na(stats$scope[[s]])) top_row else id_row[[1]]
    openxlsx::writeData(wb, sheet, stats$label[[s]], startCol = stat_col[[i]], startRow = from)
    openxlsx::mergeCells(wb, sheet, cols = stat_col[[i]], rows = from:head_row)
    openxlsx::addStyle(
      wb,
      sheet,
      down_stat_heading_style(
        if (is.na(set)) RESULTS_STATS_FILL else xtab$set_colours[[set]]
      ),
      rows = from:head_row,
      cols = stat_col[[i]],
      gridExpand = TRUE
    )
  }

  # --- each chemical group under its banner
  for (i in seq_along(chem_runs$values)) {
    r <- chem_banner_row[[i]]
    openxlsx::writeData(wb, sheet, chem_runs$values[[i]], startCol = 1, startRow = r)
    openxlsx::mergeCells(wb, sheet, cols = seq_len(last_col), rows = r)
    openxlsx::addStyle(
      wb,
      sheet,
      down_chem_banner_style(header_fill, header_font),
      rows = r,
      cols = seq_len(last_col),
      gridExpand = TRUE
    )
  }

  # --- the left block: name, unit and each set's guideline
  units <- xtab$analytes$output_unit
  units[is.na(units)] <- ""
  for (i in seq_along(chem_runs$values)) {
    analytes <- chem_starts[[i]]:chem_ends[[i]]
    left <- data.frame(
      name = xtab$analytes$chem_name[analytes],
      unit = units[analytes],
      stringsAsFactors = FALSE
    )
    for (j in seq_len(k)) {
      left[[paste0("set_", j)]] <- xtab$guideline_value[j, analytes]
    }
    openxlsx::writeData(
      wb,
      sheet,
      left,
      startCol = 1,
      startRow = analyte_row[[analytes[[1]]]],
      colNames = FALSE,
      keepNA = FALSE
    )
  }
  for (j in seq_len(k)) {
    ranged <- which(is.na(xtab$guideline_value[j, ]) & nzchar(xtab$guideline_text[j, ]))
    for (cc in ranged) {
      openxlsx::writeData(
        wb,
        sheet,
        xtab$guideline_text[j, cc],
        startCol = 2L + j,
        startRow = analyte_row[[cc]]
      )
    }
    openxlsx::addStyle(
      wb,
      sheet,
      guideline_row_style(xtab$set_colours[[j]], label = FALSE),
      rows = analyte_row,
      cols = 2L + j,
      gridExpand = TRUE
    )
  }
  openxlsx::addStyle(wb, sheet, stats_body_style(halign = "left"), rows = analyte_row, cols = 1, gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet, stats_body_style(halign = "center"), rows = analyte_row, cols = 2, gridExpand = TRUE)

  # --- the results: detects as numbers, then the text cells over the gaps
  nd <- xtab$nd
  nd[is.na(nd)] <- FALSE
  detected <- !is.na(xtab$conc) & !nd
  numbers <- xtab$conc
  numbers[!detected] <- NA
  text <- result_cell_text(xtab$conc, xtab$nd)
  for (i in seq_along(chem_runs$values)) {
    analytes <- chem_starts[[i]]:chem_ends[[i]]
    for (samples in runs) {
      openxlsx::writeData(
        wb,
        sheet,
        as.data.frame(t(numbers[samples, analytes, drop = FALSE])),
        startCol = sample_col[[samples[[1]]]],
        startRow = analyte_row[[analytes[[1]]]],
        colNames = FALSE,
        keepNA = FALSE
      )
      for (cc in analytes) {
        gaps <- rle(!detected[samples, cc])
        gap_ends <- cumsum(gaps$lengths)
        gap_starts <- gap_ends - gaps$lengths + 1L
        for (g in which(gaps$values)) {
          span <- samples[gap_starts[[g]]:gap_ends[[g]]]
          write_row(wb, sheet, text[span, cc], row = analyte_row[[cc]], col = sample_col[[span[[1]]]])
        }
      }
    }
  }

  # One style per look, each laid over every cell it applies to at once.
  shades <- c(NA_character_, xtab$sets)
  for (bold in c(TRUE, FALSE)) {
    for (s in seq_along(shades)) {
      in_shade <- if (is.na(shades[[s]])) {
        is.na(xtab$fill)
      } else {
        !is.na(xtab$fill) & xtab$fill == shades[[s]]
      }
      cells <- which(detected == bold & in_shade, arr.ind = TRUE)
      if (nrow(cells) == 0) {
        next
      }
      openxlsx::addStyle(
        wb,
        sheet,
        result_cell_style(bold = bold, fill = if (s > 1) xtab$set_colours[[s - 1]]),
        rows = analyte_row[cells[, 2]],
        cols = sample_col[cells[, 1]],
        gridExpand = FALSE,
        stack = FALSE
      )
    }
  }

  # --- the statistics, a column each
  for (i in seq_along(stat_col)) {
    s <- stat_index[[i]]
    for (r in seq_along(chem_runs$values)) {
      analytes <- chem_starts[[r]]:chem_ends[[r]]
      write_statistic_cells(
        wb,
        sheet,
        stats,
        s,
        col = stat_col[[i]],
        rows = analyte_row[analytes],
        analytes = analytes
      )
    }
    openxlsx::addStyle(
      wb,
      sheet,
      statistic_style(RESULTS_STATS_FILL, rule = stats$first[[s]], label = FALSE, across = FALSE),
      rows = analyte_row,
      cols = stat_col[[i]],
      gridExpand = TRUE
    )
  }

  write_results_notes(wb, sheet, notes, first_row = last_row + 2L)

  # --- widths, heights, the frozen header and the page
  name_width <- min(45, max(12, nchar(xtab$analytes$chem_name) + 2))
  unit_width <- min(12, max(6, nchar(units) + 2))
  set_widths <- vapply(
    seq_len(k),
    function(j) {
      words <- strsplit(xtab$set_labels[[j]], " ", fixed = TRUE)[[1]]
      min(16, max(9, nchar(c(xtab$guideline_text[j, ], words)) + 2))
    },
    numeric(1)
  )
  # Each sample's column wide enough for its results and for its values in
  # the side rows below the location, which read across.
  side <- setdiff(seq_len(n_lead), seq_len(location_j))
  side_values <- lapply(xtab$ids[side], format_id_values)
  sample_widths <- vapply(
    seq_len(n_row),
    function(i) {
      ids <- vapply(side_values, `[[`, character(1), i)
      min(18, max(7, nchar(c(text[i, ], ids)) + 2))
    },
    numeric(1)
  )
  stat_widths <- vapply(
    stat_index,
    function(s) min(12, max(8, nchar(stats$text[s, ]) + 2)),
    numeric(1)
  )
  openxlsx::setColWidths(
    wb,
    sheet,
    cols = c(left_cols, sample_col, stat_col),
    widths = c(name_width, unit_width, set_widths, sample_widths, stat_widths)
  )

  # Each side row tall enough for its values to wrap within their columns.
  # The location's, merged across its samples, keeps the default.
  id_heights <- rep(15, n_lead)
  for (m in seq_along(side)) {
    lines <- ceiling(nchar(side_values[[m]]) / pmax(sample_widths - 1, 1))
    id_heights[[side[[m]]]] <- max(15, 12 * max(c(1, lines)) + 3)
  }
  if (length(side) > 0) {
    openxlsx::setRowHeights(wb, sheet, rows = id_row[side], heights = id_heights[side])
  }
  heading_lines <- max(c(1, ceiling(nchar(xtab$set_labels) * 0.9 / pmax(set_widths, 1))))
  head_height <- max(15, 12 * heading_lines + 3)
  # Each statistic's heading is upright, merged down the side rows and the
  # heading row: at about 5.5 points a character in 9 point type, the
  # heading row takes up whatever the side rows leave short.
  if (length(stat_index) > 0) {
    upright <- 5.5 * max(nchar(stats$label[stat_index])) + 6
    head_height <- max(head_height, upright - sum(id_heights))
  }
  openxlsx::setRowHeights(wb, sheet, rows = head_row, heights = head_height)

  openxlsx::freezePane(
    wb,
    sheet,
    firstActiveRow = head_row + 1L,
    firstActiveCol = n_left + 1L
  )
  openxlsx::pageSetup(
    wb,
    sheet,
    orientation = page$orientation,
    paperSize = page$code,
    left = 0.25,
    right = 0.25,
    fitToWidth = fit_to_width,
    fitToHeight = FALSE,
    printTitleRows = seq_len(head_row),
    printTitleCols = left_cols
  )
  set_page_title(wb, sheet, title)

  invisible(wb)
}


#' Merge each run of repeated values along a written row
#'
#' The across-the-sheet twin of `merge_column_runs()`.
#'
#' @param wb the workbook to modify
#' @param sheet the worksheet name
#' @param x the row's values, or keys, as written
#' @param row the sheet row
#' @param first_col the sheet column `x` starts in
#' @returns `wb`, invisibly, modified in place
#' @noRd
merge_row_runs <- function(wb, sheet, x, row, first_col) {
  runs <- rle(x)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L
  offset <- first_col - 1L
  for (i in seq_along(runs$lengths)) {
    if (runs$lengths[[i]] > 1) {
      openxlsx::mergeCells(
        wb,
        sheet,
        cols = (starts[[i]] + offset):(ends[[i]] + offset),
        rows = row
      )
    }
  }
  invisible(wb)
}


#' Style for a side column's name, at the right of the left block
#'
#' @param fill,font background and text colours
#' @returns an openxlsx style object
#' @noRd
down_label_style <- function(fill, font) {
  openxlsx::createStyle(
    fgFill = fill,
    fontColour = font,
    fontSize = 9,
    textDecoration = "bold",
    halign = "right",
    valign = "center",
    border = "TopBottomLeftRight",
    borderColour = "#BFBFBF",
    borderStyle = "thin"
  )
}


#' Style for a sample's value in a side column's row
#'
#' Set as the other headings are - `header_row_style(rotate = FALSE)`,
#' reading across and wrapping - with a number format for a date.
#'
#' @param fill,font background and text colours
#' @param num_fmt Excel number format
#' @returns an openxlsx style object
#' @noRd
down_id_style <- function(fill, font, num_fmt = "GENERAL") {
  openxlsx::createStyle(
    fgFill = fill,
    fontColour = font,
    fontSize = 9,
    textDecoration = "bold",
    wrapText = TRUE,
    halign = "center",
    valign = "center",
    numFmt = num_fmt,
    border = "TopBottomLeftRight",
    borderColour = "#BFBFBF",
    borderStyle = "thin"
  )
}


#' Style for a guideline set's heading, in its colour
#'
#' @param fill the set's colour
#' @returns an openxlsx style object
#' @noRd
down_set_heading_style <- function(fill) {
  openxlsx::createStyle(
    fgFill = fill,
    fontSize = 9,
    textDecoration = "bold",
    wrapText = TRUE,
    halign = "center",
    valign = "center",
    border = "TopBottomLeftRight",
    borderColour = "#BFBFBF",
    borderStyle = "thin"
  )
}


#' Style for a statistic's heading, upright
#'
#' @param fill background colour
#' @returns an openxlsx style object
#' @noRd
down_stat_heading_style <- function(fill) {
  openxlsx::createStyle(
    fgFill = fill,
    fontSize = 9,
    textDecoration = "bold",
    textRotation = 90,
    halign = "center",
    valign = "bottom",
    border = "TopBottomLeftRight",
    borderColour = "#BFBFBF",
    borderStyle = "thin"
  )
}


#' Style for a chemical group's banner row
#'
#' @param fill,font background and text colours
#' @returns an openxlsx style object
#' @noRd
down_chem_banner_style <- function(fill, font) {
  openxlsx::createStyle(
    fgFill = fill,
    fontColour = font,
    fontSize = 9,
    textDecoration = "bold",
    halign = "left",
    valign = "center",
    border = "TopBottomLeftRight",
    borderColour = "#BFBFBF",
    borderStyle = "thin"
  )
}


#' Build the gt table from a crosstab, analytes down
#'
#' One row per analyte, in a row group per chemical group, with the
#' guideline sets as filled columns beside the names. Each sample's column
#' is labelled with its date, sample ID and the rest, one to a line, under
#' spanners for its location, zone and `group_by` group - so a location is
#' named once over all its samples whatever `merge_cells` says. The
#' statistics sit at the right under a "Statistics" spanner.
#'
#' @param xtab a crosstab from `results_crosstab()`
#' @param page the printed page, from `page_spec()`
#' @param title the `title` argument
#' @param notes the notes, from `results_notes()`
#' @param header_fill,header_font,location_fill,location_font as for
#'   [results_table()]
#' @returns a `gt_tbl`
#' @noRd
results_gt_down <- function(
  xtab,
  page,
  title,
  notes,
  header_fill,
  header_font,
  location_fill,
  location_font
) {
  n_lead <- length(xtab$id_labels)
  n_col <- ncol(xtab$conc)
  n_row <- nrow(xtab$conc)
  k <- length(xtab$sets)
  stats <- xtab$stats
  location_j <- if (xtab$include_zone) 2L else 1L

  axis <- xtab$axis[xtab$axis$kind != "banner", , drop = FALSE]
  is_sample <- axis$kind == "sample"
  entry_names <- paste0(".entry_", seq_len(nrow(axis)))
  # sprintf() rather than paste0(), which gives ".set_" for no sets at all.
  set_names <- sprintf(".set_%d", seq_len(k))
  sample_name <- character(n_row)
  sample_name[axis$index[is_sample]] <- entry_names[is_sample]
  stat_names <- entry_names[!is_sample]
  stat_index <- axis$index[!is_sample]

  units <- xtab$analytes$output_unit
  cells <- result_cell_text(xtab$conc, xtab$nd)
  body <- data.frame(
    .analyte = xtab$analytes$chem_name,
    .unit = ifelse(is.na(units), "", units),
    stringsAsFactors = FALSE
  )
  for (j in seq_len(k)) {
    body[[set_names[[j]]]] <- xtab$guideline_text[j, ]
  }
  for (m in seq_len(nrow(axis))) {
    body[[entry_names[[m]]]] <- if (is_sample[[m]]) {
      cells[axis$index[[m]], ]
    } else {
      stats$text[axis$index[[m]], ]
    }
  }
  chem <- xtab$analytes$chem_group
  body$.chem_group <- ifelse(is.na(chem), "Other analytes", chem)
  tbl <- gt::gt(body, groupname_col = ".chem_group")

  # --- labels: each sample's values below its location, one to a line
  shown <- lapply(xtab$ids, format_id_values)
  sample_labels <- vapply(
    seq_len(n_row),
    function(i) {
      lines <- vapply(
        setdiff(seq_len(n_lead), seq_len(location_j)),
        function(j) shown[[j]][[i]],
        character(1)
      )
      paste(escape_html(lines[nzchar(lines)]), collapse = "<br>")
    },
    character(1)
  )
  tbl <- gt::cols_label(
    tbl,
    .list = c(
      list(.analyte = "Analyte", .unit = "Unit"),
      stats::setNames(as.list(xtab$set_labels), set_names),
      stats::setNames(lapply(sample_labels, gt::html), sample_name),
      stats::setNames(as.list(stats$label[stat_index]), stat_names)
    )
  )

  # --- spanners: the location over its samples, then the zone, then the
  # group, each over its run of columns
  spanned <- FALSE
  location_ids <- character(0)
  group_ids <- character(0)
  add_spanners <- function(tbl, keys, labels, level, prefix) {
    runs <- rle(keys)
    ends <- cumsum(runs$lengths)
    starts <- ends - runs$lengths + 1L
    made <- character(0)
    for (i in seq_along(runs$values)) {
      if (is.na(runs$values[[i]]) || !nzchar(labels[[starts[[i]]]])) {
        next
      }
      id <- paste0(prefix, i)
      tbl <- gt::tab_spanner(
        tbl,
        label = labels[[starts[[i]]]],
        columns = entry_names[starts[[i]]:ends[[i]]],
        level = level,
        id = id
      )
      made <- c(made, id)
    }
    list(tbl = tbl, ids = made)
  }
  entry_key <- function(j) {
    out <- rep(NA_character_, nrow(axis))
    out[is_sample] <- xtab$merge_keys[[j]][axis$index[is_sample]]
    out
  }
  entry_value <- function(j) {
    out <- rep("", nrow(axis))
    out[is_sample] <- shown[[j]][axis$index[is_sample]]
    out
  }
  if (n_row > 0) {
    made <- add_spanners(tbl, entry_key(location_j), entry_value(location_j), 1, "location_")
    tbl <- made$tbl
    location_ids <- made$ids
    stat_key <- ifelse(is_sample, NA_character_, paste0("stats\r", axis$group))
    made <- add_spanners(tbl, stat_key, rep("Statistics", nrow(axis)), 1, "statistics_")
    tbl <- made$tbl
    level <- 1
    if (xtab$include_zone) {
      level <- level + 1
      made <- add_spanners(tbl, entry_key(1), entry_value(1), level, "zone_")
      tbl <- made$tbl
      location_ids <- c(location_ids, made$ids)
    }
    if (!is.null(xtab$group)) {
      level <- level + 1
      made <- add_spanners(tbl, axis$group, ifelse(is.na(axis$group), "", axis$group), level, "group_")
      tbl <- made$tbl
      group_ids <- made$ids
    }
    spanned <- TRUE
  }

  tbl <- gt::cols_align(tbl, align = "left", columns = ".analyte")
  tbl <- gt::cols_align(tbl, align = "center", columns = ".unit")
  tbl <- gt::cols_align(tbl, align = "right", columns = c(set_names, entry_names))
  tbl <- gt_house_style(tbl, page, header_fill, header_font, spanned)

  if (length(group_ids) > 0) {
    tbl <- gt::tab_style(
      tbl,
      style = list(
        gt::cell_fill(color = location_fill),
        gt::cell_text(color = location_font, weight = "bold")
      ),
      locations = gt::cells_column_spanners(spanners = group_ids)
    )
  }

  # --- the guideline sets: filled columns, headed in their colours
  for (j in seq_len(k)) {
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_fill(color = xtab$set_colours[[j]]),
      locations = gt::cells_body(columns = set_names[[j]])
    )
    tbl <- gt::tab_style(
      tbl,
      style = list(
        gt::cell_fill(color = xtab$set_colours[[j]]),
        gt::cell_text(color = "#000000")
      ),
      locations = gt::cells_column_labels(columns = set_names[[j]])
    )
  }

  # --- the results, an analyte row at a time
  for (cc in seq_len(n_col)) {
    has <- !is.na(xtab$conc[, cc])
    nd <- xtab$nd[, cc] %in% TRUE
    detects <- sample_name[has & !nd]
    faded <- sample_name[!has | nd]
    if (length(detects) > 0) {
      tbl <- gt::tab_style(
        tbl,
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(columns = detects, rows = cc)
      )
    }
    if (length(faded) > 0) {
      tbl <- gt::tab_style(
        tbl,
        style = gt::cell_text(color = RESULTS_ND_FONT),
        locations = gt::cells_body(columns = faded, rows = cc)
      )
    }
    for (j in seq_len(k)) {
      shaded <- sample_name[xtab$fill[, cc] %in% xtab$sets[[j]]]
      if (length(shaded) > 0) {
        tbl <- gt::tab_style(
          tbl,
          style = gt::cell_fill(color = xtab$set_colours[[j]]),
          locations = gt::cells_body(columns = shaded, rows = cc)
        )
      }
    }
  }

  # --- the statistics: grey columns, each exceedance heading in its colour
  if (length(stat_names) > 0) {
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_fill(color = RESULTS_STATS_FILL),
      locations = gt::cells_body(columns = stat_names)
    )
    for (i in seq_along(stat_names)) {
      set <- stats$set[[stat_index[[i]]]]
      tbl <- gt::tab_style(
        tbl,
        style = list(
          gt::cell_fill(
            color = if (is.na(set)) RESULTS_STATS_FILL else xtab$set_colours[[set]]
          ),
          gt::cell_text(color = "#000000")
        ),
        locations = gt::cells_column_labels(columns = stat_names[[i]])
      )
    }
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_borders(
        sides = "left",
        color = RESULTS_STATS_RULE,
        weight = gt::px(2)
      ),
      locations = gt::cells_body(columns = stat_names[stats$first[stat_index]])
    )
  }

  # --- the chemical groups, as the Excel sheet's banners
  tbl <- gt::tab_style(
    tbl,
    style = list(
      gt::cell_fill(color = header_fill),
      gt::cell_text(color = header_font, weight = "bold")
    ),
    locations = gt::cells_row_groups()
  )

  gt_titles_and_notes(tbl, title, notes)
}

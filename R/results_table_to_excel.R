#' Formatted Excel workbook of every result against its guidelines
#'
#' Writes the table [results_table()] builds - one row per sample, one column
#' per analyte, the guideline sets across the top - to a formatted `.xlsx`
#' sheet, laid out as a report table:
#'
#' * across the top, each analyte's chemical group (merged across the group's
#'   columns), its name (set upright, as [mka_to_excel()] sets its analytes)
#'   and its unit, then a row per guideline set giving the guideline, shaded
#'   in the set's colour and named down the side;
#' * down the side, the location and the `id_cols`, the location names set in
#'   white on deep green as on the [mka_to_excel()] and
#'   [summary_stats_to_excel()] sheets;
#' * in the body, a detected result is a bold number, a non-detect is `<` and
#'   its LOR as grey text, `-` marks an analyte the sample was not analysed
#'   for, and a result exceeding a guideline is shaded in that guideline's
#'   colour - the highest guideline it exceeds, where it exceeds more than
#'   one.
#'
#' Detected results stay numbers, so the sheet can still be calculated on.
#' The header block and the identifier columns are frozen, and a second sheet
#' explains the formatting.
#'
#' See [results_table()] for how the rows, columns and shading are decided.
#'
#' @inheritParams results_table
#' @param save_path full file path, including filename, for the workbook. The
#'   directory is created if it does not exist. Defaults to
#'   `"Results Table_<yyyymmdd>.xlsx"` in the working directory.
#' @param merge_zones merge each zone's repeated cells into a single block, so
#'   the zone is named once against its rows. Default `TRUE`. Set `FALSE` to
#'   leave a value in every row, which is what Excel's sort and filter tools
#'   want. Ignored unless `include_zone = TRUE`.
#' @param legend add a `"Legend"` sheet explaining the bold, grey and shaded
#'   cells. Default `TRUE`.
#' @param sheet_name name of the results worksheet. Default `"Results"`. It
#'   cannot be `"Legend"` where that sheet is written too.
#' @param overwrite overwrite `save_path` if it already exists. Default
#'   `TRUE`.
#'
#' @returns `data`, invisibly.
#' @export
#'
#' @examples
#' # One monitoring round, no guidelines
#' gRs_data %>%
#'   dplyr::filter(monitoring_round == monitoring_round[[1]]) %>%
#'   results_table_to_excel(
#'     save_path = tempfile(fileext = ".xlsx"),
#'     id_cols = c("date", "sample_code")
#'   )
#'
#' \dontrun{
#' # Two guideline sets across the top, each shading its exceedances
#' gRs_data %>%
#'   join_action_levels(nemp_99, value_col = "criteria_99") %>%
#'   join_action_levels(nemp_95, value_col = "criteria_95") %>%
#'   results_table_to_excel(criteria_col = c(criteria_99, criteria_95))
#'
#' # Wells grouped by monitoring zone, a field ID in place of the sample ID
#' compared %>%
#'   results_table_to_excel(
#'     id_cols = c("date", "field_id", "lab_report_number"),
#'     include_zone = TRUE
#'   )
#' }
#'
#' @seealso [results_table()] for the same table in a report, and
#'   [join_action_levels()] to join the guideline sets it shows.
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle
#'   addStyle setColWidths setRowHeights freezePane mergeCells
results_table_to_excel <- function(
  data,
  save_path = paste0(
    "Results Table_",
    format(Sys.Date(), "%Y%m%d"),
    ".xlsx"
  ),
  criteria_col = criteria,
  id_cols = c("date", "sample_code", "lab_report_number"),
  highlight_lor = FALSE,
  criteria_colours = NULL,
  include_zone = FALSE,
  merge_zones = TRUE,
  zone_col = monitoring_zone,
  zone_label = "Monitoring Zone",
  location_label = "Monitoring Well",
  header_fill = "#008768",
  header_font = "#FFFFFF",
  location_fill = "#9BBEAF",
  location_font = header_font,
  legend = TRUE,
  sheet_name = "Results",
  overwrite = TRUE
) {
  if (legend && identical(tolower(sheet_name), tolower(LEGEND_SHEET))) {
    stop(glue::glue(
      "`sheet_name` cannot be \"{sheet_name}\" with `legend = TRUE`; ",
      "the legend sheet takes that name."
    ))
  }

  # Built before anything is written, so a call that cannot be tabulated
  # fails without leaving half a workbook behind.
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

  wb <- openxlsx::createWorkbook()
  add_results_sheet(
    wb,
    xtab,
    sheet = sheet_name,
    header_fill = header_fill,
    header_font = header_font,
    location_fill = location_fill,
    location_font = location_font,
    merge_zones = merge_zones
  )
  if (legend) {
    add_results_legend_sheet(wb, xtab, header_fill, header_font)
  }
  save_workbook(wb, save_path, overwrite)

  invisible(data)
}


#' Write a crosstab to a worksheet
#'
#' The header block takes three rows - chemical group, analyte name and unit
#' - and then one row per guideline set. The identifier headings are each
#' merged down the three rows, and each set's name is merged across the
#' identifier columns.
#'
#' @param wb the workbook to add to
#' @param xtab a crosstab from `results_crosstab()`
#' @param sheet the name to give the sheet
#' @param header_fill,header_font,location_fill,location_font,merge_zones as
#'   for [results_table_to_excel()]
#' @returns `wb`, invisibly, modified in place
#' @noRd
add_results_sheet <- function(
  wb,
  xtab,
  sheet,
  header_fill,
  header_font,
  location_fill,
  location_font,
  merge_zones
) {
  n_lead <- length(xtab$id_labels)
  n_col <- ncol(xtab$conc)
  n_row <- nrow(xtab$conc)
  k <- length(xtab$sets)
  lead_cols <- seq_len(n_lead)
  analyte_cols <- n_lead + seq_len(n_col)
  first <- 4L + k

  openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)

  # --- identifier headings, each down the three header rows
  for (j in lead_cols) {
    openxlsx::writeData(
      wb,
      sheet,
      xtab$id_labels[[j]],
      startCol = j,
      startRow = 1
    )
    openxlsx::mergeCells(wb, sheet, cols = j, rows = 1:3)
  }
  openxlsx::addStyle(
    wb,
    sheet,
    header_row_style(header_fill, header_font, rotate = FALSE),
    rows = 1:3,
    cols = lead_cols,
    gridExpand = TRUE
  )

  # --- group, name and unit across the analyte columns
  groups <- xtab$analytes$chem_group
  groups[is.na(groups)] <- ""
  units <- xtab$analytes$output_unit
  units[is.na(units)] <- ""
  write_row(wb, sheet, groups, row = 1, col = n_lead + 1)
  write_row(wb, sheet, xtab$analytes$chem_name, row = 2, col = n_lead + 1)
  write_row(wb, sheet, units, row = 3, col = n_lead + 1)

  # The groups are contiguous, the columns having been sorted by group.
  group_runs <- rle(groups)
  group_ends <- cumsum(group_runs$lengths)
  group_starts <- group_ends - group_runs$lengths + 1L
  for (i in seq_along(group_runs$lengths)) {
    if (group_runs$lengths[[i]] > 1) {
      openxlsx::mergeCells(
        wb,
        sheet,
        cols = n_lead + group_starts[[i]]:group_ends[[i]],
        rows = 1
      )
    }
  }
  openxlsx::addStyle(
    wb,
    sheet,
    header_row_style(header_fill, header_font, rotate = FALSE),
    rows = c(1, 3),
    cols = analyte_cols,
    gridExpand = TRUE
  )
  openxlsx::addStyle(
    wb,
    sheet,
    header_row_style(header_fill, header_font, rotate = TRUE),
    rows = 2,
    cols = analyte_cols,
    gridExpand = TRUE
  )

  # --- a row per guideline set, in its colour
  for (j in seq_len(k)) {
    row <- 3L + j
    colour <- xtab$set_colours[[j]]
    openxlsx::writeData(
      wb,
      sheet,
      xtab$set_labels[[j]],
      startCol = 1,
      startRow = row
    )
    if (n_lead > 1) {
      openxlsx::mergeCells(wb, sheet, cols = lead_cols, rows = row)
    }
    openxlsx::addStyle(
      wb,
      sheet,
      guideline_row_style(colour, label = TRUE),
      rows = row,
      cols = lead_cols,
      gridExpand = TRUE
    )

    # Numbers, so the guideline can be calculated against; a range is text.
    write_row(wb, sheet, xtab$guideline_value[j, ], row = row, col = n_lead + 1)
    ranged <- which(
      is.na(xtab$guideline_value[j, ]) & nzchar(xtab$guideline_text[j, ])
    )
    for (cc in ranged) {
      openxlsx::writeData(
        wb,
        sheet,
        xtab$guideline_text[j, cc],
        startCol = n_lead + cc,
        startRow = row
      )
    }
    openxlsx::addStyle(
      wb,
      sheet,
      guideline_row_style(colour, label = FALSE),
      rows = row,
      cols = analyte_cols,
      gridExpand = TRUE
    )
  }

  if (n_row > 0) {
    rows <- first + seq_len(n_row) - 1L

    # --- identifiers
    openxlsx::writeData(
      wb,
      sheet,
      excel_id_frame(xtab$ids),
      startCol = 1,
      startRow = first,
      colNames = FALSE,
      keepNA = FALSE
    )
    location_col <- if (xtab$include_zone) 2L else 1L
    if (xtab$include_zone) {
      openxlsx::addStyle(
        wb,
        sheet,
        id_body_style(header_fill, header_font),
        rows = rows,
        cols = 1,
        gridExpand = TRUE
      )
      openxlsx::addStyle(
        wb,
        sheet,
        id_body_style(location_fill, location_font),
        rows = rows,
        cols = 2,
        gridExpand = TRUE
      )
      if (merge_zones) {
        merge_column_runs(wb, sheet, xtab$ids[[1]], first_row = first, col = 1)
      }
    } else {
      openxlsx::addStyle(
        wb,
        sheet,
        id_body_style(header_fill, header_font),
        rows = rows,
        cols = 1,
        gridExpand = TRUE
      )
    }
    for (j in setdiff(lead_cols, seq_len(location_col))) {
      openxlsx::addStyle(
        wb,
        sheet,
        stats_body_style(id_number_format(xtab$ids[[j]]), halign = "left"),
        rows = rows,
        cols = j,
        gridExpand = TRUE
      )
    }

    # --- results: detects as numbers, then the text cells over the gaps
    nd <- xtab$nd
    nd[is.na(nd)] <- FALSE
    detected <- !is.na(xtab$conc) & !nd
    numbers <- xtab$conc
    numbers[!detected] <- NA
    openxlsx::writeData(
      wb,
      sheet,
      as.data.frame(numbers),
      startCol = n_lead + 1,
      startRow = first,
      colNames = FALSE,
      keepNA = FALSE
    )
    text <- result_cell_text(xtab$conc, xtab$nd)
    # A run of text cells is written in one go: a never-detected analyte is
    # one call, not one per sample.
    for (cc in seq_len(n_col)) {
      gaps <- rle(!detected[, cc])
      gap_ends <- cumsum(gaps$lengths)
      gap_starts <- gap_ends - gaps$lengths + 1L
      for (i in which(gaps$values)) {
        openxlsx::writeData(
          wb,
          sheet,
          text[gap_starts[[i]]:gap_ends[[i]], cc],
          startCol = n_lead + cc,
          startRow = first + gap_starts[[i]] - 1L
        )
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
          result_cell_style(
            bold = bold,
            fill = if (s > 1) xtab$set_colours[[s - 1]]
          ),
          rows = first - 1L + cells[, 1],
          cols = n_lead + cells[, 2],
          gridExpand = FALSE,
          stack = FALSE
        )
      }
    }
  }

  # --- widths, heights and the frozen header
  id_widths <- vapply(
    lead_cols,
    function(j) {
      if (xtab$include_zone && j == 1) {
        return(20)
      }
      if (j == (if (xtab$include_zone) 2 else 1)) {
        return(16)
      }
      values <- format_id_values(xtab$ids[[j]])
      words <- strsplit(xtab$id_labels[[j]], " ", fixed = TRUE)[[1]]
      min(30, max(10, nchar(c(values, words)) + 2))
    },
    numeric(1)
  )
  text <- result_cell_text(xtab$conc, xtab$nd)
  analyte_widths <- vapply(
    seq_len(n_col),
    function(cc) {
      longest <- max(nchar(c(
        text[, cc],
        xtab$guideline_text[, cc],
        xtab$analytes$output_unit[[cc]]
      )), na.rm = TRUE)
      min(16, max(8, longest + 2))
    },
    numeric(1)
  )
  openxlsx::setColWidths(
    wb,
    sheet,
    cols = c(lead_cols, analyte_cols),
    widths = c(id_widths, analyte_widths)
  )

  # Tall enough for the longest analyte name set upright, at about 5.5 points
  # a character in 9 point bold; and for each group's name to wrap within
  # the columns it spans.
  name_height <- min(250, max(60, 5.5 * max(nchar(xtab$analytes$chem_name))))
  group_lines <- vapply(
    seq_along(group_runs$values),
    function(i) {
      span <- sum(analyte_widths[group_starts[[i]]:group_ends[[i]]])
      ceiling(nchar(group_runs$values[[i]]) * 0.9 / span)
    },
    numeric(1)
  )
  openxlsx::setRowHeights(
    wb,
    sheet,
    rows = 1:2,
    heights = c(max(15, 12 * max(group_lines) + 3), name_height)
  )
  openxlsx::freezePane(
    wb,
    sheet,
    firstActiveRow = first,
    firstActiveCol = n_lead + 1
  )

  invisible(wb)
}


#' Write a vector across a row
#'
#' @param wb the workbook
#' @param sheet the worksheet name
#' @param x the values, one per column
#' @param row the row to write
#' @param col the column to start in
#' @returns `wb`, invisibly, modified in place
#' @noRd
write_row <- function(wb, sheet, x, row, col) {
  frame <- as.data.frame(
    stats::setNames(as.list(x), paste0("V", seq_along(x))),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(
    wb,
    sheet,
    frame,
    startCol = col,
    startRow = row,
    colNames = FALSE,
    keepNA = FALSE
  )
  invisible(wb)
}


#' The identifier columns as written to a sheet
#'
#' A date-time with no time of day in it is written as a date, so Excel
#' shows it as one and sorts it as one.
#'
#' @param ids the `ids` of a crosstab
#' @returns a data frame of the same columns
#' @noRd
excel_id_frame <- function(ids) {
  out <- lapply(ids, function(x) {
    if (id_kind(x) == "date" && inherits(x, "POSIXt")) {
      as.Date(format(x, "%Y-%m-%d"))
    } else if (is.factor(x)) {
      as.character(x)
    } else {
      x
    }
  })
  as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE)
}


#' The Excel number format for an identifier column
#'
#' @param x the column
#' @returns a single format string
#' @noRd
id_number_format <- function(x) {
  switch(
    id_kind(x),
    date = "dd/mm/yyyy",
    datetime = "dd/mm/yyyy hh:mm",
    "GENERAL"
  )
}


#' Style for a guideline set's row
#'
#' @param fill the set's colour
#' @param label whether the cell is the set's name, rather than a guideline
#' @returns an openxlsx style object
#' @noRd
guideline_row_style <- function(fill, label) {
  openxlsx::createStyle(
    fgFill = fill,
    fontSize = 9,
    textDecoration = if (label) "bold" else NULL,
    halign = if (label) "left" else "right",
    valign = "center",
    numFmt = "GENERAL",
    border = "TopBottomLeftRight",
    borderColour = "#BFBFBF",
    borderStyle = "thin"
  )
}


#' Style for a result cell
#'
#' @param bold whether the cell holds a detected result, set in bold; the
#'   rest are set in grey
#' @param fill the colour of the guideline it exceeds, or `NULL`
#' @returns an openxlsx style object
#' @noRd
result_cell_style <- function(bold, fill = NULL) {
  openxlsx::createStyle(
    fgFill = fill,
    fontColour = if (bold) NULL else RESULTS_ND_FONT,
    fontSize = 9,
    textDecoration = if (bold) "bold" else NULL,
    halign = "right",
    valign = "center",
    numFmt = "GENERAL",
    border = "TopBottomLeftRight",
    borderColour = "#D9D9D9",
    borderStyle = "thin"
  )
}


#' Add the sheet explaining a results table's formatting
#'
#' Laid out as [mka_to_excel()]'s legend sheet: each key cell is styled as
#' the cells it explains.
#'
#' @param wb the workbook to add to
#' @param xtab a crosstab from `results_crosstab()`
#' @param header_fill,header_font colours for the header row
#' @returns `wb`, invisibly, modified in place
#' @noRd
add_results_legend_sheet <- function(wb, xtab, header_fill, header_font) {
  legend <- results_legend(xtab)

  openxlsx::addWorksheet(wb, LEGEND_SHEET, gridLines = FALSE)
  openxlsx::writeData(
    wb,
    LEGEND_SHEET,
    data.frame(Key = legend$key, Meaning = legend$meaning),
    headerStyle = legend_header_style(header_fill, header_font)
  )

  for (i in seq_len(nrow(legend))) {
    openxlsx::addStyle(
      wb,
      LEGEND_SHEET,
      openxlsx::createStyle(
        fgFill = if (!is.na(legend$fill[[i]])) legend$fill[[i]],
        fontColour = if (legend$grey[[i]]) RESULTS_ND_FONT,
        fontSize = 10,
        textDecoration = if (legend$bold[[i]]) "bold",
        halign = "center",
        valign = "center",
        border = if (nzchar(legend$key[[i]])) "TopBottomLeftRight",
        borderColour = "#D9D9D9",
        borderStyle = "thin"
      ),
      rows = i + 1,
      cols = 1
    )
    openxlsx::addStyle(
      wb,
      LEGEND_SHEET,
      openxlsx::createStyle(fontSize = 10, valign = "center"),
      rows = i + 1,
      cols = 2
    )
  }
  openxlsx::setColWidths(wb, LEGEND_SHEET, cols = 1:2, widths = c(22, 90))

  invisible(wb)
}

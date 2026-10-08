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
#' A value repeated down a side column is merged into one block
#' (`merge_cells`), and with `group_by` each group of rows sits under a banner
#' across the sheet. The header block and the identifier columns are frozen,
#' and a second sheet explains the formatting.
#'
#' The sheet is set up to print on A3 landscape by default, with the header
#' rows and the side columns repeated on every page, so a table too wide or
#' too long for one page still reads page by page.
#'
#' See [results_table()] for how the rows, columns and shading are decided.
#'
#' @inheritParams results_table
#' @param save_path full file path, including filename, for the workbook. The
#'   directory is created if it does not exist. Defaults to
#'   `"Results Table_<yyyymmdd>.xlsx"` in the working directory.
#' @param merge_cells merge a value repeated down a side column into one
#'   block, so a well is named once against all its samples. Each column's
#'   blocks sit within those of the column to its left - a date shared by two
#'   wells is not merged across them - and no block crosses a `group_by`
#'   banner. Default `TRUE`. Set `FALSE` to write the value on every row,
#'   which is what Excel's sort and filter tools want.
#' @param paper_size,orientation the printed page: `"A3"` (default), `"A2"`,
#'   `"A4"`, `"A5"`, `"letter"`, `"legal"` or `"tabloid"`, and `"landscape"`
#'   (default) or `"portrait"`.
#' @param fit_to_width scale the sheet to print one page wide. Default
#'   `FALSE`, printing at full size with the columns that do not fit carried
#'   onto further pages - each repeating the side columns.
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
#'
#' # Every statistic below the table, non-detects at half their LOR in the
#' # mean, median and standard deviation
#' compared %>%
#'   results_table_to_excel(statistics = TRUE, lor_multiplier = 0.5)
#'
#' # A banner per monitoring round, printed on A4 portrait, one page wide
#' compared %>%
#'   results_table_to_excel(
#'     group_by = "monitoring_round",
#'     paper_size = "A4",
#'     orientation = "portrait",
#'     fit_to_width = TRUE
#'   )
#' }
#'
#' @seealso [results_table()] for the same table in a report, and
#'   [join_action_levels()] to join the guideline sets it shows.
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle
#'   addStyle setColWidths setRowHeights freezePane mergeCells pageSetup
results_table_to_excel <- function(
  data,
  save_path = paste0(
    "Results Table_",
    format(Sys.Date(), "%Y%m%d"),
    ".xlsx"
  ),
  criteria_col = criteria,
  id_cols = c("date", "sample_code", "lab_report_number"),
  group_by = NULL,
  sort_analytes_by = NULL,
  analytes = "all",
  highlight_lor = FALSE,
  criteria_colours = NULL,
  criteria_labels = NULL,
  statistics = FALSE,
  statistics_by_group = FALSE,
  lor_multiplier = 1,
  merge_cells = TRUE,
  layout = "samples_down",
  include_zone = FALSE,
  zone_col = monitoring_zone,
  zone_label = "Monitoring Zone",
  location_label = "Monitoring Well",
  title = NULL,
  notes = TRUE,
  paper_size = "A3",
  orientation = "landscape",
  fit_to_width = FALSE,
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
  check_flag(fit_to_width, "fit_to_width")
  page <- page_spec(paper_size, orientation)
  check_choice(layout, LAYOUTS, "layout")
  check_title(title)
  check_notes(notes)

  # Built before anything is written, so a call that cannot be tabulated
  # fails without leaving half a workbook behind.
  xtab <- results_crosstab(
    data,
    criteria_col = rlang::enquo(criteria_col),
    criteria_named = !missing(criteria_col),
    id_cols = id_cols,
    id_named = !missing(id_cols),
    group_by = group_by,
    sort_analytes_by = sort_analytes_by,
    analytes = analytes,
    highlight_lor = highlight_lor,
    criteria_colours = criteria_colours,
    criteria_labels = criteria_labels,
    statistics = statistics,
    statistics_by_group = statistics_by_group,
    lor_multiplier = lor_multiplier,
    merge_cells = merge_cells,
    include_zone = include_zone,
    zone_name = rlang::quo_name(rlang::enquo(zone_col)),
    zone_label = zone_label,
    location_label = location_label,
    caller = "results_table_to_excel"
  )

  wb <- openxlsx::createWorkbook()
  write_sheet <- if (identical(layout, "analytes_down")) {
    add_results_sheet_down
  } else {
    add_results_sheet
  }
  write_sheet(
    wb,
    xtab,
    sheet = sheet_name,
    page = page,
    title = title,
    notes = results_notes(xtab, notes),
    fit_to_width = fit_to_width,
    header_fill = header_fill,
    header_font = header_font,
    location_fill = location_fill,
    location_font = location_font
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
#' Below it, the rows follow the crosstab's sample axis: each group's banner,
#' its samples and its statistics, then the whole table's statistics. Runs
#' of samples between them are written a block at a time; `row_at` maps
#' each row of the crosstab to the sheet row it is written on. The notes
#' follow the last row.
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
add_results_sheet <- function(
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
  lead_cols <- seq_len(n_lead)
  analyte_cols <- n_lead + seq_len(n_col)
  all_cols <- c(lead_cols, analyte_cols)
  first <- 4L + k

  axis <- xtab$axis
  entry_row <- first - 1L + seq_len(nrow(axis))
  is_sample <- axis$kind == "sample"
  row_at <- integer(n_row)
  row_at[axis$index[is_sample]] <- entry_row[is_sample]
  # A run of samples is broken by every banner and statistic.
  blocks <- unname(split(axis$index[is_sample], cumsum(!is_sample)[is_sample]))
  banner_at <- entry_row[axis$kind == "banner"]
  banner_text <- axis$group[axis$kind == "banner"]
  stat_at <- entry_row[axis$kind == "stat"]
  stat_index <- axis$index[axis$kind == "stat"]
  last_row <- max(c(first - 1L, entry_row))

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

  # The chemical groups are contiguous, the columns having been sorted by
  # group.
  chem_runs <- rle(groups)
  chem_ends <- cumsum(chem_runs$lengths)
  chem_starts <- chem_ends - chem_runs$lengths + 1L
  for (i in seq_along(chem_runs$lengths)) {
    if (chem_runs$lengths[[i]] > 1) {
      openxlsx::mergeCells(
        wb,
        sheet,
        cols = n_lead + chem_starts[[i]]:chem_ends[[i]],
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
    nd <- xtab$nd
    nd[is.na(nd)] <- FALSE
    detected <- !is.na(xtab$conc) & !nd
    numbers <- xtab$conc
    numbers[!detected] <- NA
    text <- result_cell_text(xtab$conc, xtab$nd)
    id_frame <- excel_id_frame(xtab$ids)

    # --- each group's banner, across the whole sheet
    for (i in seq_along(banner_at)) {
      openxlsx::writeData(
        wb,
        sheet,
        banner_text[[i]],
        startCol = 1,
        startRow = banner_at[[i]]
      )
      openxlsx::mergeCells(wb, sheet, cols = all_cols, rows = banner_at[[i]])
      openxlsx::addStyle(
        wb,
        sheet,
        group_banner_style(location_fill, location_font),
        rows = banner_at[[i]],
        cols = all_cols,
        gridExpand = TRUE
      )
    }

    for (rows in blocks) {
      top <- row_at[[rows[[1]]]]

      # --- identifiers, and detects as numbers
      openxlsx::writeData(
        wb,
        sheet,
        id_frame[rows, , drop = FALSE],
        startCol = 1,
        startRow = top,
        colNames = FALSE,
        keepNA = FALSE
      )
      openxlsx::writeData(
        wb,
        sheet,
        as.data.frame(numbers[rows, , drop = FALSE]),
        startCol = n_lead + 1,
        startRow = top,
        colNames = FALSE,
        keepNA = FALSE
      )

      # --- then the text cells over the gaps. A run of them is written in
      # one go: a never-detected analyte is one call, not one per sample.
      for (cc in seq_len(n_col)) {
        gaps <- rle(!detected[rows, cc])
        gap_ends <- cumsum(gaps$lengths)
        gap_starts <- gap_ends - gaps$lengths + 1L
        for (i in which(gaps$values)) {
          openxlsx::writeData(
            wb,
            sheet,
            text[rows[gap_starts[[i]]:gap_ends[[i]]], cc],
            startCol = n_lead + cc,
            startRow = top + gap_starts[[i]] - 1L
          )
        }
      }

      # --- merged blocks down the side columns, nested
      if (xtab$merge_cells) {
        for (j in lead_cols) {
          merge_column_runs(
            wb,
            sheet,
            xtab$merge_keys[[j]][rows],
            first_row = top,
            col = j
          )
        }
      }
    }

    location_col <- if (xtab$include_zone) 2L else 1L
    if (xtab$include_zone) {
      openxlsx::addStyle(
        wb,
        sheet,
        id_body_style(header_fill, header_font),
        rows = row_at,
        cols = 1,
        gridExpand = TRUE
      )
      openxlsx::addStyle(
        wb,
        sheet,
        id_body_style(location_fill, location_font),
        rows = row_at,
        cols = 2,
        gridExpand = TRUE
      )
    } else {
      openxlsx::addStyle(
        wb,
        sheet,
        id_body_style(header_fill, header_font),
        rows = row_at,
        cols = 1,
        gridExpand = TRUE
      )
    }
    for (j in setdiff(lead_cols, seq_len(location_col))) {
      openxlsx::addStyle(
        wb,
        sheet,
        stats_body_style(id_number_format(xtab$ids[[j]]), halign = "left"),
        rows = row_at,
        cols = j,
        gridExpand = TRUE
      )
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
          rows = row_at[cells[, 1]],
          cols = n_lead + cells[, 2],
          gridExpand = FALSE,
          stack = FALSE
        )
      }
    }
  }

  # --- the statistics, a row each, labelled across the side columns
  for (i in seq_along(stat_at)) {
    s <- stat_index[[i]]
    r <- stat_at[[i]]
    rule <- xtab$stats$first[[s]]
    set <- xtab$stats$set[[s]]
    openxlsx::writeData(wb, sheet, xtab$stats$label[[s]], startCol = 1, startRow = r)
    if (n_lead > 1) {
      openxlsx::mergeCells(wb, sheet, cols = lead_cols, rows = r)
    }
    openxlsx::addStyle(
      wb,
      sheet,
      statistic_style(
        fill = if (is.na(set)) RESULTS_STATS_FILL else xtab$set_colours[[set]],
        rule = rule,
        label = TRUE
      ),
      rows = r,
      cols = lead_cols,
      gridExpand = TRUE
    )
    write_statistic_cells(wb, sheet, xtab$stats, s, row = r, cols = analyte_cols)
    openxlsx::addStyle(
      wb,
      sheet,
      statistic_style(fill = RESULTS_STATS_FILL, rule = rule, label = FALSE),
      rows = r,
      cols = analyte_cols,
      gridExpand = TRUE
    )
  }

  write_results_notes(wb, sheet, notes, first_row = last_row + 2L)

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
    seq_along(chem_runs$values),
    function(i) {
      span <- sum(analyte_widths[chem_starts[[i]]:chem_ends[[i]]])
      ceiling(nchar(chem_runs$values[[i]]) * 0.9 / span)
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

  # The header block and the side columns print on every page, so a page
  # carried down or across still says what it holds.
  openxlsx::pageSetup(
    wb,
    sheet,
    orientation = page$orientation,
    paperSize = page$code,
    left = 0.25,
    right = 0.25,
    fitToWidth = fit_to_width,
    fitToHeight = FALSE,
    printTitleRows = seq_len(first - 1L),
    printTitleCols = lead_cols
  )
  set_page_title(wb, sheet, title)

  invisible(wb)
}


#' Write one statistic across its cells
#'
#' Numbers as numbers; a value with its `<`, or nothing to report, as text.
#'
#' @param wb the workbook
#' @param sheet the worksheet name
#' @param stats the statistics, from `results_statistics()`
#' @param s the statistic's row in `stats`
#' @param row,cols where a `samples_down` sheet writes it: one row, across
#'   the analyte columns
#' @param col,rows where an `analytes_down` sheet writes it: one column, down
#'   the analyte rows
#' @param analytes the analyte columns of `stats` to write, in the order of
#'   `cols` or `rows`; all of them by default
#' @returns `wb`, invisibly, modified in place
#' @noRd
write_statistic_cells <- function(
  wb,
  sheet,
  stats,
  s,
  row = NULL,
  cols = NULL,
  col = NULL,
  rows = NULL,
  analytes = seq_len(ncol(stats$value))
) {
  value <- stats$value[s, analytes]
  text <- stats$text[s, analytes]
  across <- !is.null(row)

  if (across) {
    write_row(wb, sheet, value, row = row, col = cols[[1]])
  } else {
    openxlsx::writeData(
      wb,
      sheet,
      value,
      startCol = col,
      startRow = rows[[1]],
      keepNA = FALSE
    )
  }
  # The text cells - a "<" value, or "-" - a run at a time: an exceedance
  # row is mostly "-" where most analytes have no guideline in the set.
  runs <- rle(is.na(value))
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L
  for (i in which(runs$values)) {
    span <- starts[[i]]:ends[[i]]
    if (across) {
      write_row(wb, sheet, text[span], row = row, col = cols[[span[[1]]]])
    } else {
      openxlsx::writeData(
        wb,
        sheet,
        text[span],
        startCol = col,
        startRow = rows[[span[[1]]]]
      )
    }
  }
  invisible(wb)
}


#' Style for a statistic's cells
#'
#' @param fill background colour
#' @param rule whether the row or column starts a block, and takes a medium
#'   rule before it
#' @param label whether the cell is the statistic's name
#' @param across whether the block runs across, its rule above (`TRUE`, a
#'   `samples_down` sheet) or down, its rule to its left
#' @returns an openxlsx style object
#' @noRd
statistic_style <- function(fill, rule, label, across = TRUE) {
  ruled <- if (across) "top" else "left"
  sides <- c("top", "bottom", "left", "right")
  openxlsx::createStyle(
    fgFill = fill,
    fontSize = 9,
    textDecoration = if (label) "bold" else NULL,
    halign = if (label) "left" else "right",
    valign = "center",
    numFmt = "GENERAL",
    border = sides,
    borderColour = ifelse(rule & sides == ruled, RESULTS_STATS_RULE, "#D9D9D9"),
    borderStyle = ifelse(rule & sides == ruled, "medium", "thin")
  )
}


#' Write a results table's notes below it
#'
#' A bold "Notes" line, then the key - each key cell styled as the cells it
#' explains, its meaning beside it - then the plain lines, which run on to
#' the right of their cell.
#'
#' @param wb the workbook
#' @param sheet the worksheet name
#' @param notes the notes, from `results_notes()`
#' @param first_row the sheet row to start on
#' @returns `wb`, invisibly, modified in place
#' @noRd
write_results_notes <- function(wb, sheet, notes, first_row) {
  if (nrow(notes) == 0) {
    return(invisible(wb))
  }
  openxlsx::writeData(wb, sheet, "Notes", startCol = 1, startRow = first_row)
  openxlsx::addStyle(
    wb,
    sheet,
    openxlsx::createStyle(fontSize = 9, textDecoration = "bold"),
    rows = first_row,
    cols = 1
  )

  for (i in seq_len(nrow(notes))) {
    r <- first_row + i
    key <- notes$key[[i]]
    if (!is.na(key) && nzchar(key)) {
      openxlsx::writeData(wb, sheet, key, startCol = 1, startRow = r)
      openxlsx::writeData(wb, sheet, notes$meaning[[i]], startCol = 2, startRow = r)
      openxlsx::addStyle(
        wb,
        sheet,
        openxlsx::createStyle(
          fgFill = if (!is.na(notes$fill[[i]])) notes$fill[[i]],
          fontColour = if (notes$grey[[i]]) RESULTS_ND_FONT,
          fontSize = 9,
          textDecoration = if (notes$bold[[i]]) "bold",
          halign = "center",
          valign = "center",
          border = "TopBottomLeftRight",
          borderColour = "#D9D9D9",
          borderStyle = "thin"
        ),
        rows = r,
        cols = 1
      )
      openxlsx::addStyle(
        wb,
        sheet,
        openxlsx::createStyle(fontSize = 9, valign = "center"),
        rows = r,
        cols = 2
      )
    } else {
      openxlsx::writeData(wb, sheet, notes$meaning[[i]], startCol = 1, startRow = r)
      openxlsx::addStyle(
        wb,
        sheet,
        openxlsx::createStyle(fontSize = 9, valign = "center"),
        rows = r,
        cols = 1
      )
    }
  }
  invisible(wb)
}


#' Put a results table's title in its sheet's page header
#'
#' In the centre, in bold, so it prints at the top of every page without
#' taking a row of the sheet. An "&" is a code in a page header, so one in
#' the title is doubled to print as itself.
#'
#' @param wb the workbook
#' @param sheet the worksheet name
#' @param title the `title` argument
#' @returns `wb`, invisibly, modified in place
#' @noRd
set_page_title <- function(wb, sheet, title) {
  if (is.null(title)) {
    return(invisible(wb))
  }
  openxlsx::setHeaderFooter(
    wb,
    sheet,
    header = c(NA, paste0("&B", gsub("&", "&&", title, fixed = TRUE)), NA)
  )
  invisible(wb)
}


#' Style for a group's banner row
#'
#' @param fill background colour
#' @param font text colour
#' @returns an openxlsx style object
#' @noRd
group_banner_style <- function(fill, font) {
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

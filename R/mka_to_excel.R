#' Coloured Excel summary of Mann-Kendall trends
#'
#' Pivots the output of [mann_kendall_test()] to one row per location and one
#' column per analyte, with the trend written into each cell, then writes it to
#' a formatted `.xlsx` workbook. Cells are filled by trend direction - green
#' for decreasing through to orange for increasing - the analyte headers are
#' rotated so the sheet stays narrow, the headings and location names are set
#' in white on a deep green, and the first row and column are frozen. A second
#' sheet explains what each trend category means.
#'
#' Location/analyte combinations that [mann_kendall_test()] could not test,
#' because they had too few samples or too few detects, are absent from its
#' output and so come through the pivot as `NA`. They are written as
#' `na_label` rather than left blank, so a gap in the table reads as "not
#' calculated" rather than as an oversight.
#'
#' @param data tibble from [mann_kendall_test()]. Only the location, analyte
#'   and trend columns are used; the nested `data` column and the test
#'   statistics are ignored, so there is no need to drop them first.
#' @param save_path full file path, including filename, for the workbook. The
#'   directory is created if it does not exist. Defaults to
#'   `"MKA Trend Summary_<yyyymmdd>.xlsx"` in the working directory.
#' @param trend_colours cell fill colours, as a named character vector or list
#'   of hex codes keyed by trend category, e.g.
#'   `c(Increasing = "#F0A868", Decreasing = "#63BE7B")`. Only the categories
#'   named are changed, so one colour can be overridden without restating the
#'   rest. An unnamed vector is also accepted, and must then hold all seven
#'   colours in the order Decreasing, Probably Decreasing, Stable, No
#'   Significant Trend, Probably Increasing, Increasing, `na_label`. `NULL`
#'   (default) uses the built-in green-to-orange scale.
#' @param font_colours text colours for those same cells, given the same way.
#' @param header_fill background colour for the analyte headings across the
#'   top and the location names down the side, as one hex code. Default
#'   `"#008768"`, a deep green.
#' @param header_font text colour for those same cells. Default white.
#' @param na_label what to write where a location/analyte pair has no trend.
#'   Default `"NC"`, for not calculated.
#' @param location_label heading for the first column. Default
#'   `"Monitoring Well"`.
#' @param sheet_name name of the summary worksheet.
#' @param legend include the second sheet explaining the trend categories.
#' @param overwrite overwrite `save_path` if it already exists.
#' @param location_col Name of the column containing location codes.
#'   Can be provided with or without quotes. Default is location_code
#' @param chem_name_col Name of the column containing chemical/analyte names.
#'   Can be provided with or without quotes. Default is chem_name
#' @param trend_col Name of the column containing the trend category.
#'   Can be provided with or without quotes. Default is trend
#'
#' @returns The wide tibble that was written, invisibly - locations down,
#'   analytes across, trends in the cells.
#' @export
#'
#' @seealso [mann_kendall_heatmap()] for the same table as a plot.
#'
#' @examples
#' trends <- mann_kendall_test(gRs_data)
#'
#' out <- mka_to_excel(trends, save_path = tempfile(fileext = ".xlsx"))
#' out[, 1:3]
#'
#' \dontrun{
#' # Default filename, in the working directory
#' mka_to_excel(trends)
#'
#' # Recolour one category and leave the rest alone
#' mka_to_excel(trends, trend_colours = c(Increasing = "#E06666"))
#'
#' # No legend sheet, blank cells instead of "NC"
#' mka_to_excel(trends, legend = FALSE, na_label = "")
#' }
#'
#' @importFrom dplyr select arrange mutate across all_of tibble
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom rlang enquo quo_name !!
#' @importFrom glue glue
#' @importFrom utils head
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle
#'   addStyle setColWidths setRowHeights freezePane saveWorkbook
mka_to_excel <- function(
  data,
  save_path = paste0(
    "MKA Trend Summary_",
    format(Sys.Date(), "%Y%m%d"),
    ".xlsx"
  ),
  trend_colours = NULL,
  font_colours = NULL,
  header_fill = "#008768",
  header_font = "#FFFFFF",
  na_label = "NC",
  location_label = "Monitoring Well",
  sheet_name = "Trend Summary",
  legend = TRUE,
  overwrite = TRUE,
  location_col = location_code,
  chem_name_col = chem_name,
  trend_col = trend
) {
  loc_col <- rlang::enquo(location_col)
  chem_col <- rlang::enquo(chem_name_col)
  trd_col <- rlang::enquo(trend_col)

  loc_name <- rlang::quo_name(loc_col)
  chem_name_str <- rlang::quo_name(chem_col)
  trend_name <- rlang::quo_name(trd_col)

  absent <- setdiff(c(loc_name, chem_name_str, trend_name), names(data))
  if (length(absent) > 0) {
    stop(glue::glue(
      "`data` has no column(s) {toString(absent)}. ",
      "Pass the output of mann_kendall_test(), or name the columns with ",
      "`location_col`, `chem_name_col` and `trend_col`."
    ))
  }
  if (!is.character(na_label) || length(na_label) != 1) {
    stop("`na_label` must be a single string.")
  }

  long <- dplyr::select(data, !!loc_col, !!chem_col, !!trd_col)

  # pivot_wider() would quietly make list columns out of repeated pairs, and a
  # cell holding two trends cannot be coloured as either.
  pair <- paste(long[[loc_name]], long[[chem_name_str]], sep = " / ")
  if (anyDuplicated(pair) > 0) {
    repeated <- unique(pair[duplicated(pair)])
    stop(glue::glue(
      "`data` has more than one row for ",
      "{toString(utils::head(repeated, 5))}",
      if (length(repeated) > 5) {
        glue::glue(" and {length(repeated) - 5} more")
      } else {
        ""
      },
      ". Each location/analyte pair needs a single trend."
    ))
  }

  wide <- long %>%
    tidyr::pivot_wider(names_from = !!chem_col, values_from = !!trd_col) %>%
    dplyr::arrange(!!loc_col)

  # Analytes read left to right in alphabetical order, whatever order the test
  # happened to return them in.
  chem_cols <- sort(setdiff(names(wide), loc_name))
  wide <- wide[, c(loc_name, chem_cols)] %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(chem_cols),
      ~ tidyr::replace_na(as.character(.x), na_label)
    ))
  names(wide)[1] <- location_label

  fills <- resolve_trend_colours(
    trend_colours,
    rename_na_level(TREND_FILL_DEFAULT, na_label),
    "trend_colours"
  )
  fonts <- resolve_trend_colours(
    font_colours,
    rename_na_level(TREND_FONT_DEFAULT, na_label),
    "font_colours"
  )

  mat <- as.matrix(wide[, -1, drop = FALSE])
  unstyled <- setdiff(unique(as.vector(mat)), names(fills))
  if (length(unstyled) > 0) {
    warning(glue::glue(
      "No colour given for trend value(s) {toString(unstyled)}; ",
      "those cells are left unformatted. Name them in `trend_colours` to ",
      "colour them."
    ))
  }

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet_name, gridLines = FALSE)
  openxlsx::writeData(
    wb,
    sheet_name,
    wide,
    headerStyle = openxlsx::createStyle(
      fgFill = header_fill,
      fontColour = header_font,
      textRotation = 90,
      halign = "center",
      valign = "bottom",
      textDecoration = "bold",
      fontSize = 9,
      border = "TopBottomLeftRight",
      borderColour = "#BFBFBF",
      borderStyle = "thin"
    )
  )

  n_row <- nrow(wide)
  n_col <- ncol(wide)

  if (n_row > 0) {
    openxlsx::addStyle(
      wb,
      sheet_name,
      openxlsx::createStyle(
        fgFill = header_fill,
        fontColour = header_font,
        fontSize = 9,
        textDecoration = "bold",
        halign = "left",
        valign = "center",
        border = "TopBottomLeftRight",
        borderColour = "#D9D9D9",
        borderStyle = "thin"
      ),
      rows = 2:(n_row + 1),
      cols = 1,
      gridExpand = TRUE
    )
  }

  for (lvl in names(fills)) {
    hits <- which(mat == lvl, arr.ind = TRUE)
    if (nrow(hits) == 0) {
      next
    }
    openxlsx::addStyle(
      wb,
      sheet_name,
      trend_cell_style(lvl, fills, fonts, na_label),
      rows = hits[, "row"] + 1,
      cols = hits[, "col"] + 1,
      gridExpand = FALSE,
      stack = FALSE
    )
  }

  openxlsx::setColWidths(wb, sheet_name, cols = 1, widths = 16)
  if (n_col > 1) {
    openxlsx::setColWidths(wb, sheet_name, cols = 2:n_col, widths = 21)
  }
  openxlsx::setRowHeights(wb, sheet_name, rows = 1, heights = 150)
  openxlsx::freezePane(wb, sheet_name, firstActiveRow = 2, firstActiveCol = 2)

  if (legend) {
    add_legend_sheet(wb, fills, fonts, na_label, header_fill, header_font)
  }

  out_dir <- dirname(save_path)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
    message(glue::glue("Created directory: {out_dir}"))
  }
  openxlsx::saveWorkbook(wb, save_path, overwrite = overwrite)
  message(glue::glue("Saved: {basename(save_path)} -> {save_path}"))

  invisible(wide)
}


# Cell fills, running from decreasing (favourable, green) through neutral grey
# to increasing (unfavourable, orange), in the order the legend sheet lists
# them. Keyed by the categories mann_kendall_test() assigns, plus the
# not-calculated level, whose name is swapped for `na_label` before use.
TREND_FILL_DEFAULT <- c(
  "Decreasing" = "#63BE7B",
  "Probably Decreasing" = "#B7E1B9",
  "Stable" = "#f7f2f2",
  "No Significant Trend" = "#EDEDED",
  "Probably Increasing" = "#FBD08A",
  "Increasing" = "#F0A868",
  "NC" = "#FFFFFF"
)

TREND_FONT_DEFAULT <- c(
  "Decreasing" = "#14401F",
  "Probably Decreasing" = "#14401F",
  "Stable" = "#3F3F3F",
  "No Significant Trend" = "#3F3F3F",
  "Probably Increasing" = "#6B4106",
  "Increasing" = "#6B4106",
  "NC" = "#B0B0B0"
)

TREND_MEANING_DEFAULT <- c(
  "Decreasing" = "Statistically significant decreasing trend",
  "Probably Decreasing" = "Weak evidence of a decreasing trend",
  "Stable" = "No trend; low variability (COV based)",
  "No Significant Trend" = "No statistically significant trend detected",
  "Probably Increasing" = "Weak evidence of an increasing trend",
  "Increasing" = "Statistically significant increasing trend",
  "NC" = "Not calculated - insufficient data for the analyte at this well"
)

TREND_INTERPRETATION_DEFAULT <- c(
  "Decreasing" = "Favourable",
  "Probably Decreasing" = "Favourable",
  "Stable" = "Neutral",
  "No Significant Trend" = "Neutral",
  "Probably Increasing" = "Unfavourable",
  "Increasing" = "Unfavourable",
  "NC" = "-"
)

#' Rename the not-calculated level in a default lookup
#'
#' @param x named vector keyed by trend category
#' @param na_label the name to give the "NC" entry
#' @returns `x` with that one name changed
#' @noRd
rename_na_level <- function(x, na_label) {
  names(x)[names(x) == "NC"] <- na_label
  x
}

#' Merge user colours over the defaults
#'
#' Accepts a list or a character vector. Named input overrides only the
#' categories it names, and may introduce categories of its own for data whose
#' trend column was not written by [mann_kendall_test()]. Unnamed input is
#' taken to be a complete set, in the order of the defaults.
#'
#' @param colours what the user passed, or NULL
#' @param defaults the built-in colours, already renamed for `na_label`
#' @param arg argument name, for the error messages
#' @returns a named character vector of hex codes
#' @noRd
resolve_trend_colours <- function(colours, defaults, arg) {
  if (is.null(colours)) {
    return(defaults)
  }

  colours <- unlist(colours, use.names = TRUE)
  if (!is.character(colours) || length(colours) == 0) {
    stop(glue::glue(
      "`{arg}` must be a character vector or list of colours, or NULL."
    ))
  }

  if (is.null(names(colours)) || any(names(colours) == "")) {
    if (length(colours) != length(defaults)) {
      stop(glue::glue(
        "`{arg}` is unnamed, so it must hold {length(defaults)} colours in ",
        "the order {toString(names(defaults))}. Got {length(colours)}. ",
        "Name them instead to set only some categories."
      ))
    }
    names(colours) <- names(defaults)
    return(colours)
  }

  defaults[names(colours)] <- colours
  defaults
}

#' Style for one trend category's cells
#'
#' @param lvl the trend category
#' @param fills named fill colours
#' @param fonts named font colours
#' @param na_label the not-calculated level, which is set in italics
#' @returns an openxlsx style object
#' @noRd
trend_cell_style <- function(lvl, fills, fonts, na_label) {
  font <- if (lvl %in% names(fonts)) fonts[[lvl]] else "#3F3F3F"
  openxlsx::createStyle(
    fgFill = fills[[lvl]],
    fontColour = font,
    fontSize = 9,
    halign = "center",
    valign = "center",
    border = "TopBottomLeftRight",
    borderColour = "#D9D9D9",
    borderStyle = "thin",
    textDecoration = if (identical(lvl, na_label)) "italic" else NULL
  )
}

#' Add the sheet explaining the trend categories
#'
#' Every category that has a colour gets a row, swatched in that colour, so a
#' recoloured or extended palette stays self-documenting.
#'
#' @param wb the workbook to add to
#' @param fills named fill colours
#' @param fonts named font colours
#' @param na_label the not-calculated level
#' @param header_fill background colour for the header row
#' @param header_font text colour for the header row
#' @returns `wb`, invisibly, modified in place
#' @noRd
add_legend_sheet <- function(
  wb,
  fills,
  fonts,
  na_label,
  header_fill,
  header_font
) {
  lvls <- names(fills)
  meanings <- rename_na_level(TREND_MEANING_DEFAULT, na_label)
  interpretations <- rename_na_level(TREND_INTERPRETATION_DEFAULT, na_label)

  legend <- dplyr::tibble(
    Trend = lvls,
    Meaning = unname(meanings[lvls]),
    Interpretation = unname(interpretations[lvls])
  )
  legend$Meaning[is.na(legend$Meaning)] <- ""
  legend$Interpretation[is.na(legend$Interpretation)] <- ""

  openxlsx::addWorksheet(wb, "Legend", gridLines = FALSE)
  openxlsx::writeData(
    wb,
    "Legend",
    legend,
    headerStyle = openxlsx::createStyle(
      fgFill = header_fill,
      fontColour = header_font,
      textDecoration = "bold",
      fontSize = 10,
      border = "bottom",
      borderStyle = "medium"
    )
  )

  for (i in seq_len(nrow(legend))) {
    openxlsx::addStyle(
      wb,
      "Legend",
      trend_cell_style(legend$Trend[i], fills, fonts, na_label),
      rows = i + 1,
      cols = 1
    )
    openxlsx::addStyle(
      wb,
      "Legend",
      openxlsx::createStyle(fontSize = 10, valign = "center"),
      rows = i + 1,
      cols = 2:3,
      gridExpand = TRUE
    )
  }
  openxlsx::setColWidths(wb, "Legend", cols = 1:3, widths = c(22, 62, 16))

  invisible(wb)
}

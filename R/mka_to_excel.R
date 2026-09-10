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
#' With `include_zone = TRUE` the monitoring zone is written as a further
#' column to the left of the location names, the rows are sorted by zone and
#' then location so each zone's wells sit together, and the repeated zone
#' cells of a block are merged into one, and the location names are set a
#' shade back from the zone beside them, in `location_fill`, so the grouping
#' reads at a glance. [mann_kendall_test()] nests by
#' location and analyte, so the zone is not a column of its output; where
#' `data` has no zone column of its own it is recovered from that nested
#' `data` column, and a location falling in more than one zone is an error
#' rather than a silently duplicated row.
#'
#' @param data tibble from [mann_kendall_test()]. Only the location, analyte
#'   and trend columns are used; the nested `data` column and the test
#'   statistics are ignored, so there is no need to drop them first - though
#'   `include_zone = TRUE` reads the zone back out of that nested column.
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
#' @param include_zone write the monitoring zone as the first column, ahead of
#'   the location names, and sort the rows by zone. Default `FALSE`, which
#'   lays the sheet out as before.
#' @param merge_zones merge each zone's repeated cells into a single block, so
#'   the zone is named once against its wells. Default `TRUE`. Set `FALSE` to
#'   leave a value in every row, which is what Excel's sort, filter and pivot
#'   tools want. Ignored unless `include_zone = TRUE`.
#' @param zone_label heading for the zone column. Default
#'   `"Monitoring Zone"`.
#' @param zone_col Name of the column containing monitoring zones. Can be
#'   provided with or without quotes. Default is monitoring_zone. Where `data`
#'   has no such column of its own, the zone is read out of the nested `data`
#'   column that [mann_kendall_test()] returns. Locations with no zone
#'   recorded are left blank.
#' @param location_fill background colour for the location names, where a zone
#'   column sits to their left. Default `"#9BBEAF"`, a lighter sage against the
#'   zone's deep green. The heading above them is part of the header row and
#'   stays `header_fill`. Ignored unless `include_zone = TRUE`, since with no
#'   zone beside them the location names take `header_fill` too.
#' @param location_font text colour for those same names. Defaults to
#'   `header_font`, so white unless changed - note that white on the default
#'   `location_fill` is a contrast ratio of about 2:1, and a dark green such as
#'   `"#14401F"` reads better if the sheet is to be printed.
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
#' # Group the wells by monitoring zone, read back out of the nested data
#' zoned <- mka_to_excel(
#'   trends,
#'   save_path = tempfile(fileext = ".xlsx"),
#'   include_zone = TRUE
#' )
#' zoned[, 1:3]
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
#'
#' # Zones down the side, but a value in every row so the sheet still filters
#' mka_to_excel(trends, include_zone = TRUE, merge_zones = FALSE)
#'
#' # Darker text on the well names, for printing
#' mka_to_excel(trends, include_zone = TRUE, location_font = "#14401F")
#' }
#'
#' @importFrom dplyr select arrange mutate across all_of tibble
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom rlang enquo quo_name sym !!
#' @importFrom glue glue
#' @importFrom utils head
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle
#'   addStyle setColWidths setRowHeights freezePane mergeCells saveWorkbook
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
  trend_col = trend,
  include_zone = FALSE,
  merge_zones = TRUE,
  zone_label = "Monitoring Zone",
  zone_col = monitoring_zone,
  location_fill = "#9BBEAF",
  location_font = header_font
) {
  loc_col <- rlang::enquo(location_col)
  chem_col <- rlang::enquo(chem_name_col)
  trd_col <- rlang::enquo(trend_col)
  zn_col <- rlang::enquo(zone_col)

  loc_name <- rlang::quo_name(loc_col)
  chem_name_str <- rlang::quo_name(chem_col)
  trend_name <- rlang::quo_name(trd_col)
  zone_name <- rlang::quo_name(zn_col)

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
  if (
    !is.logical(include_zone) ||
      length(include_zone) != 1 ||
      is.na(include_zone)
  ) {
    stop("`include_zone` must be TRUE or FALSE.")
  }
  if (include_zone && zone_name %in% c(loc_name, chem_name_str, trend_name)) {
    stop(glue::glue(
      "`zone_col` names {zone_name}, which is already in use as the ",
      "location, analyte or trend column."
    ))
  }

  long <- dplyr::select(data, !!loc_col, !!chem_col, !!trd_col)

  if (include_zone) {
    long[[zone_name]] <- zone_values(data, zone_name)
    check_one_zone_per_location(long[[loc_name]], long[[zone_name]], zone_name)
    long[[zone_name]] <- tidyr::replace_na(long[[zone_name]], "")
  }

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
    tidyr::pivot_wider(names_from = !!chem_col, values_from = !!trd_col)

  # Zones read down the sheet in blocks, wells alphabetically within each.
  wide <- if (include_zone) {
    dplyr::arrange(wide, !!rlang::sym(zone_name), !!loc_col)
  } else {
    dplyr::arrange(wide, !!loc_col)
  }

  # Everything that is not an identifier is an analyte, and analytes read left
  # to right in alphabetical order, whatever order the test returned them in.
  id_names <- if (include_zone) c(zone_name, loc_name) else loc_name
  n_id <- length(id_names)
  chem_cols <- sort(setdiff(names(wide), id_names))
  wide <- wide[, c(id_names, chem_cols)] %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(chem_cols),
      ~ tidyr::replace_na(as.character(.x), na_label)
    ))
  names(wide)[seq_len(n_id)] <- if (include_zone) {
    c(zone_label, location_label)
  } else {
    location_label
  }

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

  mat <- as.matrix(wide[, -seq_len(n_id), drop = FALSE])
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
    headerStyle = header_row_style(header_fill, header_font)
  )

  n_row <- nrow(wide)
  n_col <- ncol(wide)

  if (n_row > 0) {
    id_rows <- 2:(n_row + 1)

    openxlsx::addStyle(
      wb,
      sheet_name,
      id_body_style(header_fill, header_font),
      rows = id_rows,
      cols = 1,
      gridExpand = TRUE
    )

    # The zone column keeps the deep green; the well names it groups are set
    # a shade back from it, so the two read as an order. The header row above
    # them stays one colour across.
    if (include_zone) {
      openxlsx::addStyle(
        wb,
        sheet_name,
        id_body_style(location_fill, location_font),
        rows = id_rows,
        cols = 2,
        gridExpand = TRUE
      )

      if (merge_zones) {
        merge_column_runs(wb, sheet_name, wide[[1]])
      }
    }
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
      cols = hits[, "col"] + n_id,
      gridExpand = FALSE,
      stack = FALSE
    )
  }

  openxlsx::setColWidths(
    wb,
    sheet_name,
    cols = seq_len(n_id),
    widths = if (include_zone) c(20, 16) else 16
  )
  if (n_col > n_id) {
    openxlsx::setColWidths(
      wb,
      sheet_name,
      cols = (n_id + 1):n_col,
      widths = 21
    )
  }
  openxlsx::setRowHeights(wb, sheet_name, rows = 1, heights = 150)
  openxlsx::freezePane(
    wb,
    sheet_name,
    firstActiveRow = 2,
    firstActiveCol = n_id + 1
  )

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

#' The monitoring zone for each row of a Mann-Kendall result
#'
#' Prefers a zone column on `data` itself. Failing that, reads it out of the
#' nested `data` column that [mann_kendall_test()] returns, which is where the
#' zone ends up once the test has nested by location and analyte - so the
#' usual pipeline needs no join first.
#'
#' @param data the tibble passed to [mka_to_excel()]
#' @param zone_name name of the zone column, as a string
#' @returns a character vector, one zone per row of `data`
#' @noRd
zone_values <- function(data, zone_name) {
  if (zone_name %in% names(data)) {
    return(as.character(data[[zone_name]]))
  }

  nested <- data[["data"]]
  nested_has_zone <- is.list(nested) &&
    !is.data.frame(nested) &&
    length(nested) == nrow(data) &&
    all(vapply(
      nested,
      function(d) is.data.frame(d) && zone_name %in% names(d),
      logical(1)
    ))

  if (nested_has_zone) {
    return(vapply(
      nested,
      function(d) single_zone(d[[zone_name]], zone_name),
      character(1)
    ))
  }

  stop(glue::glue(
    "`data` has no column {zone_name}, and it is not in the nested `data` ",
    "column either. Join the zone on before calling, or name the column ",
    "with `zone_col`."
  ))
}

#' The one zone in a nested location/analyte frame
#'
#' @param x the zone values of one nested frame
#' @param zone_name name of the zone column, for the warning
#' @returns a single string, `NA` where no zone was recorded
#' @noRd
single_zone <- function(x, zone_name) {
  x <- unique(as.character(x[!is.na(x)]))
  if (length(x) == 0) {
    return(NA_character_)
  }
  if (length(x) > 1) {
    warning(glue::glue(
      "A location/analyte group holds more than one {zone_name} ",
      "({toString(x)}); using {x[1]}."
    ))
  }
  x[1]
}

#' Stop if a location spans more than one zone
#'
#' A well in two zones would pivot to two rows, splitting its trends across
#' them, so it is worth catching before the table is built.
#'
#' @param locations location codes
#' @param zones the matching zones
#' @param zone_name name of the zone column, for the message
#' @returns `NULL`, invisibly
#' @noRd
check_one_zone_per_location <- function(locations, zones, zone_name) {
  by_loc <- split(zones, locations)
  mixed <- names(by_loc)[vapply(
    by_loc,
    function(z) length(unique(z[!is.na(z)])) > 1,
    logical(1)
  )]

  if (length(mixed) > 0) {
    more <- if (length(mixed) > 5) {
      glue::glue(" and {length(mixed) - 5} more")
    } else {
      ""
    }
    stop(glue::glue(
      "{toString(utils::head(mixed, 5))}{more} fall in more than one ",
      "{zone_name}. Each location needs a single zone."
    ))
  }

  invisible(NULL)
}

#' Merge each run of repeated values in a written column
#'
#' @param wb the workbook to modify
#' @param sheet the worksheet name
#' @param x the column as written, excluding its header
#' @returns `wb`, invisibly, modified in place
#' @noRd
merge_column_runs <- function(wb, sheet, x) {
  runs <- rle(x)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L

  for (i in seq_along(runs$lengths)) {
    if (runs$lengths[i] > 1) {
      # +1 throughout to step over the header row.
      openxlsx::mergeCells(
        wb,
        sheet,
        cols = 1,
        rows = (starts[i] + 1):(ends[i] + 1)
      )
    }
  }

  invisible(wb)
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

#' Style for the header row across the top
#'
#' Rotated upright, so a wide analyte suite stays narrow on the page.
#'
#' @param fill background colour
#' @param font text colour
#' @returns an openxlsx style object
#' @noRd
header_row_style <- function(fill, font) {
  openxlsx::createStyle(
    fgFill = fill,
    fontColour = font,
    textRotation = 90,
    halign = "center",
    valign = "bottom",
    textDecoration = "bold",
    fontSize = 9,
    border = "TopBottomLeftRight",
    borderColour = "#BFBFBF",
    borderStyle = "thin"
  )
}

#' Style for an identifier column below the header
#'
#' @param fill background colour
#' @param font text colour
#' @returns an openxlsx style object
#' @noRd
id_body_style <- function(fill, font) {
  openxlsx::createStyle(
    fgFill = fill,
    fontColour = font,
    fontSize = 9,
    textDecoration = "bold",
    halign = "left",
    valign = "center",
    border = "TopBottomLeftRight",
    borderColour = "#D9D9D9",
    borderStyle = "thin"
  )
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

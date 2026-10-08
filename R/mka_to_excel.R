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
#' [mann_kendall_test()] tests each unit an analyte was reported in as a
#' separate series. Where an analyte comes in more than one unit, each unit
#' gets a column of its own, headed with the unit - `"Zinc (mg/L)"` and
#' `"Zinc (ug/L)"` - on the summary and statistics sheets alike. An analyte
#' reported in one unit keeps its plain name.
#'
#' A trend whose non-detects were reported at more than one LOR is marked
#' with an asterisk - `"Decreasing *"` - since the change in LOR, rather than
#' the site, may be what the test picked up; see `mark_lor_changes` and
#' [mann_kendall_test()]'s Changes in LOR.
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
#' cells of a block are merged into one (`merge_cells`), and the location
#' names are set a shade back from the zone beside them, in `location_fill`,
#' so the grouping reads at a glance. [mann_kendall_test()] nests by
#' location and analyte, so the zone is not a column of its output; where
#' `data` has no zone column of its own it is recovered from that nested
#' `data` column, and a location falling in more than one zone is an error
#' rather than a silently duplicated row.
#'
#' With `include_stats = TRUE` a further sheet sets out the test behind each
#' trend, one row per location and analyte: the trend, coloured and styled as
#' it is on the summary sheet, then the number of samples tested, the S
#' statistic, Kendall's tau, the p-value, and the mean, standard deviation
#' and coefficient of variation of the concentrations tested - after any
#' `lor_multiplier` substitution [mann_kendall_test()] made - and whether the
#' series' non-detects were reported at more than one LOR, with the lowest and
#' highest of those LORs. The values are
#' written unrounded; number formats set only what is displayed. Where every
#' value tested is the same - most often a series of non-detects all at one
#' LOR - S and its variance are both zero, so the test cannot give Kendall's
#' tau or a p-value. Those cells, and any other statistic the test could not
#' compute, are written as `-` rather than as Excel's `#NUM!` error.
#'
#' `include_summary = TRUE` adds the [summary_stats()] columns to that sheet -
#' detect counts and percentages, the minimum, mean, maximum and standard
#' deviation, and the 5th to 99th percentiles - calculated from the nested
#' `data` column, so they describe exactly the samples each trend was tested
#' on. The mean, standard deviation and percentiles take non-detects at the
#' same multiple of their LOR the test did - the `lor_multiplier`
#' [mann_kendall_test()] was run with, which it records on its result - so
#' the summary's mean and standard deviation agree with the test's own "as
#' tested" pair, and their headings name the multiplier where it is not 1:
#' "Mean (ND at 0.5x LOR)". Where the record has been lost, by
#' `dplyr::bind_rows()` say, they are of the full LOR, headed "as reported".
#' The counts, minimum and maximum are always as reported. The maximum is the
#' highest detected
#' result, as [summary_stats()] reports it. A minimum or maximum that is a
#' non-detect is written with its `<`, as text: the minimum where any
#' non-detect sits at the lowest value, the maximum only where nothing was
#' detected. The mean and percentiles are calculated rather than reported,
#' and stay numbers.
#' Where guideline sets were joined on with [join_action_levels()] before the
#' trend test, each set named in `criteria_col` follows with a guideline column
#' and an exceedance count of its own, headed with the set's name.
#'
#' @param data tibble from [mann_kendall_test()]. Only the location, analyte
#'   and trend columns go into the summary sheet. The test statistics are
#'   written only with `include_stats = TRUE`, and the nested `data` column is
#'   read only for the zone, the sample count and `include_summary`, so there
#'   is no need to drop either first.
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
#' @param sheet_name name of the summary worksheet. It cannot be `"Legend"`
#'   or `"Statistics"` where those sheets are written too.
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
#' @param merge_cells merge a value repeated down a side column into one
#'   block, as [results_table_to_excel()] and [summary_stats_to_excel()] do:
#'   the zone, so it is named once against its wells, and on the Statistics
#'   sheet each well, named once against its analytes. Each column's blocks
#'   sit within those of the column to its left - a well never merges across
#'   two zones. Default `TRUE`. Set `FALSE` to leave a value in every row,
#'   which is what Excel's sort, filter and pivot tools want.
#' @param merge_zones Deprecated; use `merge_cells`. Still accepted, with a
#'   warning.
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
#' @param include_stats add a `"Statistics"` sheet, between the summary and
#'   the legend, with the trend and test statistics for each location/analyte
#'   pair, sorted as the summary sheet is and filterable from its header row.
#'   Statistics `data` does not carry are left out, and the sample count
#'   needs the nested `data` column. Default `FALSE`.
#' @param include_summary add the [summary_stats()] columns to the statistics
#'   sheet, after the test statistics. Needs `include_stats = TRUE`, and the
#'   nested `data` column with the `concentration` and `detect_flag` columns
#'   [data_processor()] writes. Default `FALSE`.
#' @param criteria_col the guideline column or columns to write, where
#'   guideline sets were joined on by [join_action_levels()] before
#'   [mann_kendall_test()] was run - one per set, each under the `value_col`
#'   it was joined with. One column, or several as
#'   `c(criteria_95, criteria_99)`, with or without quotes; `all_of()` and
#'   helpers such as `starts_with("criteria")` work too. With
#'   `include_summary = TRUE` each set adds two columns, in the order named:
#'   the guideline, and the number of results that exceeded it, counted from
#'   that set's own `exceedance` verdicts. Each pair is headed with the set's
#'   name - `"NEMP 99% Guideline"` - where [join_action_levels()] recorded
#'   one. A location/analyte pair with no guideline in a set reads `-` in
#'   both, rather than a count of 0. A column named here that the nested data
#'   lacks is an error. Default `criteria`, the column [join_action_levels()]
#'   writes unless given another `value_col`, used where it is there; any
#'   other set joined alongside is named in a message rather than dropped
#'   unremarked. `NULL` leaves the guidelines out. Naming any without
#'   `include_summary = TRUE` warns, as they would otherwise be ignored.
#' @param mark_lor_changes mark a trend whose non-detects were reported at more
#'   than one LOR - `lor_changed` in [mann_kendall_test()]'s output - with an
#'   asterisk on the summary sheet, e.g. `"Decreasing *"`. The cell keeps its
#'   trend's colour, and a note explaining the asterisk goes on the legend
#'   sheet, or under the table where there is no legend. Default `TRUE`. Does
#'   nothing where `data` has no `lor_changed` column.
#'
#' @returns The wide tibble that was written, invisibly - locations down,
#'   analytes across, trends in the cells, with any asterisk
#'   `mark_lor_changes` added.
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
#' # Add a sheet of the statistics behind each trend
#' mka_to_excel(
#'   trends,
#'   save_path = tempfile(fileext = ".xlsx"),
#'   include_stats = TRUE
#' )
#'
#' # ...with the summary statistics of the samples tested alongside
#' mka_to_excel(
#'   trends,
#'   save_path = tempfile(fileext = ".xlsx"),
#'   include_stats = TRUE,
#'   include_summary = TRUE
#' )
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
#' mka_to_excel(trends, include_zone = TRUE, merge_cells = FALSE)
#'
#' # Darker text on the well names, for printing
#' mka_to_excel(trends, include_zone = TRUE, location_font = "#14401F")
#'
#' # Guideline and exceedance count too, joined on before the trend test -
#' # picked up from `criteria` without being asked for
#' compared <- join_action_levels(gRs_data, anzg)
#' mka_to_excel(
#'   mann_kendall_test(compared),
#'   include_stats = TRUE,
#'   include_summary = TRUE
#' )
#'
#' # Several sets, each joined under its own name and counted separately
#' sets <- gRs_data %>%
#'   join_action_levels(nemp_95, value_col = "criteria_95") %>%
#'   join_action_levels(nemp_99, value_col = "criteria_99")
#' mka_to_excel(
#'   mann_kendall_test(sets),
#'   include_stats = TRUE,
#'   include_summary = TRUE,
#'   criteria_col = c(criteria_95, criteria_99)
#' )
#' }
#'
#' @importFrom dplyr select arrange mutate across all_of tibble
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom rlang enquo quo_name quo_is_null sym syms !! !!!
#' @importFrom dplyr bind_rows bind_cols
#' @importFrom glue glue
#' @importFrom utils head
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle
#'   addStyle setColWidths setRowHeights freezePane mergeCells saveWorkbook
#'   addFilter
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
  merge_cells = TRUE,
  zone_label = "Monitoring Zone",
  zone_col = monitoring_zone,
  location_fill = "#9BBEAF",
  location_font = header_font,
  include_stats = FALSE,
  include_summary = FALSE,
  criteria_col = criteria,
  mark_lor_changes = TRUE,
  merge_zones = NULL
) {
  # merge_zones merged only the zone; merge_cells, named as in
  # results_table_to_excel() and summary_stats_to_excel(), merges the wells of
  # the Statistics sheet too.
  if (!is.null(merge_zones)) {
    if (!missing(merge_cells)) {
      stop(
        "Use `merge_cells` alone; `merge_zones` is its deprecated name.",
        call. = FALSE
      )
    }
    warning(
      "`merge_zones` is deprecated; use `merge_cells` instead.",
      call. = FALSE
    )
    merge_cells <- merge_zones
  }
  check_flag(merge_cells, "merge_cells")

  # Named outright, the guideline columns have to be there. Left at its
  # default, `criteria` is picked up only where join_action_levels() put one.
  criteria_named <- !missing(criteria_col)

  loc_col <- rlang::enquo(location_col)
  chem_col <- rlang::enquo(chem_name_col)
  trd_col <- rlang::enquo(trend_col)
  zn_col <- rlang::enquo(zone_col)
  crit_col <- rlang::enquo(criteria_col)

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
  if (
    !is.logical(include_stats) ||
      length(include_stats) != 1 ||
      is.na(include_stats)
  ) {
    stop("`include_stats` must be TRUE or FALSE.")
  }
  if (
    !is.logical(include_summary) ||
      length(include_summary) != 1 ||
      is.na(include_summary)
  ) {
    stop("`include_summary` must be TRUE or FALSE.")
  }
  if (
    !is.logical(mark_lor_changes) ||
      length(mark_lor_changes) != 1 ||
      is.na(mark_lor_changes)
  ) {
    stop("`mark_lor_changes` must be TRUE or FALSE.")
  }
  if (include_summary && !include_stats) {
    stop(
      "`include_summary = TRUE` adds to the Statistics sheet, so needs ",
      "`include_stats = TRUE` as well."
    )
  }
  if (criteria_named && !rlang::quo_is_null(crit_col) && !include_summary) {
    warning(
      "`criteria_col` is only used with `include_summary = TRUE`, so no ",
      "guideline or exceedance count was written."
    )
  }
  if (include_stats && identical(tolower(sheet_name), tolower(STATS_SHEET))) {
    stop(glue::glue(
      "`sheet_name` cannot be \"{sheet_name}\" with `include_stats = TRUE`; ",
      "the statistics sheet takes that name."
    ))
  }
  if (legend && identical(tolower(sheet_name), tolower(LEGEND_SHEET))) {
    stop(glue::glue(
      "`sheet_name` cannot be \"{sheet_name}\" with `legend = TRUE`; ",
      "the legend sheet takes that name."
    ))
  }
  if (include_zone && zone_name %in% c(loc_name, chem_name_str, trend_name)) {
    stop(glue::glue(
      "`zone_col` names {zone_name}, which is already in use as the ",
      "location, analyte or trend column."
    ))
  }

  long <- dplyr::select(data, !!loc_col, !!chem_col, !!trd_col)

  # mann_kendall_test() tests each unit separately, so an analyte reported in
  # two units has a trend for each, and each needs a column of its own.
  long[[chem_name_str]] <- unit_labelled(
    long[[chem_name_str]],
    data[["output_unit"]]
  )

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

  # Built before anything is written, so a call with no statistics to show
  # fails without leaving half a workbook behind.
  stats <- if (include_stats) {
    stats_table(
      data,
      long,
      key_names = c(if (include_zone) zone_name, loc_name, chem_name_str),
      trend_name = trend_name,
      na_label = na_label,
      include_summary = include_summary,
      criteria_col = crit_col,
      criteria_named = criteria_named
    )
  }

  # Marked after the statistics are taken, so the Statistics sheet shows the
  # trend plainly beside its own LOR columns.
  marked <- FALSE
  if (mark_lor_changes && "lor_changed" %in% names(data)) {
    trends <- as.character(long[[trend_name]])
    flag <- data$lor_changed %in% TRUE & !is.na(trends)
    long[[trend_name]] <- ifelse(flag, paste0(trends, LOR_MARK), trends)
    marked <- any(flag)
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

  # Coloured by the trend under any marker, so "Decreasing *" is filled as
  # "Decreasing" is.
  mat <- as.matrix(wide[, -seq_len(n_id), drop = FALSE])
  mat <- unmark_lor(mat)
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

      # A well has a row of its own here, so the zone is all there is to merge.
      if (merge_cells) {
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

  if (include_stats) {
    add_stats_sheet(
      wb,
      stats$table,
      id_labels = names(wide)[seq_len(n_id)],
      labels = stats$labels,
      formats = stats$formats,
      fills = fills,
      fonts = fonts,
      na_label = na_label,
      header_fill = header_fill,
      header_font = header_font,
      include_zone = include_zone,
      # The zone, the well and the analyte - unique within a well, so it never
      # merges, but the wells merge over their analytes.
      merge_cols = if (merge_cells) n_id + 1L else 0L,
      location_fill = location_fill,
      location_font = location_font
    )
  }

  if (legend) {
    add_legend_sheet(
      wb,
      fills,
      fonts,
      na_label,
      header_fill,
      header_font,
      lor_note = marked
    )
  } else if (marked) {
    # With no legend to explain it, the marker is explained under the table.
    write_note(wb, sheet_name, LOR_NOTE, row = n_row + 3, cols = 1:n_col)
  }

  save_workbook(wb, save_path, overwrite)

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

# The statistics and legend sheets' names, which the summary sheet may not
# also take.
STATS_SHEET <- "Statistics"
LEGEND_SHEET <- "Legend"

# What the statistics sheet writes for a statistic the test could not compute.
STATS_NA <- "-"

# The columns the statistics sheet writes after the trend, in order, with the
# heading each is written under. All come from mann_kendall_test(); a result
# from before it returned n_samples has it counted from the nested data.
STATS_LABELS <- c(
  "n_samples" = "Samples",
  "S_statistic" = "S Statistic",
  "tau_statistic" = "Kendall's Tau",
  "p_value" = "p-value",
  "sample_mean" = "Mean",
  "SD" = "Standard Deviation",
  "COV" = "Coefficient of Variation",
  "lor_changed" = "LOR Changed",
  "lor_min" = "Lowest ND LOR",
  "lor_max" = "Highest ND LOR"
)

# Excel number formats for those columns. The mean and standard deviation are
# in whatever units the concentrations were, and left in General so a trace
# metal at 0.0001 mg/L does not display as zero; so are the LORs.
STATS_FORMATS <- c(
  "n_samples" = "0",
  "S_statistic" = "0",
  "tau_statistic" = "0.000",
  "p_value" = "0.0000",
  "sample_mean" = "GENERAL",
  "SD" = "GENERAL",
  "COV" = "0.00",
  "lor_changed" = "GENERAL",
  "lor_min" = "GENERAL",
  "lor_max" = "GENERAL"
)

# Added to a trend whose non-detects were reported at more than one LOR, and
# the note that explains it.
LOR_MARK <- " *"
LOR_NOTE <- paste(
  "* Non-detects in this series were reported at more than one LOR, so the",
  "trend may reflect the change in LOR rather than the site. See the",
  "Statistics sheet, or the lor_min and lor_max columns of",
  "mann_kendall_test()."
)

#' Save a workbook, creating its directory if need be
#'
#' @param wb the workbook to save
#' @param path full file path including filename
#' @param overwrite overwrite `path` if it already exists
#' @returns `path`, invisibly
#' @noRd
save_workbook <- function(wb, path, overwrite) {
  out_dir <- dirname(path)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
    message(glue::glue("Created directory: {out_dir}"))
  }
  openxlsx::saveWorkbook(wb, path, overwrite = overwrite)
  message(glue::glue("Saved: {basename(path)} -> {path}"))
  invisible(path)
}

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

  nested <- nested_frames(data)
  nested_has_zone <- !is.null(nested) &&
    all(vapply(nested, function(d) zone_name %in% names(d), logical(1)))

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

#' The nested series behind each row of a Mann-Kendall result
#'
#' @param data the tibble passed to [mka_to_excel()]
#' @returns the nested `data` column, a list of data frames with one per row
#'   of `data`, or `NULL` where `data` has no such column
#' @noRd
nested_frames <- function(data) {
  nested <- data[["data"]]
  is_nested <- is.list(nested) &&
    !is.data.frame(nested) &&
    length(nested) == nrow(data) &&
    all(vapply(nested, is.data.frame, logical(1)))

  if (is_nested) nested else NULL
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
#' @param first_row the sheet row `x` starts on. Default 2, below a single
#'   header row; [results_table_to_excel()] writes several.
#' @param col the sheet column `x` is written in. Default 1.
#' @returns `wb`, invisibly, modified in place
#' @noRd
merge_column_runs <- function(wb, sheet, x, first_row = 2L, col = 1L) {
  runs <- rle(x)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L
  # Steps over the header rows above the column.
  offset <- first_row - 1L
  long <- runs$lengths > 1
  if (!any(long)) {
    return(invisible(wb))
  }

  # Added in one go, as openxlsx::mergeCells() would add them one at a time.
  # It checks each new merge against every merge already on the sheet, which
  # turns quadratic over the thousands of nested blocks a grouped summary
  # writes; the runs of one column cannot overlap, so there is nothing for it
  # to find. This writes the same <mergeCell> entries mergeCells() does, into
  # the Workbook fields it writes them to - wb$validateSheet() and
  # wb$worksheets[[i]]$mergeCells, openxlsx internals unchanged through 4.x,
  # hence DESCRIPTION's openxlsx (>= 4.2.5). The tests check the exact refs
  # written, so a release that moves them fails there, not in a user's sheet.
  letter <- openxlsx::int2col(col)
  refs <- paste0(
    letter, starts[long] + offset, ":", letter, ends[long] + offset
  )
  i <- wb$validateSheet(sheet)
  wb$worksheets[[i]]$mergeCells <- c(
    wb$worksheets[[i]]$mergeCells,
    sprintf("<mergeCell ref=\"%s\"/>", refs)
  )

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
#' Rotated upright by default, so a wide analyte suite stays narrow on the
#' page.
#'
#' @param fill background colour
#' @param font text colour
#' @param rotate set the text upright; otherwise it reads across and wraps
#' @returns an openxlsx style object
#' @noRd
header_row_style <- function(fill, font, rotate = TRUE) {
  openxlsx::createStyle(
    fgFill = fill,
    fontColour = font,
    textRotation = if (rotate) 90 else NULL,
    wrapText = !rotate,
    halign = "center",
    valign = if (rotate) "bottom" else "center",
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

#' Style for a plain body cell on the statistics sheet
#'
#' @param num_fmt Excel number format
#' @param halign horizontal alignment, or `NULL` for Excel's own - text to
#'   the left, numbers to the right
#' @returns an openxlsx style object
#' @noRd
stats_body_style <- function(num_fmt = "GENERAL", halign = NULL) {
  openxlsx::createStyle(
    fontSize = 9,
    halign = halign,
    valign = "center",
    numFmt = num_fmt,
    border = "TopBottomLeftRight",
    borderColour = "#D9D9D9",
    borderStyle = "thin"
  )
}

#' Style for the header row of a legend sheet
#'
#' @param fill background colour
#' @param font text colour
#' @returns an openxlsx style object
#' @noRd
legend_header_style <- function(fill, font) {
  openxlsx::createStyle(
    fgFill = fill,
    fontColour = font,
    textDecoration = "bold",
    fontSize = 10,
    border = "bottom",
    borderStyle = "medium"
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
  # Looked up with match() rather than [[, which cannot find a level named ""
  # - and na_label = "" names the not-calculated level exactly that.
  font <- unname(fonts[match(lvl, names(fonts))])
  if (is.na(font)) {
    font <- "#3F3F3F"
  }
  openxlsx::createStyle(
    fgFill = unname(fills[match(lvl, names(fills))]),
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
#' @param lor_note write `LOR_NOTE` under the table, for a summary sheet with
#'   trends marked by `mark_lor_changes`
#' @returns `wb`, invisibly, modified in place
#' @noRd
add_legend_sheet <- function(
  wb,
  fills,
  fonts,
  na_label,
  header_fill,
  header_font,
  lor_note = FALSE
) {
  lvls <- names(fills)
  meanings <- rename_na_level(TREND_MEANING_DEFAULT, na_label)
  interpretations <- rename_na_level(TREND_INTERPRETATION_DEFAULT, na_label)

  # match() rather than indexing by name, which never finds a level named "".
  legend <- dplyr::tibble(
    Trend = lvls,
    Meaning = unname(meanings[match(lvls, names(meanings))]),
    Interpretation = unname(interpretations[match(
      lvls,
      names(interpretations)
    )])
  )
  legend$Meaning[is.na(legend$Meaning)] <- ""
  legend$Interpretation[is.na(legend$Interpretation)] <- ""

  openxlsx::addWorksheet(wb, LEGEND_SHEET, gridLines = FALSE)
  openxlsx::writeData(
    wb,
    LEGEND_SHEET,
    legend,
    headerStyle = legend_header_style(header_fill, header_font)
  )

  for (i in seq_len(nrow(legend))) {
    openxlsx::addStyle(
      wb,
      LEGEND_SHEET,
      trend_cell_style(legend$Trend[i], fills, fonts, na_label),
      rows = i + 1,
      cols = 1
    )
    openxlsx::addStyle(
      wb,
      LEGEND_SHEET,
      openxlsx::createStyle(fontSize = 10, valign = "center"),
      rows = i + 1,
      cols = 2:3,
      gridExpand = TRUE
    )
  }
  openxlsx::setColWidths(wb, LEGEND_SHEET, cols = 1:3, widths = c(22, 62, 16))

  if (lor_note) {
    write_note(wb, LEGEND_SHEET, LOR_NOTE, row = nrow(legend) + 3, cols = 1:3)
  }

  invisible(wb)
}

#' Take the LOR marker off the trends it was added to
#'
#' @param x trend text, possibly ending in `LOR_MARK`
#' @returns `x`, the same shape, with the marker removed
#' @noRd
unmark_lor <- function(x) {
  marked <- !is.na(x) & endsWith(x, LOR_MARK)
  x[marked] <- substr(x[marked], 1, nchar(x[marked]) - nchar(LOR_MARK))
  x
}

#' Write a note across several columns below a table
#'
#' @param wb the workbook to write to
#' @param sheet the sheet to write on
#' @param text the note
#' @param row the row to write it on
#' @param cols the columns to merge it across
#' @returns `wb`, invisibly, modified in place
#' @noRd
write_note <- function(wb, sheet, text, row, cols) {
  openxlsx::writeData(wb, sheet, text, startRow = row, startCol = cols[[1]])
  if (length(cols) > 1) {
    openxlsx::mergeCells(wb, sheet, cols = cols, rows = row)
  }
  openxlsx::addStyle(
    wb,
    sheet,
    openxlsx::createStyle(
      fontSize = 10,
      textDecoration = "italic",
      wrapText = TRUE,
      valign = "top"
    ),
    rows = row,
    cols = cols,
    gridExpand = TRUE
  )
  openxlsx::setRowHeights(wb, sheet, rows = row, heights = 45)
  invisible(wb)
}

#' The long table of statistics behind each trend
#'
#' @param data the tibble passed to [mka_to_excel()]
#' @param long the identifier and trend columns already taken from `data`,
#'   in the same row order, with any zone filled in
#' @param key_names the identifier columns to sort by: the zone where there is
#'   one, then location, then analyte
#' @param trend_name name of the trend column
#' @param na_label what to write where the test returned no trend
#' @param include_summary add the `summary_columns()` after the statistics
#' @param criteria_col,criteria_named passed on to `summary_columns()`
#' @returns a list of `table`, a tibble of the keys, the trend, then the
#'   statistics in `STATS_LABELS` order under their original column names,
#'   then any summary columns; and `labels` and `formats`, the heading and
#'   Excel number format of each statistic, named by column
#' @noRd
stats_table <- function(
  data,
  long,
  key_names,
  trend_name,
  na_label,
  include_summary = FALSE,
  criteria_col = rlang::quo(NULL),
  criteria_named = TRUE
) {
  from_data <- intersect(names(STATS_LABELS), names(data))
  if (length(from_data) == 0) {
    stop(glue::glue(
      "`include_stats = TRUE`, but `data` has none of the statistics ",
      "mann_kendall_test() returns ",
      "({toString(setdiff(names(STATS_LABELS), 'n_samples'))})."
    ))
  }

  stats <- long[, c(key_names, trend_name)]
  stats[[trend_name]] <- tidyr::replace_na(
    as.character(stats[[trend_name]]),
    na_label
  )

  nested <- nested_frames(data)
  if (!is.null(nested)) {
    stats$n_samples <- vapply(nested, nrow, integer(1))
  }
  # mk.test() hands back named estimates, which would otherwise ride along.
  # Where every value tested is the same - all non-detects at one LOR, say -
  # S and its variance are both zero, and tau and the p-value come back NaN.
  # openxlsx writes NaN and Inf as #NUM!, so they go in as NA, which
  # add_stats_sheet() writes as STATS_NA.
  for (col in from_data) {
    stats[[col]] <- finite_or_na(unname(data[[col]]))
  }
  if ("lor_changed" %in% names(stats)) {
    stats$lor_changed <- ifelse(stats$lor_changed, "Yes", "No")
  }

  stats <- stats[, c(
    key_names,
    trend_name,
    intersect(names(STATS_LABELS), names(stats))
  )]

  labels <- STATS_LABELS
  formats <- STATS_FORMATS

  if (include_summary) {
    analytes <- long[[key_names[length(key_names)]]]
    # The summary substitutes non-detects as the test did, so its mean and
    # standard deviation are the test's own figures, not a second opinion.
    lor_multiplier <- attr(data, "lor_multiplier")
    if (is.null(lor_multiplier)) {
      lor_multiplier <- 1
    }
    summ <- summary_columns(
      data,
      analytes,
      criteria_col,
      criteria_named,
      lor_multiplier = lor_multiplier
    )
    stats <- dplyr::bind_cols(stats, summ$table)

    labels[c("sample_mean", "SD")] <- c(
      "Mean (as tested)",
      "Standard Deviation (as tested)"
    )
    spec <- summary_column_spec(lor_multiplier)
    labels <- c(labels, spec$labels, summ$labels)
    formats <- c(formats, spec$formats, summ$formats)
  }

  list(
    table = dplyr::arrange(stats, !!!rlang::syms(key_names)),
    labels = labels,
    formats = formats
  )
}

#' NaN and Inf as NA
#'
#' @param x a column bound for the statistics sheet
#' @returns `x`, with any non-finite number made `NA`, which
#'   `add_stats_sheet()` writes as `STATS_NA` rather than as #NUM!
#' @noRd
finite_or_na <- function(x) {
  if (is.numeric(x)) {
    x[!is.finite(x)] <- NA
  }
  x
}

#' The summary_stats() columns for each row of a Mann-Kendall result
#'
#' Runs [summary_stats()] over the nested series, so the figures describe the
#' samples each trend was tested on rather than everything the original data
#' held. summary_stats() groups by location_code and chem_name, so each series
#' goes in under its row number as its location_code: that holds whatever the
#' caller's own columns are called, and lines the result back up with `data`.
#' It also groups by output_unit and criteria_set where present. A series
#' from mann_kendall_test() holds one unit, so that gives one row per series;
#' a series that still holds two - nested some other way - is an error,
#' since one row of statistics cannot describe numbers in two units.
#'
#' Each guideline set named in `criteria_col` adds a guideline column and an
#' exceedance count of its own. summary_stats() counts each set against the
#' verdicts join_action_levels() wrote for that set, and no other.
#'
#' @param data the tibble passed to [mka_to_excel()]
#' @param analytes the analyte of each row, for summary_stats()'s warnings
#' @param criteria_col what was passed as `criteria_col`, as a quosure
#' @param criteria_named whether `criteria_col` was passed at all
#' @param lor_multiplier the multiplier [mann_kendall_test()] substituted
#'   non-detects with, for summary_stats() to substitute them with too
#' @returns a list of `table`, a tibble with one row per row of `data` - the
#'   summary_stats() columns bar `n_samples`, among them `min_nd` and
#'   `max_nd`, then a `guideline__<set>` and `exceedances__<set>` pair for
#'   each guideline set - and the `labels` and `formats` of the guideline
#'   columns
#' @noRd
summary_columns <- function(
  data,
  analytes,
  criteria_col = rlang::quo(NULL),
  criteria_named = TRUE,
  lor_multiplier = 1
) {
  nested <- nested_frames(data)
  if (is.null(nested)) {
    stop(
      "`include_summary = TRUE` needs the nested `data` column ",
      "mann_kendall_test() returns, to summarise the samples behind each ",
      "trend."
    )
  }

  shared <- Reduce(intersect, lapply(nested, names))
  absent <- setdiff(c("concentration", "detect_flag"), shared)
  if (length(absent) > 0) {
    stop(glue::glue(
      "`include_summary = TRUE` needs {toString(absent)} in the nested ",
      "`data` column. Run mann_kendall_test() on a table from ",
      "data_processor()."
    ))
  }

  sets <- criteria_columns(
    criteria_col,
    criteria_named,
    template = nested[[1]][0, shared, drop = FALSE]
  )

  samples <- dplyr::bind_rows(lapply(seq_along(nested), function(i) {
    d <- nested[[i]]
    d$location_code <- as.character(i)
    d$chem_name <- as.character(analytes[i])
    d
  }))
  in_order <- function(summ) {
    split <- unique(summ$location_code[duplicated(summ$location_code)])
    if (length(split) > 0) {
      which_series <- toString(utils::head(analytes[as.integer(split)], 5))
      stop(glue::glue(
        "The series behind {which_series} hold results in more than one ",
        "unit or guideline set, which one row ",
        "of statistics cannot describe. Run mann_kendall_test() on a table ",
        "from data_processor(); it tests each unit as a separate series."
      ))
    }
    summ[match(as.character(seq_along(nested)), summ$location_code), ]
  }

  summ <- summary_stats(
    samples,
    include_criteria = length(sets) > 0,
    criteria_col = dplyr::all_of(sets),
    lor_multiplier = lor_multiplier
  )
  guidelines <- guideline_sheet_columns(
    in_order(summ),
    sets,
    attr(summ, "criteria_names")
  )

  # The sample count is on the sheet already, counted from the same series,
  # and the unit is in the analyte's name where it needs saying.
  summ <- guidelines$table
  summ <- summ[setdiff(
    names(summ),
    c("location_code", "chem_name", "output_unit", "criteria_set", "n_samples")
  )]
  summ[] <- lapply(summ, finite_or_na)

  list(table = summ, labels = guidelines$labels, formats = guidelines$formats)
}

#' Swap summary_stats()'s guideline columns for the statistics sheet's
#'
#' [summary_stats()] names each set's pair after its columns - `criteria_99`
#' and `criteria_99_exceedance_count`. The sheet heads them with the set's
#' name instead, and reads `-` rather than 0 for a count where there was no
#' guideline to exceed.
#'
#' @param summ [summary_stats()] output, with `include_criteria` for `sets`
#' @param sets the guideline value columns, in the order to write them
#' @param set_names each set's recorded name, as `recorded_set_names()`
#'   gives them and [summary_stats()] attaches them. `NULL` where the
#'   attribute has been lost, which heads the sets as though none were named.
#' @returns a list of `table`, `summ` with each set's pair moved to the end as
#'   `guideline__<set>` and `exceedances__<set>`; and the `labels` and
#'   `formats` of those columns
#' @noRd
guideline_sheet_columns <- function(summ, sets, set_names = NULL) {
  prefixes <- vapply(
    sets,
    function(set) {
      criteria_set_label(
        if (set %in% names(set_names)) set_names[[set]] else NA_character_,
        set,
        several = length(sets) > 1
      )
    },
    character(1)
  )
  # Two sets joined under one name are told apart by their columns.
  clash <- duplicated(prefixes) | duplicated(prefixes, fromLast = TRUE)
  prefixes[clash] <- paste0(prefixes[clash], " (", sets[clash], ")")

  counts <- vapply(
    sets,
    function(set) paste0(comparison_columns(set)[["exceedance"]], "_count"),
    character(1)
  )
  out <- summ[setdiff(names(summ), c(sets, counts))]

  labels <- character(0)
  formats <- character(0)
  for (i in seq_along(sets)) {
    set <- sets[[i]]
    guideline <- finite_or_na(summ[[set]])
    exceedances <- summ[[counts[[i]]]]
    # summary_stats() counts no exceedances where there was nothing to exceed,
    # but a 0 there would read as compliance.
    exceedances[is.na(guideline)] <- NA

    cols <- paste0(c("guideline__", "exceedances__"), set)
    out[[cols[1]]] <- guideline
    out[[cols[2]]] <- exceedances

    labels[cols] <- trimws(paste(
      prefixes[[set]],
      c("Guideline", "Exceedances")
    ))
    formats[cols] <- c("GENERAL", "0")
  }

  list(table = out, labels = labels, formats = formats)
}

#' The guideline columns `criteria_col` asks for
#'
#' Resolved with [dplyr::select()], so `c(criteria_95, criteria_99)`, quoted
#' names and helpers such as `starts_with()` all work. For [mka_to_excel()]
#' that is against the nested data, where the guidelines ride once
#' [join_action_levels()] has run ahead of [mann_kendall_test()].
#'
#' @param criteria_col what was passed as `criteria_col`, as a quosure
#' @param criteria_named whether it was passed at all. Left at its default,
#'   `criteria` is used where it is there and skipped where it is not, and
#'   any other set joined alongside is named in a message rather than
#'   dropped unremarked.
#' @param template a zero-row frame with the columns to select from
#' @param holder,when where the columns were looked for, and when they should
#'   have been joined on, for the error where one is missing
#' @returns column names, in the order asked for; empty for none
#' @noRd
criteria_columns <- function(
  criteria_col,
  criteria_named,
  template,
  holder = "the nested `data` column",
  when = " before running mann_kendall_test()"
) {
  if (rlang::quo_is_null(criteria_col)) {
    return(character(0))
  }

  if (!criteria_named) {
    picked <- intersect("criteria", names(template))
    others <- setdiff(criteria_sets(template), picked)
    if (length(others) > 0) {
      message(glue::glue(
        "Guideline set(s) {toString(others)} joined but not written. Name ",
        "every set wanted in `criteria_col`, e.g. ",
        "criteria_col = c({toString(c(picked, others))})."
      ))
    }
    return(picked)
  }

  picked <- tryCatch(
    names(dplyr::select(template, !!criteria_col)),
    error = function(e) {
      stop(
        "`criteria_col` names a column ",
        holder,
        " does not hold. Join each guideline set on with ",
        "join_action_levels()",
        when,
        ".\n",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  guideline_value_columns(picked, template)
}

#' The name each guideline set was joined under
#'
#' [join_action_levels()] records each set's name - `"NEMP 99%"`, say -
#' beside its value column, and that is what a reader will know it by.
#'
#' @param frames the data the sets were joined onto, as a list of data frames
#' @param sets the guideline value columns
#' @returns a character vector named by `sets`, `NA` for a set with no single
#'   name recorded
#' @noRd
recorded_set_names <- function(frames, sets) {
  vapply(
    sets,
    function(set) {
      name_col <- paste0(set, "_name")
      found <- unique(unlist(lapply(frames, function(d) {
        as.character(d[[name_col]])
      })))
      found <- found[!is.na(found) & nzchar(found)]
      if (length(found) == 1) found else NA_character_
    },
    character(1)
  )
}

#' The name a guideline set's columns are headed with
#'
#' The name the set was joined under, where it has one. Failing that, a lone
#' set needs no prefix at all, and one of several is told apart by its column.
#'
#' @param name the set's recorded name, or `NA`
#' @param set the guideline value column
#' @param several whether other sets are being written beside it
#' @returns a single string, possibly empty
#' @noRd
criteria_set_label <- function(name, set, several) {
  if (!is.na(name)) {
    name
  } else if (several) {
    set
  } else {
    ""
  }
}

#' A reported concentration as text, for writing after a "<"
#'
#' Up to ten significant figures and never in scientific notation, so it
#' reads as the number beside it in the column would.
#'
#' @param x numeric vector
#' @returns character vector
#' @noRd
format_reported <- function(x) {
  trimws(formatC(x, digits = 10, format = "fg"))
}

#' The non-detect substitution a statistic was calculated with, for its label
#'
#' Shared by the [summary_stats_to_excel()] headings and the [results_table()]
#' statistics, so the two say it in the same words.
#'
#' @param lor_multiplier what a non-detect's LOR was multiplied by, `NULL`
#'   where that is not known
#' @returns `"(ND at 0.5x LOR)"`, say, or `NULL` at the full LOR or where the
#'   multiplier is not known
#' @noRd
lor_basis <- function(lor_multiplier) {
  if (is.null(lor_multiplier) || isTRUE(all.equal(lor_multiplier, 1))) {
    return(NULL)
  }
  paste0("(ND at ", format(lor_multiplier), "x LOR)")
}

#' Headings and number formats for the summary_stats() columns
#'
#' A function rather than constants beside STATS_LABELS, since the percentiles
#' come from PERCENTILES in summary_stats.R, which is collated after this file.
#'
#' The mean, standard deviation and percentiles say which non-detect
#' substitution they were calculated with. At the full LOR they are "as
#' reported", and the percentiles, which a reader takes as reported anyway,
#' say nothing.
#'
#' @param lor_multiplier what [summary_stats()] multiplied a non-detect's LOR
#'   by, `NULL` where that is not known
#' @returns a list of `labels` and `formats`, each named by column
#' @noRd
summary_column_spec <- function(lor_multiplier = 1) {
  pct <- paste0("p", PERCENTILES)
  general <- c("min", "mean", "max", "std_dev", pct)

  basis <- lor_basis(lor_multiplier)
  substituted <- !is.null(basis)
  if (!substituted) {
    basis <- "(as reported)"
  }
  # Every percentile summary_stats() reports takes "th".
  percentiles <- paste0(PERCENTILES, "th Percentile")
  if (substituted) {
    percentiles <- paste(percentiles, basis)
  }

  list(
    labels = c(
      "n_detects" = "Detects",
      "n_non_detects" = "Non-Detects",
      "pct_detects" = "% Detects",
      "pct_non_detects" = "% Non-Detects",
      "min" = "Minimum",
      "mean" = paste("Mean", basis),
      "max" = "Maximum",
      "std_dev" = paste("Standard Deviation", basis),
      stats::setNames(percentiles, pct)
    ),
    formats = c(
      "n_detects" = "0",
      "n_non_detects" = "0",
      "pct_detects" = "0.0",
      "pct_non_detects" = "0.0",
      stats::setNames(rep("GENERAL", length(general)), general)
    )
  )
}

#' Add the sheet of statistics behind each trend
#'
#' Laid out to read alongside the summary sheet: the zone and well columns
#' and the trend cells take the same styles they have there. Also writes the
#' one sheet of [summary_stats_to_excel()], which has no trend column.
#'
#' @param wb the workbook to add to
#' @param stats the table from `stats_table()`: the identifier columns, the
#'   text columns, the trend where there is one, then the statistics
#' @param id_labels headings for the zone and location columns, as written on
#'   the summary sheet
#' @param labels,formats the heading and Excel number format of each
#'   statistic, named by column, from `stats_table()`
#' @param fills named fill colours
#' @param fonts named font colours
#' @param na_label the not-calculated level
#' @param header_fill,header_font colours for the header row and the first
#'   identifier column
#' @param include_zone whether the first column is the zone
#' @param merge_cols how many of the side columns, from the left, to merge a
#'   repeated value down into one block - each column's blocks within those of
#'   the column to its left. 0 merges none.
#' @param location_fill,location_font colours for the well names beside a zone
#' @param text_labels headings for the text columns between the identifiers
#'   and the trend - the analyte, and anything else to be read as a label
#'   rather than a statistic
#' @param trend whether a trend column follows the text columns
#' @param sheet the name to give the sheet
#' @returns `wb`, invisibly, modified in place
#' @noRd
add_stats_sheet <- function(
  wb,
  stats,
  id_labels,
  labels,
  formats,
  fills,
  fonts,
  na_label,
  header_fill,
  header_font,
  include_zone,
  merge_cols,
  location_fill,
  location_font,
  text_labels = "Analyte",
  trend = TRUE,
  sheet = STATS_SHEET
) {
  nd_flags <- stats[intersect(c("min_nd", "max_nd"), names(stats))]
  stats <- stats[setdiff(names(stats), names(nd_flags))]

  n_id <- length(id_labels)
  n_text <- length(text_labels)
  text_cols <- n_id + seq_len(n_text)
  trend_col <- n_id + n_text + 1
  # Everything before the first statistic.
  n_lead <- n_id + n_text + trend
  stat_names <- names(stats)[-seq_len(n_lead)]
  trends <- if (trend) stats[[trend_col]]

  # A non-detect minimum or maximum goes over its number as text, with its
  # "<", once the column has been written and styled.
  nd_cells <- lapply(intersect(c("min", "max"), stat_names), function(col) {
    flag <- nd_flags[[paste0(col, "_nd")]] & !is.na(stats[[col]])
    list(
      rows = which(flag) + 1,
      col = n_lead + match(col, stat_names),
      text = paste0("<", format_reported(stats[[col]][flag]))
    )
  })

  names(stats) <- c(
    id_labels,
    text_labels,
    if (trend) "Trend",
    unname(labels[stat_names])
  )

  openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)
  openxlsx::writeData(
    wb,
    sheet,
    stats,
    headerStyle = header_row_style(header_fill, header_font, rotate = FALSE),
    keepNA = TRUE,
    na.string = STATS_NA
  )

  n_row <- nrow(stats)
  n_col <- ncol(stats)

  if (n_row > 0) {
    rows <- 2:(n_row + 1)

    openxlsx::addStyle(
      wb,
      sheet,
      id_body_style(header_fill, header_font),
      rows = rows,
      cols = 1,
      gridExpand = TRUE
    )
    if (include_zone) {
      openxlsx::addStyle(
        wb,
        sheet,
        id_body_style(location_fill, location_font),
        rows = rows,
        cols = 2,
        gridExpand = TRUE
      )
    }

    # Nested, as results_table_to_excel() merges its side columns: an analyte
    # shared by two wells is not merged across them. Compared as written, where
    # a missing value reads STATS_NA, so a blank cell never swallows a "-".
    shown <- lapply(stats[seq_len(merge_cols)], function(x) {
      replace(format_id_values(x), is.na(x), STATS_NA)
    })
    keys <- merge_keys(as.data.frame(shown), group = NULL)
    for (j in seq_len(merge_cols)) {
      merge_column_runs(wb, sheet, keys[[j]], col = j)
    }

    openxlsx::addStyle(
      wb,
      sheet,
      stats_body_style(),
      rows = rows,
      cols = text_cols,
      gridExpand = TRUE
    )

    # A trend the palette does not name is left unformatted, as it is on the
    # summary sheet, which has already warned about it.
    for (lvl in intersect(names(fills), trends)) {
      openxlsx::addStyle(
        wb,
        sheet,
        trend_cell_style(lvl, fills, fonts, na_label),
        rows = which(trends == lvl) + 1,
        cols = trend_col,
        gridExpand = TRUE
      )
    }

    # Right-aligned, so a dash sits in line with the numbers around it.
    for (i in seq_along(stat_names)) {
      openxlsx::addStyle(
        wb,
        sheet,
        stats_body_style(formats[[stat_names[i]]], halign = "right"),
        rows = rows,
        cols = n_lead + i,
        gridExpand = TRUE
      )
    }

    for (cells in nd_cells) {
      for (k in seq_along(cells$rows)) {
        openxlsx::writeData(
          wb,
          sheet,
          cells$text[k],
          startCol = cells$col,
          startRow = cells$rows[k]
        )
      }
    }
  }

  openxlsx::setColWidths(
    wb,
    sheet,
    cols = seq_len(n_col),
    widths = c(
      if (include_zone) 20,
      16,
      28,
      rep(10, n_text - 1),
      if (trend) 21,
      rep(14, length(stat_names))
    )
  )
  # Tall enough for the longest heading to wrap within its column, at about a
  # dozen characters a line - a long guideline set name included.
  header_lines <- max(1, ceiling(nchar(labels[stat_names]) / 12))
  openxlsx::setRowHeights(
    wb,
    sheet,
    rows = 1,
    heights = max(30, 15 * header_lines)
  )
  openxlsx::freezePane(
    wb,
    sheet,
    firstActiveRow = 2,
    firstActiveCol = n_id + n_text + 1
  )
  openxlsx::addFilter(wb, sheet, rows = 1, cols = seq_len(n_col))

  invisible(wb)
}

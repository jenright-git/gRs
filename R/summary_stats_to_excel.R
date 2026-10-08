#' Formatted Excel workbook of summary statistics
#'
#' Writes the output of [summary_stats()] to a formatted `.xlsx` sheet, laid
#' out as the statistics sheet [mka_to_excel()] writes with
#' `include_summary = TRUE` but without the trend test. [mann_kendall_test()]
#' leaves out any series with too few samples or too few detects to test, so
#' that sheet describes only the series that were tested. This one describes
#' every row of the summary.
#'
#' One row per location, analyte and unit: the sample count, detect counts
#' and percentages, the minimum, mean, maximum and standard deviation, and
#' the 5th to 99th percentiles. The maximum is the highest detected result.
#' Where [summary_stats()] was run with a `lor_multiplier`, the mean,
#' standard deviation and percentile headings say so - "Mean (ND at 0.5x
#' LOR)" rather than "Mean (as reported)". A minimum or maximum that is a
#' non-detect, as [summary_stats()] flags it in `min_nd` and `max_nd`, is
#' written with its `<`, as text. Each guideline set the
#' summary carries - those named in its `criteria_col` - follows with a
#' guideline column and an exceedance count of its own, headed with the name
#' [join_action_levels()] recorded for the set. A location/analyte with no
#' guideline in a set reads `-` in both, rather than a count of 0, as does any
#' statistic that could not be calculated, such as the standard deviation of
#' a single result.
#'
#' The set names travel in the `"criteria_names"` attribute [summary_stats()]
#' attaches, which `dplyr::filter()`, `dplyr::arrange()` and the like keep.
#' Where it has been lost - by `dplyr::bind_rows()`, say - each set is headed
#' with its column name instead.
#'
#' The headings and location names are set in white on a deep green, the
#' first row and the identifier and analyte columns are frozen, and the
#' header row carries filters. Any further grouping column, from the
#' `group_vars` of [summary_stats()], is written beside the analyte. A value
#' repeated down these identifier columns - the zone, well, analyte, unit and
#' any grouping column - is merged into one block (`merge_cells`), as
#' [results_table_to_excel()] merges its side columns.
#'
#' @param data tibble from [summary_stats()], not the results behind it -
#'   choose the guideline sets there, with `include_criteria` and
#'   `criteria_col`, and group by the monitoring zone there for
#'   `include_zone`. Not one stacked by [criteria_long()]: name the sets in
#'   `criteria_col` and they are written side by side.
#' @param save_path full file path, including filename, for the workbook. The
#'   directory is created if it does not exist. Defaults to
#'   `"Summary Statistics_<yyyymmdd>.xlsx"` in the working directory.
#' @param include_zone write the monitoring zone as the first column, ahead of
#'   the location names, and sort the rows by zone. Needs the zone in `data`,
#'   from `summary_stats(group_vars = "monitoring_zone")`. Default `FALSE`.
#' @param zone_col Name of the column containing monitoring zones. Can be
#'   provided with or without quotes. Default is monitoring_zone. Locations
#'   with no zone recorded are left blank, and a location falling in more
#'   than one zone is an error.
#' @param merge_cells merge a value repeated down an identifier column - the
#'   zone, well, analyte, unit and any further grouping column - into one
#'   block, so a well is named once against all its analytes. Each column's
#'   blocks sit within those of the column to its left - an analyte measured
#'   at two wells is not merged across them. Default `TRUE`. Set `FALSE` to
#'   write the value on every row, which is what Excel's sort and filter tools
#'   want.
#' @param sheet_name name of the worksheet. Default `"Summary Statistics"`.
#' @inheritParams mka_to_excel
#'
#' @returns `data`, invisibly.
#' @export
#'
#' @seealso [summary_stats()] for the table this writes, and [mka_to_excel()]
#'   for the same statistics beside each trend.
#'
#' @examples
#' gRs_data %>%
#'   summary_stats() %>%
#'   summary_stats_to_excel(save_path = tempfile(fileext = ".xlsx"))
#'
#' # Wells grouped by monitoring zone
#' gRs_data %>%
#'   summary_stats(group_vars = "monitoring_zone") %>%
#'   summary_stats_to_excel(
#'     save_path = tempfile(fileext = ".xlsx"),
#'     include_zone = TRUE
#'   )
#'
#' # A value in every row, so the sheet sorts and filters
#' gRs_data %>%
#'   summary_stats() %>%
#'   summary_stats_to_excel(
#'     save_path = tempfile(fileext = ".xlsx"),
#'     merge_cells = FALSE
#'   )
#'
#' \dontrun{
#' # Two guideline sets side by side, a guideline and a count for each
#' gRs_data %>%
#'   join_action_levels(nemp_95, value_col = "criteria_95") %>%
#'   join_action_levels(nemp_99, value_col = "criteria_99") %>%
#'   summary_stats(
#'     include_criteria = TRUE,
#'     criteria_col = c(criteria_95, criteria_99)
#'   ) %>%
#'   summary_stats_to_excel()
#' }
#'
#' @importFrom dplyr arrange as_tibble
#' @importFrom tidyr replace_na
#' @importFrom rlang enquo quo_name syms !!!
#' @importFrom glue glue
#' @importFrom openxlsx createWorkbook saveWorkbook
summary_stats_to_excel <- function(
  data,
  save_path = paste0(
    "Summary Statistics_",
    format(Sys.Date(), "%Y%m%d"),
    ".xlsx"
  ),
  include_zone = FALSE,
  merge_cells = TRUE,
  zone_col = monitoring_zone,
  zone_label = "Monitoring Zone",
  location_label = "Monitoring Well",
  header_fill = "#008768",
  header_font = "#FFFFFF",
  location_fill = "#9BBEAF",
  location_font = header_font,
  sheet_name = "Summary Statistics",
  overwrite = TRUE
) {
  zone_name <- rlang::quo_name(rlang::enquo(zone_col))

  if (
    !is.logical(include_zone) ||
      length(include_zone) != 1 ||
      is.na(include_zone)
  ) {
    stop("`include_zone` must be TRUE or FALSE.")
  }
  check_flag(merge_cells, "merge_cells")
  # Stacked, each location/analyte has a row per set, and the set names that
  # would head them are no longer in the table.
  if ("criteria_set" %in% names(data)) {
    stop(
      "`data` comes from a table stacked by criteria_long(). Run ",
      "summary_stats() on the table from join_action_levels() instead, and ",
      "name the sets to write side by side in `criteria_col`, e.g. ",
      "criteria_col = c(criteria_95, criteria_99)."
    )
  }
  results <- results_message(data)
  if (!is.null(results)) {
    stop(results)
  }
  required <- c("location_code", "chem_name", "n_samples")
  absent <- setdiff(required, names(data))
  if (length(absent) > 0) {
    stop(
      "`data` is missing required columns: ",
      toString(absent),
      ". Pass the output of summary_stats()."
    )
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
        "`data` has no column {zone_name}. Group by it in summary_stats(), ",
        "with group_vars = \"{zone_name}\", or name the column with ",
        "`zone_col`."
      ))
    }
  }

  sets <- summary_sets(data)
  counts <- vapply(sets, exceedance_count_column, character(1))
  spec <- summary_column_spec(attr(data, "lor_multiplier"))

  # summary_stats() writes its grouping columns first, so whatever sits ahead
  # of n_samples identifies the row and everything from it on is a figure.
  first_stat <- match("n_samples", names(data))
  lead <- c(if (include_zone) zone_name, "location_code")
  text <- c(
    "chem_name",
    intersect("output_unit", names(data)),
    setdiff(
      names(data)[seq_len(first_stat - 1)],
      c(lead, "chem_name", "output_unit")
    )
  )
  figures <- setdiff(names(data), c(lead, text))
  # A column of the caller's own, added after the statistics, is written under
  # its own name.
  extra <- setdiff(
    figures,
    c("n_samples", names(spec$labels), "min_nd", "max_nd", sets, counts)
  )

  table <- dplyr::as_tibble(data)[c(lead, text, figures)]
  if (include_zone) {
    table[[zone_name]] <- tidyr::replace_na(
      as.character(table[[zone_name]]),
      ""
    )
    check_one_zone_per_location(
      table$location_code,
      table[[zone_name]],
      zone_name
    )
  }
  # Zones read down the sheet in blocks, wells alphabetically within each.
  table <- dplyr::arrange(table, !!!rlang::syms(c(lead, text)))

  guidelines <- guideline_sheet_columns(
    table,
    sets,
    attr(data, "criteria_names")
  )
  table <- guidelines$table
  table[] <- lapply(table, finite_or_na)

  wb <- openxlsx::createWorkbook()
  add_stats_sheet(
    wb,
    table,
    id_labels = c(if (include_zone) zone_label, location_label),
    labels = c(
      "n_samples" = "Samples",
      spec$labels,
      stats::setNames(extra, extra),
      guidelines$labels
    ),
    formats = c(
      "n_samples" = "0",
      spec$formats,
      stats::setNames(rep("GENERAL", length(extra)), extra),
      guidelines$formats
    ),
    fills = NULL,
    fonts = NULL,
    na_label = STATS_NA,
    header_fill = header_fill,
    header_font = header_font,
    include_zone = include_zone,
    merge_cols = if (merge_cells) length(c(lead, text)) else 0L,
    location_fill = location_fill,
    location_font = location_font,
    text_labels = column_labels(text),
    trend = FALSE,
    sheet = sheet_name
  )
  save_workbook(wb, save_path, overwrite)

  invisible(data)
}

#' The guideline sets a summary_stats() table carries
#'
#' A set is a value column with the exceedance count [summary_stats()] writes
#' beside it, `criteria_99` and `criteria_99_exceedance_count`.
#'
#' @param summ a summary_stats() table
#' @returns the value column names, in the order they sit in `summ`
#' @noRd
summary_sets <- function(summ) {
  nms <- names(summ)
  nms[vapply(
    nms,
    function(nm) exceedance_count_column(nm) %in% nms,
    logical(1)
  )]
}

#' The column summary_stats() counts a guideline set's exceedances in
#'
#' @param set the guideline value column
#' @returns a single column name
#' @noRd
exceedance_count_column <- function(set) {
  paste0(comparison_columns(set)[["exceedance"]], "_count")
}

#' Headings for the text columns of the summary statistics sheet
#'
#' The analyte and unit are headed as they are on the [mka_to_excel()]
#' statistics sheet. Any further grouping column takes its label from the
#' dictionary [create_gt()] uses, or failing that its own name.
#'
#' @param cols the text column names
#' @returns a character vector of headings
#' @noRd
column_labels <- function(cols) {
  labels <- table_labels(c(chem_name = "Analyte", output_unit = "Unit"))
  out <- unname(labels[cols])
  out[is.na(out)] <- cols[is.na(out)]
  out
}

#' The error for `data` that holds results rather than a summary_stats() table
#'
#' `data %>% summary_stats_to_excel()` skips the summary_stats() step that
#' chooses the guideline sets and groups, so the error gives the call to make,
#' with the sets `data` carries.
#'
#' @param data the table passed to [summary_stats_to_excel()]
#' @returns the message, or `NULL` where `data` is not results
#' @noRd
results_message <- function(data) {
  if (!"concentration" %in% names(data) || "n_samples" %in% names(data)) {
    return(NULL)
  }

  sets <- criteria_sets(data)
  call <- if (length(sets) == 0) {
    "summary_stats()"
  } else if (identical(sets, "criteria")) {
    "summary_stats(include_criteria = TRUE)"
  } else if (length(sets) == 1) {
    paste0("summary_stats(include_criteria = TRUE, criteria_col = ", sets, ")")
  } else {
    paste0(
      "summary_stats(include_criteria = TRUE, criteria_col = c(",
      toString(sets),
      "))"
    )
  }

  paste0(
    "`data` holds results, not a summary_stats() table. Summarise it ",
    "first: data %>% ",
    call,
    " %>% summary_stats_to_excel()."
  )
}

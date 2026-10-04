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
#' sample ID and lab report. A value repeated down a side column is merged
#' into one block (`merge_cells`), and the rows can be split into groups -
#' by monitoring round, say - each under a banner naming it (`group_by`).
#'
#' Analytes are ordered by chemical group, then by name, read so that a
#' dissolved analyte sits beside its total: where the table holds both,
#' "Dissolved Copper" follows "Copper" rather than filing under D.
#' `sort_analytes_by` orders them by another column instead, such as the
#' `chem_code`, without showing it. `analytes` keeps only those detected,
#' with a guideline, or exceeding one - a wide suite is often mostly
#' non-detects.
#'
#' `layout = "analytes_down"` turns the table on its side: analytes down the
#' left, grouped under their chemical group, with each guideline set as a
#' column beside them, and the samples across the top. It suits a long suite
#' measured in few samples. Every option works in either layout.
#'
#' `statistics` adds summary rows - the number of results and of detects,
#' the minimum and maximum, and the exceedances of each guideline set - over
#' the results the table shows, and with `statistics_by_group` for each
#' `group_by` group as well. The maximum is the highest detected result, as
#' [summary_stats()] reports it; only where nothing was detected is it the
#' highest LOR, written with its `<`.
#'
#' Notes below the table give the key to the formatting, the guideline
#' sets' full names - shorten them in the table with `criteria_labels` - and
#' how the statistics were worked out. `title` heads the table.
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
#' @param group_by column or columns to split the rows by, e.g.
#'   `"monitoring_round"`. Each group is written under a banner across the
#'   table - "Round: 2024 Q1" - with its rows below it. Name an element to
#'   set the banner's heading, as for `id_cols`. Rounds (`monitoring_round`,
#'   `task_code`) are ordered by the date they were sampled, so "2024 Q10"
#'   follows "2024 Q9"; dates and numbers are ordered as such, and other
#'   text alphabetically. A missing value reads "not recorded" and comes
#'   last. A column named here and in `id_cols` is shown in both. Default
#'   `NULL`, no groups.
#' @param sort_analytes_by column or columns of `data` to order the analytes
#'   by within each chemical group, without showing them - `"chem_code"` to
#'   order by CAS number, say, or a lab report-order column. Each analyte
#'   takes the smallest value among its results, numbers in numeric order and
#'   text alphabetically. Ties fall back to the name, so a dissolved analyte
#'   sharing its total's code still sits beside it. Default `NULL`, ordering
#'   by name.
#' @param analytes which analytes to show: `"all"` (default); `"detected"`,
#'   those detected at least once; `"with_guideline"`, those with a guideline
#'   in a set shown; or `"exceeding"`, those with at least one shaded result.
#'   A sample left with no result to show is left out too, and the notes say
#'   what was filtered.
#' @param criteria_labels short names for the guideline sets, used in the
#'   table, its legend and its statistics, while the notes keep each set's
#'   full name. Named by the set's column or its full name, e.g.
#'   `c(criteria_99 = "ANZG 99%")`; sets not named keep their full names.
#' @param statistics summary rows at the foot of the table: `TRUE` for the
#'   number of results, the number of detects, the minimum, the maximum and
#'   the exceedances of each guideline set, or a selection of `"n"`,
#'   `"n_detects"`, `"min"`, `"max"`, `"mean"`, `"median"` and
#'   `"exceedances"`. The mean and median are of the results as reported,
#'   non-detects at their LOR, to 4 significant figures. An analyte with no
#'   guideline in a set reads `-` for that set's exceedances. In
#'   `analytes_down` they are columns at the right. Default `FALSE`.
#' @param statistics_by_group add a block of `statistics` at the foot of each
#'   `group_by` group as well as the whole table's. It needs both
#'   `statistics` and `group_by`, and says so where either is missing.
#'   Default `FALSE`.
#' @param layout `"samples_down"` (default), one row per sample and one
#'   column per analyte; or `"analytes_down"`, one row per analyte, grouped by
#'   chemical group, and one column per sample.
#' @param title the table's title. In gt, the table's heading; in Excel, the
#'   centre of the page header, printed at the top of every page. Default
#'   `NULL`, no title.
#' @param notes `TRUE` (default) writes notes below the table: the key to
#'   its formatting, each guideline set's full name, how the statistics were
#'   worked out, any analytes filtered, and the date. `FALSE` writes none;
#'   a character vector adds its lines after them.
#' @param merge_cells merge a value repeated down a side column into one
#'   block, so a well is named once against all its samples. Each column's
#'   blocks sit within those of the column to its left - a date shared by two
#'   wells is not merged across them - and no block crosses a `group_by`
#'   banner. Default `TRUE`. In gt, which cannot merge cells, the repeats are
#'   left blank instead. `FALSE` writes the value on every row.
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
#' @param paper_size,orientation the printed page: `"A3"` (default), `"A2"`,
#'   `"A4"`, `"A5"`, `"letter"`, `"legal"` or `"tabloid"`, and `"landscape"`
#'   (default) or `"portrait"`. A gt table takes them when saved as RTF with
#'   `gt::gtsave()`; in a Quarto or Word document, the document sets the
#'   page.
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
#' # A banner per monitoring round; metals ordered by CAS number
#' compared %>%
#'   results_table(group_by = "monitoring_round", sort_analytes_by = "chem_code")
#'
#' # Only analytes detected, with statistics, a title and short set names
#' compared %>%
#'   results_table(
#'     analytes = "detected",
#'     statistics = TRUE,
#'     criteria_labels = c(criteria = "ANZG 95%"),
#'     title = "Table 3: Surface Water Analytical Results"
#'   )
#'
#' # On its side: analytes down, samples across
#' compared %>% results_table(layout = "analytes_down")
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
  group_by = NULL,
  sort_analytes_by = NULL,
  analytes = "all",
  highlight_lor = FALSE,
  criteria_colours = NULL,
  criteria_labels = NULL,
  statistics = FALSE,
  statistics_by_group = FALSE,
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
  header_fill = "#008768",
  header_font = "#FFFFFF",
  location_fill = "#9BBEAF",
  location_font = header_font
) {
  rlang::check_installed(
    "gt",
    reason = "to build a table with results_table()."
  )
  page <- page_spec(paper_size, orientation)
  check_choice(layout, LAYOUTS, "layout")
  check_title(title)
  check_notes(notes)

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
    merge_cells = merge_cells,
    include_zone = include_zone,
    zone_name = rlang::quo_name(rlang::enquo(zone_col)),
    zone_label = zone_label,
    location_label = location_label
  )

  build <- if (identical(layout, "analytes_down")) results_gt_down else results_gt
  build(
    xtab,
    page = page,
    title = title,
    notes = results_notes(xtab, notes),
    header_fill = header_fill,
    header_font = header_font,
    location_fill = location_fill,
    location_font = location_font
  )
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
#' @param group_by,sort_analytes_by,analytes,highlight_lor as for
#'   [results_table()]
#' @param criteria_colours,criteria_labels,statistics,statistics_by_group as
#'   for [results_table()]
#' @param merge_cells,include_zone,zone_label,location_label as for
#'   [results_table()]
#' @param zone_name the zone column's name, as a string
#' @returns a list:
#'   * `ids`: one row per table row - the zone, the location and the
#'     `id_cols` - as their own types, sorted by group and then by these;
#'   * `id_labels`: a heading for each of those columns;
#'   * `include_zone`: whether the first of them is the zone;
#'   * `group`: each row's banner, or `NULL` without `group_by`. A group's
#'     rows are contiguous;
#'   * `merge_cells`, `merge_keys`: whether to merge, and for each side
#'     column a key per row that changes wherever a merged block must end;
#'   * `analytes`: one row per analyte column, `chem_group`, `chem_name` and
#'     `output_unit`;
#'   * `conc`, `nd`: matrices of the result in each cell and whether it is a
#'     non-detect, `NA` where the sample was not analysed for the analyte;
#'   * `fill`: matrix of the guideline set each cell is shaded for, `NA` for
#'     none;
#'   * `hits`: one logical matrix per set, of the cells exceeding it - every
#'     set a cell exceeds, where `fill` names only the highest;
#'   * `sets`, `set_labels`, `set_names`, `set_colours`: each guideline set's
#'     column, the name it is shown under, its full name and its colour;
#'   * `guideline_value`, `guideline_text`: matrices, one row per set and one
#'     column per analyte, of the guideline as a number (`NA` where there is
#'     none, or a range) and as text (`""` where there is none);
#'   * `stats`: the summary statistics, from `results_statistics()`;
#'   * `axis`: what runs along the sample direction, in order, from
#'     `sample_axis()`;
#'   * `filtered`: what `analytes` left out, for the notes; `shared`: the
#'     number of cells whose results differed; `highlight_lor`.
#' @noRd
results_crosstab <- function(
  data,
  criteria_col,
  criteria_named,
  id_cols,
  id_named,
  group_by = NULL,
  sort_analytes_by = NULL,
  analytes = "all",
  highlight_lor = FALSE,
  criteria_colours = NULL,
  criteria_labels = NULL,
  statistics = FALSE,
  statistics_by_group = FALSE,
  merge_cells = TRUE,
  include_zone = FALSE,
  zone_name = "monitoring_zone",
  zone_label = "Monitoring Zone",
  location_label = "Monitoring Well"
) {
  check_flag(highlight_lor, "highlight_lor")
  check_flag(include_zone, "include_zone")
  check_flag(merge_cells, "merge_cells")
  check_flag(statistics_by_group, "statistics_by_group")
  check_choice(analytes, ANALYTE_FILTERS, "analytes")
  # `analytes` names the table of analyte columns below.
  analyte_filter <- analytes
  statistics <- resolve_statistics(statistics)

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
  groups <- resolve_named_columns(group_by, data, "group_by")
  sort_cols <- unname(resolve_named_columns(sort_analytes_by, data, "sort_analytes_by"))
  lacking <- c(
    if (length(groups) == 0) "`group_by`",
    if (length(statistics) == 0) "`statistics`"
  )
  if (statistics_by_group && length(lacking) > 0) {
    message(
      "`statistics_by_group = TRUE` does nothing without ",
      paste(lacking, collapse = " and "),
      "; no statistics were added for each group."
    )
  }

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
  if (length(sets) == 0 && analyte_filter %in% c("with_guideline", "exceeding")) {
    stop(
      "`analytes = \"", analyte_filter, "\"` keeps analytes by their ",
      "guidelines, but no guideline set is shown. Name the sets in ",
      "`criteria_col`, or use analytes = \"all\".",
      call. = FALSE
    )
  }

  conc <- suppressWarnings(as.numeric(data$concentration))
  # As join_action_levels() decides it: only a "Y" is a detect.
  nd <- if ("detect_flag" %in% names(data)) {
    !(as.character(data$detect_flag) %in% "Y")
  } else {
    as.character(data$prefix) %in% "<"
  }

  # --- columns: one per analyte and unit, by chemical group then name
  analyte <- data.frame(
    chem_group = column_or_na(data, "chem_group"),
    chem_name = as.character(data$chem_name),
    output_unit = column_or_na(data, "output_unit"),
    stringsAsFactors = FALSE
  )
  analytes <- unique(analyte)
  rownames(analytes) <- NULL
  analytes <- analytes[
    analyte_order(analytes, analyte, data, sort_cols),
    ,
    drop = FALSE
  ]
  rownames(analytes) <- NULL
  col_id <- match(row_keys(analyte), row_keys(analytes))

  # --- rows: one per group, location and id_cols combination
  key_cols <- unique(c(unname(groups), lead))
  keys <- dplyr::as_tibble(as.data.frame(data)[key_cols])
  if (include_zone) {
    keys[[zone_name]] <- tidyr::replace_na(as.character(keys[[zone_name]]), "")
    check_one_zone_per_location(
      keys$location_code,
      keys[[zone_name]],
      zone_name
    )
  }
  rows <- dplyr::distinct(keys)
  rows <- rows[
    do.call(
      order,
      c(group_order_keys(rows, groups, data), unname(as.list(rows[lead])))
    ),
    ,
    drop = FALSE
  ]
  row_id <- match(row_keys(keys), row_keys(rows))
  group <- group_labels(rows, groups)

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
  set_names <- character(0)
  hits <- list()

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
    # Every set each shown result exceeds, not only the highest: the
    # statistics count each set's exceedances.
    hits <- lapply(seq_len(k), function(j) {
      exceeded <- matrix(FALSE, n_row, n_col)
      exceeded[at] <- hit[kept, j]
      exceeded
    })

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
    set_names <- vapply(
      seq_len(k),
      function(j) criteria_set_label(recorded[[j]], sets[[j]], several = TRUE),
      character(1)
    )
    # Two sets joined under one name are told apart by their columns.
    clash <- duplicated(set_names) | duplicated(set_names, fromLast = TRUE)
    set_names[clash] <- paste0(set_names[clash], " (", sets[clash], ")")
  }

  # --- which analytes to show, and the samples left with something to show
  shown <- analytes_to_show(analyte_filter, conc_m, nd_m, fill_m, guideline_text)
  filtered <- list(
    analytes = analyte_filter,
    shown = sum(shown),
    of = n_col,
    dropped_rows = 0L
  )
  if (!all(shown)) {
    if (!any(shown)) {
      stop(
        ANALYTE_FILTER_EMPTY[[analyte_filter]],
        ", so the table would be empty. Use analytes = \"all\".",
        call. = FALSE
      )
    }
    keep_rows <- rowSums(!is.na(conc_m[, shown, drop = FALSE])) > 0
    filtered$dropped_rows <- sum(!keep_rows)
    message(
      "Showing ", sum(shown), " of ", n_col, " analytes (",
      ANALYTE_FILTER_TEXT[[analyte_filter]], ")",
      if (filtered$dropped_rows > 0) {
        paste0(
          "; ", filtered$dropped_rows,
          " sample(s) left with no result to show were left out"
        )
      },
      "."
    )
    analytes <- analytes[shown, , drop = FALSE]
    rownames(analytes) <- NULL
    conc_m <- conc_m[keep_rows, shown, drop = FALSE]
    nd_m <- nd_m[keep_rows, shown, drop = FALSE]
    fill_m <- fill_m[keep_rows, shown, drop = FALSE]
    hits <- lapply(hits, function(m) m[keep_rows, shown, drop = FALSE])
    guideline_value <- guideline_value[, shown, drop = FALSE]
    guideline_text <- guideline_text[, shown, drop = FALSE]
    rows <- rows[keep_rows, , drop = FALSE]
    group <- group[keep_rows]
  }

  set_labels <- short_set_labels(criteria_labels, sets, set_names)
  ids_out <- rows[lead]
  xtab <- list(
    ids = ids_out,
    id_labels = c(if (include_zone) zone_label, location_label, names(ids)),
    include_zone = include_zone,
    group = group,
    merge_cells = merge_cells,
    merge_keys = merge_keys(ids_out, group),
    analytes = analytes,
    conc = conc_m,
    nd = nd_m,
    fill = fill_m,
    hits = hits,
    sets = sets,
    set_labels = set_labels,
    set_names = set_names,
    set_colours = unname(resolve_set_colours(
      criteria_colours,
      sets,
      set_names,
      set_labels
    )),
    guideline_value = guideline_value,
    guideline_text = guideline_text,
    highlight_lor = highlight_lor,
    filtered = filtered,
    shared = shared
  )
  xtab$stats <- results_statistics(xtab, statistics, statistics_by_group)
  xtab$axis <- sample_axis(xtab$group, nrow(conc_m), xtab$stats)
  xtab
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
      stop_if_absent(cols, data, "id_cols")
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


#' Settle the columns an argument names, each with a heading
#'
#' For `group_by` and `sort_analytes_by`, which have no default to fall back
#' on: every column named must be in `data`. A name on an element is its
#' heading; an unnamed one takes its heading from `id_heading()`.
#'
#' @param cols the argument's value
#' @param data the results
#' @param arg the argument's name, for the errors
#' @returns a character vector of column names, named by their headings
#' @noRd
resolve_named_columns <- function(cols, data, arg) {
  if (is.null(cols) || length(cols) == 0) {
    return(stats::setNames(character(0), character(0)))
  }
  if (!is.character(cols)) {
    stop(
      "`", arg, "` must be a character vector of column names, e.g. ",
      "\"monitoring_round\".",
      call. = FALSE
    )
  }

  headings <- names(cols)
  if (is.null(headings)) {
    headings <- rep("", length(cols))
  }
  headings[is.na(headings)] <- ""
  cols <- unname(cols)
  keep <- !is.na(cols) & nzchar(cols) & !duplicated(cols)
  cols <- cols[keep]
  headings <- headings[keep]
  stop_if_absent(cols, data, arg)

  auto <- !nzchar(headings)
  headings[auto] <- vapply(cols[auto], id_heading, character(1))
  stats::setNames(cols, headings)
}


#' Stop where an argument names columns `data` does not have
#'
#' @param cols the columns named
#' @param data the results
#' @param arg the argument's name
#' @returns `NULL`, invisibly, where every column is present
#' @noRd
stop_if_absent <- function(cols, data, arg) {
  absent <- setdiff(cols, names(data))
  if (length(absent) == 0) {
    return(invisible(NULL))
  }
  hints <- unique(unlist(lapply(absent, near_names, names(data))))
  stop(
    "`", arg, "` names column(s) `data` does not have: ",
    toString(absent),
    ".",
    if (length(hints) > 0) paste0(" Did you mean ", toString(hints), "?") else "",
    call. = FALSE
  )
}


#' The order analytes are written across the table
#'
#' By chemical group, blanks last; then by any `sort_analytes_by` columns;
#' then by name, read so that a dissolved analyte sits beside its total; then
#' by unit.
#'
#' @param analytes the distinct analyte columns: `chem_group`, `chem_name`
#'   and `output_unit`
#' @param analyte the same three columns, one row per result
#' @param data the results
#' @param sort_cols columns of `data` to order by ahead of the name
#' @returns an integer ordering of the rows of `analytes`
#' @noRd
analyte_order <- function(analytes, analyte, data, sort_cols) {
  keys <- list(is.na(analytes$chem_group), analytes$chem_group)
  if (length(sort_cols) > 0) {
    id <- match(row_keys(analyte), row_keys(analytes))
    for (col in sort_cols) {
      value <- analyte_sort_values(data[[col]], id, nrow(analytes))
      keys <- c(keys, list(is.na(value), value))
    }
  }
  pairs <- dissolved_pairs(analytes$chem_name)
  keys <- c(
    keys,
    list(
      tolower(pairs$key),
      pairs$dissolved,
      analytes$chem_name,
      analytes$output_unit
    )
  )
  do.call(order, unname(keys))
}


#' The value each analyte column is sorted by
#'
#' @param x the sort column, one value per result
#' @param id the analyte column each result belongs to
#' @param n the number of analyte columns
#' @returns one value per analyte column, the smallest among its results:
#'   numeric for a number or date column, text otherwise
#' @noRd
analyte_sort_values <- function(x, id, n) {
  by_analyte <- split(x, factor(id, levels = seq_len(n)))
  if (is.numeric(x) || inherits(x, c("Date", "POSIXt"))) {
    vapply(
      by_analyte,
      function(v) {
        v <- as.numeric(v[!is.na(v)])
        if (length(v) == 0) NA_real_ else min(v)
      },
      numeric(1),
      USE.NAMES = FALSE
    )
  } else {
    vapply(
      by_analyte,
      function(v) {
        v <- sort(as.character(v[!is.na(v)]))
        if (length(v) == 0) NA_character_ else v[[1]]
      },
      character(1),
      USE.NAMES = FALSE
    )
  }
}


#' Read a dissolved analyte as its total, for sorting
#'
#' [data_processor()] names a filtered result "Dissolved <name>". Where the
#' table also holds the analyte without the prefix - its total - the two
#' sort as one name, the total first, so they sit side by side. A name with
#' no total beside it, such as "Dissolved Oxygen", keeps its own place.
#'
#' @param name the analyte names
#' @returns list(key, dissolved): the name to sort by, and whether the
#'   analyte is the dissolved half of a pair
#' @noRd
dissolved_pairs <- function(name) {
  base <- sub("^dissolved\\s+", "", name, ignore.case = TRUE, perl = TRUE)
  dissolved <- !is.na(name) & base != name & tolower(base) %in% tolower(name)
  list(key = ifelse(dissolved, base, name), dissolved = dissolved)
}


#' Sort keys putting the table's rows in group order
#'
#' Dates and numbers order themselves. A round recorded as text does not -
#' "2024 Q10" sorts before "2024 Q9" - so a round column is ordered by the
#' earliest date sampled in each round, falling back to the text where
#' `data` carries no date. Other text is alphabetical. A missing value comes
#' last.
#'
#' @param rows the distinct rows of the table, holding the group columns
#' @param groups the group columns
#' @param data the results, for the dates a round is ordered by
#' @returns a list of vectors to pass to order(), one group column after
#'   another
#' @noRd
group_order_keys <- function(rows, groups, data) {
  keys <- list()
  for (col in unname(groups)) {
    v <- rows[[col]]
    keys <- c(keys, list(is.na(v)))
    if (!(is.numeric(v) || inherits(v, c("Date", "POSIXt")))) {
      v <- as.character(v)
      dates <- if (col %in% ROUND_COLUMNS) round_ordering_dates(data, col)
      if (!is.null(dates)) {
        first <- suppressWarnings(tapply(
          as.numeric(dates),
          as.character(data[[col]]),
          min,
          na.rm = TRUE
        ))
        keys <- c(keys, list(unname(first[v])))
      }
    }
    keys <- c(keys, list(v))
  }
  keys
}


#' The banner over each row's group
#'
#' @param rows the distinct rows of the table, holding the group columns
#' @param groups the group columns, named by their headings
#' @returns a character vector, one per row - "Round: 2024 Q1", several
#'   columns joined by " | " - or `NULL` where there are no groups
#' @noRd
group_labels <- function(rows, groups) {
  if (length(groups) == 0) {
    return(NULL)
  }
  parts <- lapply(seq_along(groups), function(i) {
    v <- rows[[groups[[i]]]]
    shown <- format_id_values(v)
    shown[is.na(v)] <- "not recorded"
    paste0(names(groups)[[i]], ": ", shown)
  })
  do.call(paste, c(parts, sep = " | "))
}


#' Where each side column's merged blocks end
#'
#' A block in a column ends wherever that column's value changes, or the
#' value of any column to its left, or the group - so blocks nest, and none
#' crosses a banner. Values are compared as the table shows them.
#'
#' @param ids the side columns, in table order
#' @param group each row's banner, or `NULL`
#' @returns a list with one character vector per column: a row whose key
#'   matches the row above's continues its block
#' @noRd
merge_keys <- function(ids, group) {
  key <- if (is.null(group)) rep("", nrow(ids)) else group
  out <- vector("list", length(ids))
  for (j in seq_along(ids)) {
    key <- paste(key, format_id_values(ids[[j]]), sep = "\r")
    out[[j]] <- key
  }
  out
}


# Paper a results table can be printed on: the Excel paperSize code
# (ECMA-376) and the portrait width and height in inches, as gt takes them.
PAPER_SIZES <- list(
  A2 = list(code = 66, width = 16.54, height = 23.39),
  A3 = list(code = 8, width = 11.69, height = 16.54),
  A4 = list(code = 9, width = 8.27, height = 11.69),
  A5 = list(code = 11, width = 5.83, height = 8.27),
  letter = list(code = 1, width = 8.5, height = 11),
  legal = list(code = 5, width = 8.5, height = 14),
  tabloid = list(code = 3, width = 11, height = 17)
)


#' Settle the printed page
#'
#' @param paper_size,orientation as for [results_table()]
#' @returns a list: `code`, `width` and `height` from `PAPER_SIZES`, and
#'   `orientation`
#' @noRd
page_spec <- function(paper_size, orientation) {
  size <- match(tolower(paper_size), tolower(names(PAPER_SIZES)))
  if (length(paper_size) != 1 || is.na(size)) {
    stop(
      "`paper_size` must be one of ",
      toString(paste0("\"", names(PAPER_SIZES), "\"")),
      ".",
      call. = FALSE
    )
  }
  orientation <- tolower(orientation)
  if (length(orientation) != 1 || !orientation %in% c("landscape", "portrait")) {
    stop("`orientation` must be \"landscape\" or \"portrait\".", call. = FALSE)
  }
  c(PAPER_SIZES[[size]], list(orientation = orientation))
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
#' @param full the sets' full names
#' @param labels the names the sets are shown under, from `criteria_labels`
#' @returns a character vector of colours, one per set, named by set column
#' @noRd
resolve_set_colours <- function(colours, sets, full, labels = full) {
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

  out[match_sets(nms, sets, full, labels, "criteria_colours")] <- unname(colours)
  out
}


#' Find the guideline sets an argument's names refer to
#'
#' A set can be named by its column, its full name or the short label
#' `criteria_labels` gave it.
#'
#' @param keys the names given
#' @param sets,full,labels the sets' columns, full names and labels
#' @param arg the argument's name, for the error
#' @returns the index of the set each key names
#' @noRd
match_sets <- function(keys, sets, full, labels = full, arg) {
  where <- match(keys, sets)
  where[is.na(where)] <- match(keys[is.na(where)], full)
  where[is.na(where)] <- match(keys[is.na(where)], labels)
  if (anyNA(where)) {
    stop(
      "`", arg, "` names no guideline set in the table: ",
      toString(keys[is.na(where)]),
      ". Name a set by its column (",
      toString(sets),
      ") or by its name (",
      toString(unique(c(full, labels))),
      ").",
      call. = FALSE
    )
  }
  where
}


#' The names the guideline sets are shown under
#'
#' @param criteria_labels the `criteria_labels` argument
#' @param sets the sets' columns
#' @param full the sets' full names
#' @returns a character vector, one per set: its short label where given,
#'   its full name otherwise
#' @noRd
short_set_labels <- function(criteria_labels, sets, full) {
  if (is.null(criteria_labels) || length(criteria_labels) == 0) {
    return(full)
  }
  keys <- names(criteria_labels)
  if (
    !is.character(criteria_labels) ||
      is.null(keys) ||
      any(is.na(keys) | !nzchar(keys))
  ) {
    stop(
      "`criteria_labels` must be a named character vector, e.g. ",
      "c(criteria_99 = \"ANZG 99%\").",
      call. = FALSE
    )
  }
  out <- full
  out[match_sets(keys, sets, full, arg = "criteria_labels")] <-
    unname(criteria_labels)
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


#' Stop unless an argument is one of its choices
#'
#' @param x the argument's value
#' @param choices the values it may take
#' @param arg the argument's name
#' @returns `NULL`, invisibly
#' @noRd
check_choice <- function(x, choices, arg) {
  if (!is.character(x) || length(x) != 1 || !x %in% choices) {
    stop(
      "`", arg, "` must be one of ",
      toString(paste0("\"", choices, "\"")),
      ".",
      call. = FALSE
    )
  }
  invisible(NULL)
}


#' Stop unless `title` is a single string or NULL
#'
#' @param title the `title` argument
#' @returns `NULL`, invisibly
#' @noRd
check_title <- function(title) {
  if (!is.null(title) && (!is.character(title) || length(title) != 1)) {
    stop("`title` must be a single string, or NULL.", call. = FALSE)
  }
  invisible(NULL)
}


#' Stop unless `notes` is TRUE, FALSE or lines of text
#'
#' @param notes the `notes` argument
#' @returns `NULL`, invisibly
#' @noRd
check_notes <- function(notes) {
  ok <- (is.logical(notes) && length(notes) == 1 && !is.na(notes)) ||
    is.character(notes)
  if (!ok) {
    stop(
      "`notes` must be TRUE, FALSE or a character vector of lines to add.",
      call. = FALSE
    )
  }
  invisible(NULL)
}


# The layouts a results table comes in.
LAYOUTS <- c("samples_down", "analytes_down")

# What `analytes` may keep, and how each is said in the notes and messages.
ANALYTE_FILTERS <- c("all", "detected", "with_guideline", "exceeding")
ANALYTE_FILTER_TEXT <- c(
  all = "shown",
  detected = "detected in at least one sample",
  with_guideline = "with a guideline in a set shown",
  exceeding = "exceeding a guideline at least once"
)
# The error where a filter leaves nothing to show.
ANALYTE_FILTER_EMPTY <- c(
  detected = "No analyte in `data` was detected",
  with_guideline = "No analyte in `data` has a guideline in the sets shown",
  exceeding = "No result in `data` exceeds a guideline"
)


#' Which analyte columns `analytes` keeps
#'
#' @param filter the `analytes` argument
#' @param conc,nd,fill the crosstab's matrices
#' @param guideline_text the guidelines, one row per set
#' @returns a logical vector, one per analyte column
#' @noRd
analytes_to_show <- function(filter, conc, nd, fill, guideline_text) {
  switch(
    filter,
    all = rep(TRUE, ncol(conc)),
    detected = colSums(!is.na(conc) & !nd, na.rm = TRUE) > 0,
    # nzchar() drops a matrix's shape.
    with_guideline = colSums(matrix(
      nzchar(guideline_text),
      nrow = nrow(guideline_text),
      ncol = ncol(guideline_text)
    )) > 0,
    exceeding = colSums(!is.na(fill)) > 0
  )
}


# The statistics a table can carry, in the order they are written, and the
# set `statistics = TRUE` gives.
STATISTICS <- c("n", "n_detects", "min", "max", "mean", "median", "exceedances")
STATISTICS_DEFAULT <- c("n", "n_detects", "min", "max", "exceedances")
STATISTIC_LABELS <- c(
  n = "Results (n)",
  n_detects = "Detects (n)",
  min = "Minimum",
  max = "Maximum",
  mean = "Mean",
  median = "Median"
)


#' Settle which statistics to show
#'
#' @param statistics the `statistics` argument
#' @returns a character vector of `STATISTICS`, in their written order
#' @noRd
resolve_statistics <- function(statistics) {
  if (is.null(statistics) || isFALSE(statistics)) {
    return(character(0))
  }
  if (isTRUE(statistics)) {
    return(STATISTICS_DEFAULT)
  }
  if (!is.character(statistics)) {
    stop(
      "`statistics` must be TRUE, FALSE or a selection of ",
      toString(paste0("\"", STATISTICS, "\"")),
      ".",
      call. = FALSE
    )
  }
  unknown <- setdiff(statistics, STATISTICS)
  if (length(unknown) > 0) {
    stop(
      "`statistics` names no statistic gRs reports: ",
      toString(unknown),
      ". Choose from ",
      toString(paste0("\"", STATISTICS, "\"")),
      ".",
      call. = FALSE
    )
  }
  STATISTICS[STATISTICS %in% statistics]
}


#' Work out the summary statistics a table carries
#'
#' Over the results the table shows - one per cell - for each analyte
#' column: the whole table's, and with `by_group` each group's too.
#'
#' @param xtab a crosstab, as `results_crosstab()` builds it
#' @param which the statistics, from `resolve_statistics()`
#' @param by_group also work them out for each `group_by` group
#' @returns a list: `text` and `value`, matrices with a row per statistic
#'   and a column per analyte - the statistic as shown, and as a number
#'   where it is one (`NA` for a `<` value or nothing to report); `label`;
#'   `set`, the guideline set an exceedance row counts (`NA` otherwise);
#'   `scope`, the group a row describes (`NA` for the whole table); and
#'   `first`, whether a row starts its block
#' @noRd
results_statistics <- function(xtab, which, by_group) {
  n_col <- ncol(xtab$conc)
  empty <- list(
    text = matrix("", 0, n_col),
    value = matrix(NA_real_, 0, n_col),
    label = character(0),
    set = integer(0),
    scope = character(0),
    first = logical(0)
  )
  if (length(which) == 0) {
    return(empty)
  }

  scopes <- list()
  if (by_group && !is.null(xtab$group)) {
    for (g in unique(xtab$group)) {
      scopes[[length(scopes) + 1]] <- list(scope = g, rows = which(xtab$group == g))
    }
  }
  scopes[[length(scopes) + 1]] <- list(
    scope = NA_character_,
    rows = seq_len(nrow(xtab$conc))
  )

  blocks <- lapply(scopes, function(s) {
    block <- statistic_block(xtab, which, s$rows)
    block$scope <- rep(s$scope, length(block$label))
    block$first <- seq_along(block$label) == 1
    block
  })
  list(
    text = do.call(rbind, c(list(empty$text), lapply(blocks, `[[`, "text"))),
    value = do.call(rbind, c(list(empty$value), lapply(blocks, `[[`, "value"))),
    label = unlist(lapply(blocks, `[[`, "label")),
    set = unlist(lapply(blocks, `[[`, "set")),
    scope = unlist(lapply(blocks, `[[`, "scope")),
    first = unlist(lapply(blocks, `[[`, "first"))
  )
}


#' One block of summary statistics, over some of a table's rows
#'
#' @param xtab a crosstab
#' @param which the statistics
#' @param rows the rows of the crosstab the block describes
#' @returns a list of `text`, `value`, `label` and `set`, as
#'   `results_statistics()` returns them
#' @noRd
statistic_block <- function(xtab, which, rows) {
  n_col <- ncol(xtab$conc)
  text <- list()
  value <- list()
  label <- character(0)
  set <- integer(0)

  for (stat in which) {
    if (identical(stat, "exceedances")) {
      for (j in seq_along(xtab$sets)) {
        counts <- vapply(
          seq_len(n_col),
          function(cc) {
            has <- !is.na(xtab$conc[rows, cc])
            # Nothing to exceed, or nothing to count: a 0 would read as
            # compliance.
            if (!nzchar(xtab$guideline_text[j, cc]) || !any(has)) {
              return(NA_real_)
            }
            as.numeric(sum(xtab$hits[[j]][rows, cc] & has))
          },
          numeric(1)
        )
        text[[length(text) + 1]] <- ifelse(
          is.na(counts),
          STATS_NA,
          format(counts, trim = TRUE)
        )
        value[[length(value) + 1]] <- counts
        label <- c(label, paste0("Exceedances: ", xtab$set_labels[[j]]))
        set <- c(set, j)
      }
      next
    }
    cells <- lapply(seq_len(n_col), function(cc) {
      one_statistic(stat, xtab$conc[rows, cc], xtab$nd[rows, cc])
    })
    text[[length(text) + 1]] <- vapply(cells, `[[`, "", "text")
    value[[length(value) + 1]] <- vapply(cells, `[[`, 0, "value")
    label <- c(label, STATISTIC_LABELS[[stat]])
    set <- c(set, NA_integer_)
  }

  list(
    text = matrix(unlist(text), ncol = n_col, byrow = TRUE),
    value = matrix(unlist(value), ncol = n_col, byrow = TRUE),
    label = label,
    set = set
  )
}


#' One statistic of one analyte's results
#'
#' The minimum and maximum follow [summary_stats()]: the maximum is the
#' highest detect, and only where nothing was detected the highest LOR, with
#' its `<`; the minimum carries a `<` where a non-detect is the lowest.
#'
#' @param stat the statistic
#' @param conc,nd the results and whether each is a non-detect
#' @returns list(text, value): as shown, and as a number where it is one
#' @noRd
one_statistic <- function(stat, conc, nd) {
  keep <- !is.na(conc)
  x <- conc[keep]
  flag <- ifelse(nd[keep], "N", "Y")
  count <- function(n) list(text = format(n), value = as.numeric(n))
  if (identical(stat, "n")) {
    return(count(length(x)))
  }
  if (identical(stat, "n_detects")) {
    return(count(sum(flag == "Y")))
  }
  if (length(x) == 0) {
    return(list(text = STATS_NA, value = NA_real_))
  }
  extreme <- function(v, below) {
    if (below) {
      list(text = paste0("<", format_reported(v)), value = NA_real_)
    } else {
      list(text = format_reported(v), value = v)
    }
  }
  switch(
    stat,
    min = extreme(safe_min(x), nd_extremes(x, flag)[[1]]),
    max = extreme(max_detected(x, flag), nd_extremes(x, flag)[[2]]),
    mean = {
      v <- signif(mean(x), 4)
      list(text = format_reported(v), value = v)
    },
    median = {
      v <- signif(stats::median(x), 4)
      list(text = format_reported(v), value = v)
    }
  )
}


#' What runs along a table's sample direction, in order
#'
#' Each group's banner, then its samples, then its statistics; the whole
#' table's statistics last. The rows of a `samples_down` table, and the
#' columns of an `analytes_down` one.
#'
#' @param group each sample's group, or `NULL`
#' @param n the number of samples
#' @param stats the statistics, from `results_statistics()`
#' @returns a data frame of `kind` (`"banner"`, `"sample"` or `"stat"`),
#'   `index` (the sample, or the statistic's row in `stats`) and `group`
#' @noRd
sample_axis <- function(group, n, stats) {
  overall <- which(is.na(stats$scope))
  entry <- function(kind, index, group) {
    data.frame(
      kind = rep(kind, length(index)),
      index = as.integer(index),
      group = rep(group, length(index)),
      stringsAsFactors = FALSE
    )
  }
  if (is.null(group)) {
    return(rbind(
      entry("sample", seq_len(n), NA_character_),
      entry("stat", overall, NA_character_)
    ))
  }
  parts <- lapply(unique(group), function(g) {
    rbind(
      entry("banner", NA_integer_, g),
      entry("sample", which(group == g), g),
      entry("stat", which(!is.na(stats$scope) & stats$scope == g), g)
    )
  })
  rbind(do.call(rbind, parts), entry("stat", overall, NA_character_))
}


#' The notes written below a results table
#'
#' @param xtab a crosstab
#' @param notes the `notes` argument
#' @returns a data frame as `results_legend()` returns, with a `key` of
#'   `NA` for a line of plain text; no rows where `notes = FALSE`
#' @noRd
results_notes <- function(xtab, notes) {
  if (isFALSE(notes)) {
    return(results_legend(xtab)[0, ])
  }
  line <- function(text) {
    data.frame(
      key = NA_character_,
      meaning = text,
      fill = NA_character_,
      bold = FALSE,
      grey = FALSE,
      stringsAsFactors = FALSE
    )
  }
  out <- list(results_legend(xtab))

  # Each set by the name it is shown under, its full name beside a short one.
  if (length(xtab$sets) > 0) {
    named <- ifelse(
      xtab$set_labels == xtab$set_names,
      xtab$set_names,
      paste0(xtab$set_labels, " (", xtab$set_names, ")")
    )
    out[[length(out) + 1]] <- line(paste0(
      if (length(named) > 1) "Guidelines: " else "Guideline: ",
      paste(named, collapse = "; "),
      "."
    ))
  }

  shown <- xtab$stats$label
  if (length(shown) > 0) {
    if ("Minimum" %in% shown || "Maximum" %in% shown) {
      out[[length(out) + 1]] <- line(paste(
        "Minimum: the lowest result, with < where a non-detect is the lowest.",
        "Maximum: the highest detected result; where nothing was detected,",
        "the highest LOR, with <."
      ))
    }
    if ("Mean" %in% shown || "Median" %in% shown) {
      out[[length(out) + 1]] <- line(paste(
        "Mean and median: of the results as reported, with non-detects at",
        "their LOR, to 4 significant figures."
      ))
    }
    if (any(!is.na(xtab$stats$set))) {
      out[[length(out) + 1]] <- line(paste0(
        "Exceedances: the results above each guideline",
        if (xtab$highlight_lor) {
          ", counting non-detects whose LOR is above it"
        } else {
          ""
        },
        "; - where an analyte has no guideline in that set."
      ))
    }
    if (any(!is.na(xtab$stats$scope))) {
      out[[length(out) + 1]] <- line(
        "Statistics are given for each group and for the whole table."
      )
    }
  }

  filtered <- xtab$filtered
  if (!identical(filtered$analytes, "all")) {
    out[[length(out) + 1]] <- line(paste0(
      "Only analytes ", ANALYTE_FILTER_TEXT[[filtered$analytes]],
      " are shown (", filtered$shown, " of ", filtered$of, ")."
    ))
  }
  if (xtab$shared > 0) {
    out[[length(out) + 1]] <- line(paste(
      "Where a sample has more than one result for an analyte, the highest",
      "detected result is shown, or the highest LOR where none was detected."
    ))
  }
  out[[length(out) + 1]] <- line(paste0(
    "Generated ", format(Sys.Date(), "%d/%m/%Y"), " with gRs."
  ))
  if (is.character(notes)) {
    for (extra in notes) {
      out[[length(out) + 1]] <- line(extra)
    }
  }
  do.call(rbind, out)
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


# The fill behind a block of summary statistics, and the rule above it.
RESULTS_STATS_FILL <- "#F2F2F2"
RESULTS_STATS_RULE <- "#7F7F7F"


#' Build the gt table from a crosstab, samples down
#'
#' The guideline sets are the first rows of the body, shaded in their
#' colours, since gt has no header row to hold a value per column. The
#' analyte's unit goes under its name in the column label. With `group_by`,
#' the guideline rows are a row group of their own ahead of the banners, and
#' the whole table's statistics a row group after them.
#'
#' gt cannot merge cells, so a merged block is drawn by blanking the repeats
#' below its first row and hiding the lines between them.
#'
#' @param xtab a crosstab from `results_crosstab()`
#' @param page the printed page, from `page_spec()`
#' @param title the `title` argument
#' @param notes the notes, from `results_notes()`
#' @param header_fill,header_font,location_fill,location_font as for
#'   [results_table()]
#' @returns a `gt_tbl`
#' @noRd
results_gt <- function(
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
  lead_names <- paste0(".id_", seq_len(n_lead))
  analyte_names <- paste0(".analyte_", seq_len(n_col))
  stats <- xtab$stats

  ids <- matrix(
    unlist(lapply(xtab$ids, format_id_values), use.names = FALSE),
    nrow = n_row
  )
  # A row continuing the block above it in a side column, blanked there.
  repeats <- matrix(FALSE, n_row, n_lead)
  if (xtab$merge_cells && n_row > 1) {
    for (j in seq_len(n_lead)) {
      key <- xtab$merge_keys[[j]]
      repeats[, j] <- c(FALSE, key[-1] == key[-n_row])
    }
    ids[repeats] <- ""
  }
  cells <- result_cell_text(xtab$conc, xtab$nd)

  # The body below the guideline rows, in the order the axis gives: each
  # group's samples and statistics, the whole table's statistics last.
  axis <- xtab$axis[xtab$axis$kind != "banner", , drop = FALSE]
  is_sample <- axis$kind == "sample"
  lines_lead <- matrix("", nrow(axis), n_lead)
  lines_value <- matrix("", nrow(axis), n_col)
  lines_lead[is_sample, ] <- ids[axis$index[is_sample], , drop = FALSE]
  lines_value[is_sample, ] <- cells[axis$index[is_sample], , drop = FALSE]
  lines_lead[!is_sample, 1] <- stats$label[axis$index[!is_sample]]
  lines_value[!is_sample, ] <- stats$text[axis$index[!is_sample], , drop = FALSE]

  set_rows <- matrix("", k, n_lead)
  set_rows[, 1] <- xtab$set_labels
  body <- rbind(
    cbind(set_rows, xtab$guideline_text),
    cbind(lines_lead, lines_value)
  )
  body <- as.data.frame(body, stringsAsFactors = FALSE)
  names(body) <- c(lead_names, analyte_names)

  body_row <- k + seq_len(nrow(axis))
  sample_row <- integer(n_row)
  sample_row[axis$index[is_sample]] <- body_row[is_sample]
  stat_row <- body_row[!is_sample]
  stat_index <- axis$index[!is_sample]

  grouped <- !is.null(xtab$group)
  if (grouped) {
    body$.group <- c(
      rep("Guideline values", k),
      ifelse(is.na(axis$group), "Summary statistics", axis$group)
    )
    tbl <- gt::gt(body, groupname_col = ".group")
  } else {
    tbl <- gt::gt(body)
  }

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
  tbl <- gt_house_style(tbl, page, header_fill, header_font, spanned)

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

  # --- the zone and location, as the house sheets set them
  id_style <- function(fill, font) {
    list(gt::cell_fill(color = fill), gt::cell_text(color = font, weight = "bold"))
  }
  if (xtab$include_zone) {
    tbl <- gt::tab_style(
      tbl,
      style = id_style(header_fill, header_font),
      locations = gt::cells_body(columns = lead_names[[1]], rows = sample_row)
    )
    tbl <- gt::tab_style(
      tbl,
      style = id_style(location_fill, location_font),
      locations = gt::cells_body(columns = lead_names[[2]], rows = sample_row)
    )
  } else {
    tbl <- gt::tab_style(
      tbl,
      style = id_style(header_fill, header_font),
      locations = gt::cells_body(columns = lead_names[[1]], rows = sample_row)
    )
  }

  # --- merged blocks: no line between a block's rows
  for (j in seq_len(n_lead)) {
    continuing <- which(repeats[, j])
    if (length(continuing) > 0) {
      tbl <- gt::tab_style(
        tbl,
        style = gt::cell_borders(sides = "top", style = "hidden"),
        locations = gt::cells_body(
          columns = lead_names[[j]],
          rows = sample_row[continuing]
        )
      )
    }
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
        locations = gt::cells_body(columns = column, rows = sample_row[detects])
      )
    }
    if (length(faded) > 0) {
      tbl <- gt::tab_style(
        tbl,
        style = gt::cell_text(color = RESULTS_ND_FONT),
        locations = gt::cells_body(columns = column, rows = sample_row[faded])
      )
    }
    for (j in seq_len(k)) {
      shaded <- which(xtab$fill[, cc] %in% xtab$sets[[j]])
      if (length(shaded) > 0) {
        tbl <- gt::tab_style(
          tbl,
          style = gt::cell_fill(color = xtab$set_colours[[j]]),
          locations = gt::cells_body(columns = column, rows = sample_row[shaded])
        )
      }
    }
  }

  # --- the statistics: grey rows, each exceedance label in its set's colour
  if (length(stat_row) > 0) {
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_fill(color = RESULTS_STATS_FILL),
      locations = gt::cells_body(columns = c(lead_names[-1], analyte_names), rows = stat_row)
    )
    set_of <- stats$set[stat_index]
    plain <- stat_row[is.na(set_of)]
    if (length(plain) > 0) {
      tbl <- gt::tab_style(
        tbl,
        style = gt::cell_fill(color = RESULTS_STATS_FILL),
        locations = gt::cells_body(columns = lead_names[[1]], rows = plain)
      )
    }
    for (j in unique(stats::na.omit(set_of))) {
      tbl <- gt::tab_style(
        tbl,
        style = gt::cell_fill(color = xtab$set_colours[[j]]),
        locations = gt::cells_body(
          columns = lead_names[[1]],
          rows = stat_row[set_of %in% j]
        )
      )
    }
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = lead_names[[1]], rows = stat_row)
    )
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_borders(
        sides = "top",
        color = RESULTS_STATS_RULE,
        weight = gt::px(2)
      ),
      locations = gt::cells_body(rows = stat_row[stats$first[stat_index]])
    )
  }

  # --- banners, set as the Excel sheet sets them
  if (grouped) {
    tbl <- gt::tab_style(
      tbl,
      style = list(
        gt::cell_fill(color = location_fill),
        gt::cell_text(color = location_font, weight = "bold")
      ),
      locations = gt::cells_row_groups()
    )
  }

  gt_titles_and_notes(tbl, title, notes)
}


#' Set a results gt table out in the house style
#'
#' @param tbl the `gt_tbl`
#' @param page the printed page, from `page_spec()`
#' @param header_fill,header_font as for [results_table()]
#' @param spanned whether the table has column spanners to style
#' @returns `tbl`
#' @noRd
gt_house_style <- function(tbl, page, header_fill, header_font, spanned) {
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
    source_notes.font.size = gt::px(11),
    page.orientation = page$orientation,
    page.width = paste0(page$width, "in"),
    page.height = paste0(page$height, "in")
  )
  header_cells <- list(gt::cells_column_labels())
  if (spanned) {
    header_cells <- c(header_cells, list(gt::cells_column_spanners()))
  }
  gt::tab_style(
    tbl,
    style = gt::cell_text(color = header_font, weight = "bold"),
    locations = header_cells
  )
}


#' Head a results gt table with its title, and foot it with its notes
#'
#' @param tbl the `gt_tbl`
#' @param title the `title` argument
#' @param notes the notes, from `results_notes()`
#' @returns `tbl`
#' @noRd
gt_titles_and_notes <- function(tbl, title, notes) {
  if (!is.null(title)) {
    tbl <- gt::tab_header(tbl, title = title)
  }
  for (i in seq_len(nrow(notes))) {
    key <- notes$key[[i]]
    note <- if (!is.na(key) && nzchar(key)) {
      key_style <- c(
        if (!is.na(notes$fill[[i]])) {
          paste0(
            "background-color:", notes$fill[[i]],
            ";padding:0 4px;border:1px solid #BFBFBF"
          )
        },
        if (notes$bold[[i]]) "font-weight:bold",
        if (notes$grey[[i]]) paste0("color:", RESULTS_ND_FONT)
      )
      paste0(
        "<span style=\"", paste(key_style, collapse = ";"), "\">",
        escape_html(key),
        "</span>: ",
        escape_html(notes$meaning[[i]])
      )
    } else {
      escape_html(notes$meaning[[i]])
    }
    tbl <- gt::tab_source_note(tbl, gt::html(note))
  }
  tbl
}

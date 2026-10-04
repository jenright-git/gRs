# gRs (development version)

## Breaking changes

* `create_dt()` has been removed, and with it the dependency on DT. Use
  `create_gt()` for a formatted table, or call `DT::datatable()` directly
  for an interactive one.

* `summary_stats()` reports the highest *detected* result as `max`, as
  `analyte_summary()` does, falling back to the highest result - flagged by
  `max_nd` - only where nothing was detected. Before, a non-detect whose LOR
  sat above every detect was reported as the maximum. The mean, standard
  deviation and percentiles are unchanged, of every result as reported.
  `summary_stats_to_excel()`, `mka_to_excel(include_summary = TRUE)` and
  `create_gt()` report the new maximum.

## New features

* `data_processor()` and `action_level_processor()` accept a data frame as
  well as a file path, so the ESdat OData feeds read with esdatr
  (`esdatr::get_esdat_odata_chemistry()` and friends) go straight in. The
  feeds are the views the Excel exports are made from, so a feed gives the
  same table as its export: the report family is detected from the columns,
  and date-times sent as text are converted. `Water_Depth_bgl`, from
  `ESdat_Water_Depths`, is read as `water_depth`. EQuIS chemistry read with
  `AEQuIS::get_equis_chemistry()` goes in the same way; those tables carry
  only the numeric `FACILITY_ID`, which becomes `site_id` (as text) where no
  facility code is present.

* `action_level_processor()` reads guidelines straight from either database,
  giving the same table as the export: ESdat's
  `ESdat_Environmental_Standards` feed
  (`esdatr::get_esdat_odata_action_levels()`) and
  EQuIS's `DT_ACTION_LEVEL_PARAMETER` table
  (`AEQuIS::get_equis_odata_all()`). Each row's set - `Action_Level_Source`
  or `ACTION_LEVEL_CODE` - becomes `criteria_name`, so one read can hold
  several sets; `name` relabels a table holding one, and is an error against
  several rather than merging them. ESdat's `Applies_To_Total_Result` and
  `Applies_To_Filtered_Result` become `total` and `filtered`; EQuIS's single
  `FRACTION` sets one or the other. A guideline with a lower bound
  (`Action_Level_Min`, `ACTION_LEVEL_MIN`) is a range and is dropped and
  reported, as a `"6.5 - 8.5"` cell in an export is. `name` is only required
  for a data frame that names no set.

* `summary_stats()` takes several guideline sets at once:
  `criteria_col = c(criteria_95, criteria_99)` adds a guideline column and an
  exceedance count for each set, side by side and in the order named, each
  counted against that set's own verdicts. Quoted names, `all_of()` and
  helpers such as `starts_with("criteria")` work too; a helper's catch of a
  set's `_name`, `_unit` and verdict columns is dropped. A single set gives
  the same columns as before.

* `summary_stats()` gains `lor_multiplier`, as `half_lor()` and
  `mann_kendall_test()` take it: the mean, standard deviation and
  percentiles take non-detects at their LOR times the multiplier (0.5 for
  half LOR), while the counts, minimum and maximum stay as reported.
  `summary_stats_to_excel()` heads the changed columns with the multiplier
  used, e.g. "Mean (ND at 0.5x LOR)".

* `summary_stats()` gains `group_vars`, for further grouping columns such as
  `"monitoring_zone"` ahead of the location, and `min_nd` and `max_nd`
  columns flagging a minimum or maximum that is a non-detect. With
  `include_criteria = TRUE` it attaches each set's name, as recorded by
  `join_action_levels()`, in a `"criteria_names"` attribute.

## Bug fixes

* `summary_stats()` groups by `output_unit` and `criteria_set` where present,
  as `analyte_summary()` and `historical_range()` already did. An analyte
  reported in two units now gets a row per unit instead of one pooled mean,
  and a table stacked by `criteria_long()` gets a row per guideline set.
  Before, every result was counted once per set (doubling `n_samples` for
  two sets), the exceedances of all sets were added together, and the lowest
  set's guideline was reported with a warning wrongly blaming units. The
  extra grouping columns appear in the output and in the `tidy_path` export.

* `data_processor()` now prefixes `Dissolved` onto EQuIS dissolved results
  (fraction `D`) as it does ESDAT filtered ones (`F`), so the two fractions
  of an analyte are no longer pooled under one `chem_name`.

* `join_action_levels()` folds EQuIS's QA/QC matrix codes `WQ` and `SQ`, and
  `WL`, `WM` and `WF`, into water and soil, so their results meet water and
  soil guidelines rather than none.

* `mka_to_excel()` writes the output of `mann_kendall_test()` to a formatted
  Excel workbook: one row per location, one column per analyte, the trend in
  each cell, and the cell filled by trend direction - green for decreasing
  through neutral grey to orange for increasing. Analyte headers are rotated so
  a wide suite still fits on a page, and the first row and column are frozen so
  the well name stays visible while scrolling across.

  Location/analyte pairs the test could not reach - too few samples, too few
  detects - are absent from its output and so pivot to `NA`. They are written
  as `"NC"` rather than left blank, so a gap reads as "not calculated" rather
  than as an oversight. `na_label` sets that text.

  The analyte headings across the top and the location names down the side are
  set in white on a deep green, as is the legend's header row. `header_fill` and
  `header_font` change both sheets together.

  Colours are passed as a named vector or list keyed by trend category, e.g.
  `trend_colours = c(Increasing = "#E06666")`. Only the categories named are
  changed, so one colour can be overridden without restating the rest;
  `font_colours` sets the text colours the same way. A category the palette
  does not name is left unformatted with a warning rather than failing.

  A second sheet explains what each category means and whether it is
  favourable, built from whatever palette was used, so a recoloured workbook
  stays self-documenting. `legend = FALSE` omits it.

  `include_zone = TRUE` writes the monitoring zone as a further column ahead of
  the well names, sorts the rows by zone and then well, and merges each zone's
  repeated cells into one block, so a suite reads zone by zone. Because
  `mann_kendall_test()` nests by location and analyte, the zone is not a column
  of its output; it is read back out of the nested `data` column, so no join is
  needed first. A well that falls in two zones is an error rather than a
  silently duplicated row. `merge_zones = FALSE` leaves a value in every row for
  sorting and filtering, `zone_label` sets the heading, and `zone_col` names the
  column where it is not `monitoring_zone`.

  The well names are set a shade back from the zone beside them - `#9BBEAF`
  against the zone's deep green - so the grouping reads at a glance. The header
  row above stays one colour across. `location_fill` and `location_font` change
  those names. Without a zone column the well names keep the deep green they
  always had.

  `include_stats = TRUE` adds a `Statistics` sheet between the summary and the
  legend, setting out the test behind each trend: one row per location and
  analyte, sorted as the summary is, with the trend cell coloured and styled
  exactly as it is there, followed by the number of samples tested, the S
  statistic, Kendall's tau, the p-value, and the mean, standard deviation and
  coefficient of variation of the concentrations. Values are written
  unrounded, with number formats setting only what is displayed, and the
  header row carries a filter. Where every value tested is the same - most
  often non-detects all at one LOR - the test cannot give tau or a p-value,
  and those cells read `-` rather than Excel's `#NUM!`.

  `include_summary = TRUE` adds the `summary_stats()` columns to that sheet -
  detect counts and percentages, minimum, mean, maximum, standard deviation and
  every percentile - calculated from the nested series, so they describe
  exactly the samples each trend was tested on. They use concentrations as
  reported, so the two means and standard deviations are labelled "as tested"
  and "as reported"; they agree at the default `lor_multiplier = 1`. A minimum
  or maximum that is a non-detect is written with its `<`, as text, while
  detected values stay numbers.

  Guideline sets joined on with `join_action_levels()` before the trend test
  follow, each as a guideline column and an exceedance count of its own:
  `criteria_col = c(criteria_95, criteria_99)` writes both sets side by side,
  in that order, each counted from its own exceedance verdicts and headed with
  the set's name ("NEMP 99% Guideline"). Quoted names, `all_of()` and helpers
  such as `starts_with("criteria")` work too, without catching the name, unit
  and verdict columns that sit beside each set. Left at its default,
  `criteria` is written where it is there, and any other set joined alongside
  is named in a message rather than dropped unremarked. A pair with no
  guideline in a set reads `-` in both columns rather than a count of 0, and
  naming `criteria_col` without `include_summary = TRUE` warns rather than
  being silently ignored. The zone and well columns follow
  `include_zone`, `merge_zones` and `location_fill` as the summary does.

* `mka_to_excel(na_label = "")` no longer fails with "subscript out of
  bounds"; the blank cells are styled and explained in the legend as `"NC"`
  is. `sheet_name = "Legend"` alongside the legend sheet is now an error that
  says so, rather than openxlsx's duplicate sheet error.

* `timeseries_plot()` draws each analyte's own guideline. Faceted by analyte,
  each panel used to get the lowest guideline of every analyte plotted, with
  a warning blaming mixed units; where analytes share a panel, each now gets
  a line in its own colour.

* `timeseries_plot()` keeps results outside `ymin` and `ymax`, running them
  off the edge of the panel, rather than dropping them from the plot - which
  hid exactly the high results an exceedance plot is drawn to show.

# gRs 0.1.0

## New features

* `analyte_summary()` summarises a single monitoring round, one row per
  analyte: samples, detects, the concentration range, the location the maximum
  came from, and - where a guideline set has been joined on by
  `join_action_levels()` - the guideline, the number of exceedances, and the
  locations that exceeded it.

  The maximum reported is the highest *detected* result, falling back to the
  highest result (with its `<` carried through) where nothing was detected. An
  analyte with no guideline at all reports `NA` for the exceedance columns
  rather than `0`, which would say a guideline applied and nothing exceeded it.

  The round defaults to the latest one in the data. A round recorded as text is
  ordered by the latest date sampled within it, so `"2025 Q10"` is not read as
  earlier than `"2025 Q9"`. Name a different column with `round_col`, or a
  different round with `round`.

  Passing the output of `criteria_long()` gives one row per analyte per
  guideline set, with no further arguments.

* `exceedance_summary()` returns the results that exceeded their guideline in
  a monitoring round, one row per exceeding result, worst first within each
  analyte. It reads the same `exceedance` column as `analyte_summary()` and
  `historical_range()`, so the three cannot disagree about what exceeded.

  `include_lor = TRUE` also returns the non-detects whose limit of reporting
  sits above the guideline - too high to demonstrate compliance, which is a
  finding rather than a pass - told apart by the `lor_above_criteria` column.

  The exceeding locations for each analyte are attached as a `"locations"`
  attribute for writing inline text. Every analyte that had a guideline
  appears, so one that exceeded nothing gives `character(0)` rather than
  `NULL`.

* `historical_range()` compares a monitoring round against the record behind
  it, one row per location and analyte: the historical range, the round's own
  result, and flags for a new maximum, a new minimum, and a spike well above
  the previous detected maximum. The per-location range table and the
  "new maximums" screening list are the same call with a different `keep`.

  The conventions are deliberately asymmetric. The historical minimum spans
  all prior results, so a non-detect can set one; the historical maximum spans
  prior *detections* only, since limits of reporting fall over the life of a
  program and an old `<0.5` would otherwise stand as a maximum that was never
  measured. A new maximum therefore needs a detection on both sides.

  History is what was sampled *before* the round, so a round taken from the
  middle of the record is compared against what preceded it rather than
  against results that had not been collected yet.

  Where a location holds more than one result for the round, the maximum is
  reported and the groups are named in a warning pointing at
  `select_max_concentration()`.

* `min_max_locations()` returns the highest and lowest results of a monitoring
  round, one row per selected result, carrying the location and everything else
  the result was reported with. `n_max` and `n_min` set how many to take from
  each end - the peak and the four cleanest locations behind it, say - and
  `group_vars` ranks within a zone rather than across the site.

  It reports the maximum under the same rule as `analyte_summary()`: the
  highest *detected* result, falling back to the highest result with its `<`
  carried through where nothing was detected. The minimum is the lowest result,
  detected or not. Results of equal concentration share a rank, and
  `with_ties = TRUE` keeps every result tied with the last one taken rather
  than breaking the tie on `location_code`.

* `create_gt()` formats a summary table for a report, as the counterpart of
  `create_dt()`. It merges each `<x>_prefix`/`<x>_conc` pair into one cell so a
  non-detect reads `<0.05`, formats concentrations, counts and percentages
  apart, and labels columns from a shared dictionary. `gt` is in `Suggests`, so
  it is only needed by those who call this.

  A guideline set joined under its own `value_col` names its columns after
  itself - `criteria_99`, `criteria_99_n_exceedances` - and these are now read
  as that set's columns: labelled as though the set had been joined under the
  default name, and counted as counts rather than formatted as concentrations.

  Where a table carries two sets, whether bound side by side or joined onto the
  same results, each set's columns are put under a spanner naming the set, so
  "Criteria" appearing twice is not ambiguous. Name the sets by labelling their
  value columns, e.g.
  `labels = c(criteria = "ANZG 95%", criteria_99 = "ANZG 99%")`.

  The monitoring round the table reports on is added as a source note, read
  from the attribute `analyte_summary()` and `historical_range()` already
  attach - so a printed table says which round it describes. Switch it off with
  `round_note = FALSE`.

  The findings are marked on the cell they describe rather than left as a
  column of `TRUE`s and `0`s for the reader to find: a new maximum is bold, a
  new minimum italic and underlined, and a spike shaded, all on the current
  concentration and footnoted with the factor `historical_range()` used; an
  exceedance is bold, on the count or verdict itself, for every guideline set
  the table carries.

  `new_max`, `new_min` and `spike` are hidden once the styling has said them -
  whether or not the round tripped them, so the table keeps one shape from
  round to round. `max_ratio` and the exceedance counts stay, being numbers
  worth reading rather than flags the styling replaces. `highlight = FALSE`
  leaves the table unstyled with the flags shown as columns.

  A minimum and a maximum are one fact - the range the results span - so they
  are merged into a single cell: `historical_range()`'s two history columns
  become `0.5 - 8` under "Historical Range", and `analyte_summary()`'s become
  "Concentration Range". `merge_range = FALSE` reports the two ends apart.

  A range with one end to report reads as that end alone. A group holding a
  single result would otherwise read "5 - 5", and one whose only history is a
  non-detect - a minimum with no detected maximum behind it - would read
  "5 - ", both saying the range is wider than the record shows.

  A detected result no longer renders on a second line inside its cell. An
  absent prefix was left to `sub_missing()`, which blanks a cell with a line
  break, and merging then put that break in front of the value.

## Breaking changes

* `establish_plotting_variables()` has been removed. It assigned seven
  variables into the global environment with `<<-`. Use
  `get_plotting_variables()`, which returns the same values as a named list.

* `mann_kendall_reduced_test()` has been removed. It errored on every call,
  and `mann_kendall_test(traditional = TRUE)` produces the same three trend
  categories.

* Arguments naming a column are now spelled the same way in every function.
  `analyte_col` (`timeseries_plot()`, `mann_kendall_test()`) and `chem_col`
  (`select_max_concentration()`) are both now `chem_name_col`.

* `half_lor()`'s `multiplier` argument is now `lor_multiplier`, matching
  `mann_kendall_test()` and `mk_analysis()`.

* `summary_stats()`'s `value_col` argument is now `criteria_col`, matching
  `timeseries_plot()`, and accepts the column name with or without quotes.

* `mann_kendall_heatmap_bw()` now takes the output of
  `mann_kendall_test(traditional = TRUE)`. Its previous input,
  `mann_kendall_reduced_test()`, has been removed.

* `get_plotting_variables()` no longer takes `use_rcolorbrewer`. RColorBrewer
  is used when it is installed.

* `gRs_data` has been brought into line with what `data_processor()` returns:
  `total_or_filtered` is now `fraction`, and `detect_flag` has been added. It
  could not be passed to `summary_stats()` before.

## Bug fixes

* `scale_x_limitval()` drew its markers with `geom_hline()`, so every plot
  using it failed to build. It now uses `geom_vline()`.

* `plot_by_analyte()` passed the analyte and monitoring zone to
  `timeseries_plot()` in place of the arguments given in `...`, so every plot
  errored. Arguments in `...` now reach `timeseries_plot()` as documented.

* `plot_by_analyte(create_dirs = FALSE)` still wrote into a per-zone
  subdirectory that was never created, so every save failed. Plots now go to
  `save_path` itself, with the zone in the file name.

* `plot_by_analyte()` now sanitises analyte and zone names before using them
  as file names, so an analyte such as `Nitrate/Nitrite as N` is written
  rather than failing to open a connection.

* `summary_stats()` errored on any group holding a missing concentration -
  its percentiles were computed without `na.rm = TRUE` while every other
  statistic used it. A group with no readable concentration now reports `NA`
  for `min` and `max` rather than `Inf`.

* `summary_stats()` no longer requires `chem_group`, `fraction`, `prefix` or
  `date`, which `data_processor()` does not guarantee. It names the columns
  it genuinely needs when one is absent.

* `select_max_concentration()` returns the table unchanged, with a warning,
  when there is no `sample_type` column, instead of failing inside a
  `mutate()`.

* `data_processor()` no longer prefixes `Dissolved` onto an analyte the
  laboratory had already named that way, which produced
  `Dissolved Dissolved Total Phosphorus` and matched no guideline.

* `data_processor()` and `action_level_processor()` read the whole of each
  column before deciding its type. A real export previously produced hundreds
  of `readxl` type warnings.

* `timeseries_plot()` and `get_plotting_variables()` no longer reset the
  global random seed as a side effect of choosing colours.

## Other

* Added a `testthat` suite covering the read, join, summary, trend and
  plotting paths, replacing the standalone scripts in the package root.

* `R CMD check` passes with no errors, warnings or notes.

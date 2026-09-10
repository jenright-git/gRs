# gRs (development version)

## New features

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

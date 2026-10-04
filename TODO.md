# h gRs to-do

Ideas for future work, kept for the record. Most came from a review of the reporting functions on 2026-10-03, looking for what an environmental scientist assessing contamination would want beyond the monitoring summaries already in the package. Nothing here is committed to; tick items off or delete them as they are done or dropped.

## Fixes to existing functions

- [ ] **`summary_stats()` grouping** (in progress). Groups by `location_code` and `chem_name` only, where `analyte_summary()`, `historical_range()` and `min_max_locations()` also group by `output_unit` and `criteria_set`. After `criteria_long()` with two sets, LOC_01 ammonia in `gRs_data` (11 results) reported `n_samples = 22`, pooled the exceedances of both sets, and took the lower set's value as "the" criterion with a warning blaming units.
- [ ] **`summary_stats()` non-detect handling.** Mean, SD and percentiles treat non-detects at the full LOR with no option. Consider an `lor_multiplier` argument, or Kaplan-Meier statistics, and say in the output which was used.
- [ ] **`mann_kendall_test()` detect definition.** `nd_threshold` and `min_detects` decide detects from `prefix == "<"`; everything else uses `detect_flag`.
- [ ] **`mann_kendall_test()` minimum sample count.** Hard-coded at 4. With 4 results the smallest achievable two-sided p is about 0.09, so those series can never be "Increasing" or "Decreasing". Make it an argument, and/or report `n` beside every trend.
- [ ] **`mann_kendall_test()` pools units** (in progress). Now nests by `output_unit` too, so each unit is its own trend; `mka_to_excel()` and the heatmaps name the analyte with its unit where it has more than one.
- [ ] **Unit conversion step, e.g. `harmonise_units()`** (low priority). ESdat and EQuIS convert units before export and API pulls, so this only matters for results pulled straight from EQuIS `DT_RESULT`. Splitting by unit is correct but a lab switch from mg/L to ug/L cuts one series into two short ones. Convert each analyte to one unit (its most common, or one named per analyte) before analysis, using the `unit_conversion_factor()` table `join_action_levels()` already uses. Leave units in different dimensions (mg/kg solid vs ug/L leachate) apart, since those are different measurements.
- [ ] **`timeseries_plot()` mixed units.** Plots every result on one axis whatever its `output_unit`, and takes the axis label from `y_unit` rather than the data. Facet or colour by unit, or warn, where an analyte has more than one.
- [ ] **Trends caused by LOR changes.** A falling LOR over the record, with non-detects substituted, gives a "Decreasing" trend that comes from the lab rather than the site. Flag series where the LOR changed.

## QA/QC

The exports carry `Field_D`, `Interlab_D`, `Rinsate`, `Trip_B`, `Field_B`, `Trip_S` and `TSC` samples, and EQuIS carries `parent_sample_code`. `select_max_concentration()` currently just collapses them away.

- [ ] **`duplicate_rpd()`** (in progress). Primary/duplicate pairs with RPD and pass/fail against LOR-banded limits (e.g. no limit below 10x LOR, 50% at 10-20x, 30% above 20x), configurable.
- [ ] **`blank_summary()`** (in progress). Detections in rinsate, trip and field blanks, the samples they may affect, and trip spike recoveries.
- [ ] **`qa_frequency()`.** Duplicate and blank counts against project targets (e.g. 1 in 10, 1 in 20) per round and matrix.
- [ ] **`ion_balance()`.** Cation/anion balance in meq/L and percent difference, flagged beyond +/-5-10%. `gRs_data` has a Major Ions group.
- [ ] **`fraction_check()`.** Dissolved results above their total result by more than a tolerance (filtration or sampling problem), with the dissolved/total ratio.

## Assessment and statistics

- [ ] **`derive_analytes()`.** Calculated rows in the `data_processor()` schema so they flow into `join_action_levels()`: PFOS + PFHxS, total PFAS, total xylenes and BTEX, TRH F1 (C6-C10 minus BTEX), F2 (\>C10-C16 minus naphthalene), carcinogenic PAHs as B(a)P TEQ. Explicit non-detect rule (zero / half / full LOR) and a count of detected components.
- [ ] **`hardness_adjust()`.** ANZG hardness-modified freshwater guidelines for Cd, Cr(III), Cu, Pb, Ni and Zn, per sample.
- [ ] **`screen_copc()`.** Contaminant of potential concern screening: detection frequency, maximum against the criterion, maximum against background, giving retained or excluded with the reason.
- [ ] **`ucl_summary()`.** 95% UCL of the mean per analyte per area or depth range, Kaplan-Meier for censored data (e.g. EnvStats in Suggests), with the NSW EPA tests: UCL below the criterion, SD under 50% of it, no result above 250% of it.
- [ ] **`compare_background()`.** Upgradient against downgradient (or site against background): rank-sum test (Gehan for non-detects), a background threshold value, and a boxplot with the guideline line.
- [ ] **`delineation_check()`.** Soil boreholes whose deepest sample still exceeds (not vertically delineated), and exceedances with no clean location within a set distance (not laterally delineated), from `x_coord`, `y_coord` and the depth columns.

## Trends

`trend` is already imported, so these need no new dependencies.

- [ ] **Sen's slope** (`trend::sens.slope()`) in units per year, added to `mann_kendall_test()`.
- [ ] **Projected date to cross the guideline**, from the Sen's slope.
- [ ] **Seasonal Kendall** (`trend::smk.test()`) for quarterly programs.

## Groundwater

`data_processor()` reads gauging data, but nothing reports on it yet.

- [ ] **`water_level_summary()`.** Current against historical range per well, in the style of `historical_range()`.
- [ ] **`hydrograph_plot()`.** Groundwater elevation over time, optionally with a concentration on a second axis.
- [ ] **`flow_direction()`.** Hydraulic gradient magnitude and direction per round, from a plane fitted through three or more wells' coordinates and water levels.
- [ ] **`lnapl_summary()`.** Apparent LNAPL thickness by well and round.

## Presentation and report production

- [x] **`results_table()`** (done 2026-10-04, with `results_table_to_excel()`). The standard report table. Built with analytes across, grouped by `chem_group`, and samples down, with each guideline set as a row across the top. Cells are shaded in the colour of the highest guideline exceeded, and `highlight_lor` marks an LOR above the criteria.
- [ ] **`exceedance_map()`.** Locations at their coordinates coloured by exceedance ratio class (\<0.5x, 0.5-1x, 1-10x, \>10x), faceted by analyte or round, with optional GeoPackage export for GIS.
- [ ] **`depth_profile_plot()`.** Concentration against depth per borehole, with the criterion line.
- [ ] **`boxplot_with_criteria()`.** Boxplot by location or group with the guideline as a dashed line. Carried over from the old `planned_functions.txt`; may be covered by `compare_background()`.
- [ ] **`describe_exceedances()`.** Sentences for inline Quarto text, built on the `"locations"` attribute `exceedance_summary()` attaches, e.g. "PFOS exceeded the NEMP 99% guideline at 3 of 12 locations (MW01, MW06, MW07); maximum 0.45 ug/L at MW06, 4.5x the guideline."
- [ ] **`sampling_completeness()`.** Location x round grid of what was missed (dry, not sampled, analyte missing), plus holding-time checks where analysis dates exist (EQuIS Analytical Results II has them).
- [ ] **`report_workbook()`.** Every summary table in one formatted workbook, reusing the `mka_to_excel()` styling helpers.
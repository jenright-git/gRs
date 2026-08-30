
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gRs

<!-- badges: start -->

<!-- badges: end -->

gRs processes and analyses surface water, groundwater and soil/sediment
data exported from esdat and EQuIS. It reads an export, normalises it to
a shared set of column names, and takes it through duplicate handling,
action level comparison, summary statistics, trend testing and plotting
without you reshaping it in between.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("jenright-git/gRs")
```

## The workflow

Each function takes the table the previous one returns, so a project is
one pipeline:

``` r
library(gRs)

chem <- data_processor("davLChem1_Chemistry.xlsx") |>  # read and normalise
  select_max_concentration() |>                        # collapse duplicates
  half_lor(lor_multiplier = 0.5)                       # substitute non-detects

anzg <- action_level_processor("ANZG 95 Marine.xlsx", name = "ANZG 95% Marine")
compared <- join_action_levels(chem, anzg)              # add guidelines

summary_stats(compared, include_criteria = TRUE)        # tabulate
mann_kendall_test(chem) |> mann_kendall_heatmap()       # test for trends
plot_by_analyte(compared, save_path = "figures")        # plot and save
```

Where a function needs telling which column holds what, the argument is
named for the column: `location_col`, `chem_name_col`, `chem_group_col`,
`date_col`, `concentration_col`, `prefix_col`, `output_unit_col`,
`monitoring_zone_col`, `site_id_col`, `sample_type_col`. Each takes the
name with or without quotes, and the defaults are the names
`data_processor()` produces — so you only pass them for a table from
somewhere else.

## Reading an export

`data_processor()` reads chemistry exports (EQuIS Analytical Results II,
esdat `LChem1_Chemistry` for liquids and `SChem1_Chemistry` for soils,
and esdat `Chemistry List`) and gauging reports (EQuIS `Water Levels II`
and esdat gauging reports), normalising each to a shared set of column
names. The format is detected from the worksheet contents, so no extra
arguments are needed.

Soil exports report each analyte twice - once as a solid-phase
concentration (mg/kg) and once as a leachate concentration (mg/L or
ug/L). Only the solid-phase results are returned by default; pass
`result_type = "LEACHED_REG"` for the leachate results, or
`result_type = "all"` for both.

## Action levels

`action_level_processor()` reads an esdat action level export - a
guideline set such as the ANZG marine water toxicant DGVs - and
`join_action_levels()` puts a guideline against every result so the two
can be compared. The export carries no column naming the guideline set,
so the name is supplied by `name`:

``` r
chem <- data_processor("davLChem1_Chemistry.xlsx")
anzg <- action_level_processor("ANZG Marine Water Toxicant DGVs LOSP 95.xlsx",
                               name = "ANZG 95% Marine")

compared <- join_action_levels(chem, anzg)
#> Matched 1335 of 10086 results to ANZG 95% Marine (chem_code: 1335).
```

Analytes are matched on `chem_code` - the CAS number, which identifies
the analyte and is never trimmed, stripped or otherwise reduced to force
a join. A suffixed code names a different analyte from the bare code, so
`91-20-3` and `91-20-3_VOC` keep their separate guidelines and neither
stands in for the other. Where a result carries no code, `chem_name` is
tried instead, which is what matches an EQuIS export reporting names
against codes the guideline set does not share. Anything that finds no
guideline is returned as `attr(compared, "unmatched")` rather than being
matched approximately.

A guideline is only put against a result it applies to. The matrix must
agree, a filtered (dissolved) result only takes a guideline flagged
`filtered` and a total result one flagged `total`, a leachate result
only one flagged `leached`, and the guideline is converted into the unit
the result was reported in - so a ug/L guideline lands as mg/L against a
result reported in mg/L, and a mg/kg guideline is never compared against
a water result at all.

A guideline carrying no unit is never joined and never assumed to share
the result’s unit - a missing unit is a fault in the guideline file, and
`action_level_processor()` says so when it reads one. Fix it there.

Only a detected result exceeds a guideline. A non-detect whose LOR sits
above the guideline is recorded separately in `lor_above_criteria`; pass
`lor_as_exceedance = TRUE` to count those as exceedances too. That
choice is made once, at the join, and everything reading `exceedance`
afterwards follows it:

``` r
compared <- join_action_levels(chem, anzg, lor_as_exceedance = TRUE)
```

The added `criteria` column is the one the rest of the package already
reads:

``` r
summary_stats(compared, include_criteria = TRUE)
timeseries_plot(compared, criteria_col = criteria)
```

To compare against more than one guideline set, join them one at a time
into their own columns with `value_col`, then stack them into a single
`criteria` column with `criteria_long()` when it is time to analyse
across sets:

``` r
compared <- chem |>
  join_action_levels(anzg_95) |>
  join_action_levels(anzg_99, value_col = "criteria_99")

criteria_long(compared) |>
  dplyr::group_by(criteria_set, chem_name) |>
  dplyr::summarise(exceedances = sum(exceedance, na.rm = TRUE))
```

## Trend analysis

`mann_kendall_test()` runs a Mann-Kendall test on every location/analyte
combination at once. `gRs_data` is a worked example bundled with the
package, in the shape `data_processor()` returns:

``` r
library(gRs)
library(dplyr)
library(tidyr)

trends <- mann_kendall_test(gRs_data)

trends |>
  select(location_code, chem_name, p_value, tau_statistic, trend) |>
  head(5)
#> # A tibble: 5 × 5
#>   location_code chem_name  p_value tau_statistic trend               
#>   <chr>         <chr>        <dbl>         <dbl> <chr>               
#> 1 LOC_01        Phosphorus  0.813        -0.0748 Stable              
#> 2 LOC_02        Phosphorus  0.482         0.183  No Significant Trend
#> 3 LOC_03        Phosphorus  0.0725        0.440  Probably Increasing 
#> 4 LOC_04        Phosphorus  0.0785        0.443  Probably Increasing 
#> 5 LOC_05        Phosphorus  0.522        -0.173  Stable
```

Combinations dominated by non-detects can be dropped before testing,
with either `nd_threshold` (a maximum proportion of non-detects) or
`min_detects` (a minimum count of detected results). Both report what
they excluded.

Visualise the trends with a heatmap:

``` r
mann_kendall_heatmap(trends, width = 18)
```

<img src="man/figures/README-trend-heatmap-1.png" alt="" width="100%" />

`mann_kendall_heatmap_bw()` draws the same grid without fill, for
printing. It takes the three categories of
`mann_kendall_test(traditional = TRUE)`.

Plot the increasing trends. `timeseries_plot()` facets by analyte and
colours by location by default; `facet_by = "location"` swaps the two
around:

``` r
trends |>
  filter(trend == "Increasing") |>
  unnest(data) |>
  timeseries_plot(date_break = "2 month", date_label = "%b")
```

<img src="man/figures/README-increasing-trends-1.png" alt="" width="100%" />

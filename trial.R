# trial script to see if functionality is what I expect
load_all()
library(tidyverse)
library(esdatr)

df <- get_esdat_odata_chemistry(
  site_id = "Origin Eraring",
  project_id = "60653655 - Contam",
  location_types = "MW",
  chem_groups = c("Metals", "Nutrients"),
  sample_types = "Normal"
) |>
  data_processor()


al <- get_esdat_odata_action_levels(
  sources = "ANZG (2018) Marine Water Toxicant DGVs LOSP 99% (July 2026)"
) |>
  action_level_processor()

al2 <- get_esdat_odata_action_levels(
  sources = "ANZG (2018) Marine Water Toxicant DGVs LOSP 95% (July 2026)"
) |>
  action_level_processor()

df_al <- df |>
  join_action_levels(
    al,
    lor_as_exceedance = FALSE,
    value_col = "criteria_99"
  ) |>
  join_action_levels(
    al2,
    lor_as_exceedance = FALSE,
    value_col = "criteria_95"
  )


df_al |>
  mann_kendall_test() |>
  mka_to_excel(
    include_stats = TRUE,
    include_summary = TRUE,
    mark_lor_changes = TRUE
  )


df_al |>
  results_table(
    include_zone = FALSE,
    id_cols = c('location_code', 'date', 'sample_type', 'field_id'),
    criteria_col = c(criteria_99, criteria_95),
    criteria_colours = c(
      criteria_99 = "#e98f09",
      criteria_95 = "#09b5e9"
    ),
    group_by = 'monitoring_zone',
  )

df_al |>
  filter(monitoring_round == "2026-06") |>
  results_table_to_excel(
    include_zone = FALSE,
    id_cols = c(
      'location_code',
      'date',
      'sample_type',
      'field_id',
      'sample_code'
    ),
    criteria_col = c(criteria_99, criteria_95),
    criteria_colours = c(
      criteria_99 = "#e98f09",
      criteria_95 = "#09b5e9"
    ),
    merge_cells = TRUE,
    highlight_lor = FALSE,
    statistics = TRUE
  )


df_al |>
  results_table_to_excel(
    criteria_col = c(criteria_99, criteria_95),
    criteria_labels = c(criteria_99 = "ANZG 99%", criteria_95 = "ANZG 95%"),
    analytes = "exceeding",
    statistics = TRUE,
    #group_by = "monitoring_round",
    statistics_by_group = TRUE,
    title = "Table 3: Surface Water Analytical Results",
    layout = "samples_down"
  )
df_al |>
  results_table_to_excel(
    layout = "analytes_down",
    overwrite = TRUE,
    # criteria_col = c(criteria_99, criteria_95),
    # criteria_labels = c(criteria_99 = "ANZG 99%", criteria_95 = "ANZG 95%")
  )

df_al |>
  filter(monitoring_round == '2026-06') |>
  results_table(
    layout = "analytes_down",
    analytes = "detected"
    #  criteria_col = c(criteria_99, criteria_95),
    # criteria_labels = c(criteria_99 = "ANZG 99%", criteria_95 = "ANZG 95%"),
  )

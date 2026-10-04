# trial script to see if functionality is what I expect

library(tidyverse)
load_all()

df <- data_processor("example_reports/davLChem1_Chemistry (29).xlsx") |>
  filter(
    location_type == "SW",
    sample_type == "Normal",
    chem_code %in% c("1763-23-1", "335-67-1")
  )


al <- gRs::action_level_processor(
  "example_reports/HEPA (2025) PFAS NEMP Version 3.1 Interim marine 99_.xlsx",
  name = "NEMP"
)


sw_data <- join_action_levels(df, al)

sw_data |>
  summary_stats(include_criteria = TRUE) |>
  arrange(desc(exceedance_count))

sw_data |>
  group_by(location_code, chem_name) |>
  summarise(
    sum = sum(exceedance)
  ) |>
  arrange(desc(sum))


al_99 <- action_level_processor(
  "example_reports/HEPA (2025) PFAS NEMP Version 3.1 Interim marine 99_.xlsx",
  name = "NEMP 99%"
)
al_95 <- action_level_processor(
  "example_reports/HEPA (2025) PFAS NEMP Version 3.1 Interim marine 95_.xlsx",
  name = "NEMP 95%"
)

both <- df |>
  join_action_levels(al_99, value_col = "criteria_99") |>
  join_action_levels(al_95, value_col = "criteria_95")

stats_99 <- summary_stats(
  both,
  include_criteria = TRUE,
  criteria_col = criteria_99
)
stats_95 <- summary_stats(
  both,
  include_criteria = TRUE,
  criteria_col = criteria_95
)

stats_99 |>
  dplyr::left_join(
    dplyr::select(
      stats_95,
      location_code,
      chem_name,
      criteria_95,
      criteria_95_exceedance_count
    ),
    by = c("location_code", "chem_name")
  ) |>
  arrange(desc(criteria_99_exceedance_count))


sw_data |>
  #mutate(monitoring_round = "2026-06") |>
  analyte_summary(round = "2026-06", criteria_col = criteria) |>
  create_gt()

both |>
  criteria_long() |>
  analyte_summary() |>
  create_gt()


sw_data |>
  historical_range(
    round = "2026-06",
    include_criteria = FALSE,
    spike_factor = 10
  ) |>
  create_gt(merge_range = FALSE, highlight = TRUE)


sw_data |>
  summary_stats(include_criteria = TRUE) |>
  create_gt()


ar <- data_processor(
  "example_reports/19784298_ALII-CN_Summerhill-Discharge-EIA-PBI_20260830071244.xlsx"
)

ar |>
  analyte_summary(criteria_col = action_level) |>
  create_gt()


ar |>
  min_max_locations(n_max = 2, n_min = 2, group_vars = "task_code") |>
  select(
    location_code,
    extreme,
    rank,
    date,
    chem_name,
    concentration,
    detect_flag
  ) |>
  group_by(chem_name) |>
  create_gt()


both |>
  criteria_long() |>
  summary_stats(include_criteria = TRUE) |>
  create_gt()


both |>
  mann_kendall_test() |>
  mka_to_excel(
    include_zone = TRUE,
    include_stats = TRUE,
    include_summary = TRUE,
    criteria = c(criteria_95, criteria_99)
  )


both |>
  summary_stats(
    include_criteria = TRUE,
    criteria_col = c(criteria_95, criteria_99),
    lor_multiplier = 0
  ) |>
  create_gt()

# Takes summary_stats()'s output, as mka_to_excel() takes mann_kendall_test()'s
both |>
  summary_stats(
    include_criteria = TRUE,
    criteria_col = c(criteria_95, criteria_99),
    group_vars = "monitoring_zone"
  ) |>
  summary_stats_to_excel(include_zone = TRUE)


both <- df |>
  join_action_levels(al_99, value_col = "criteria_99") |>
  join_action_levels(al_95, value_col = "criteria_95")

results_table_to_excel(both, criteria_col = c(criteria_99, criteria_95))
results_table(
  both,
  criteria_col = c(criteria_99, criteria_95),
  highlight_lor = FALSE,
  include_zone = TRUE
)
results_table_to_excel(
  both,
  id_cols = c("date", "Field ID" = "field_id"),
  criteria_col = c(criteria_99, criteria_95),
  highlight_lor = FALSE
)


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
    group_by = 'monitoring_zone',
    merge_cells = TRUE
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
  filter(monitoring_round == '2026-06', location_code == "10_South") |>
  results_table(
    layout = "analytes_down",
    analytes = "detected"
    #  criteria_col = c(criteria_99, criteria_95),
    # criteria_labels = c(criteria_99 = "ANZG 99%", criteria_95 = "ANZG 95%"),
  )

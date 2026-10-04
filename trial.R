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
  min_max_locations(n_max = 2, n_min = 2) |>
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

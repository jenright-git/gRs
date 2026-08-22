# Action level joins - a worked example
#
# Runs a guideline set from an esdat action level export onto a chemistry
# table, compares every result against it, and stacks a second set alongside
# the first. Every file used sits in example_reports/, so this runs as-is
# from the package root.

library(gRs)
library(dplyr)

al_file <- "example_reports/ANZG Marine Water Toxicant DGVs LOSP 95_ (March 2026).xlsx"
liquid_file <- "example_reports/davLChem1_Chemistry (28).xlsx"

# ---------------------------------------------------------------------------
# 1. Read the guideline set
#
# The export carries no column naming the set itself, so `name` supplies it -
# it ends up in criteria_name and labels the set wherever it is used later.
# The value and its unit share one cell ("0.006 µg Sn/L"), and are split into
# criteria / criteria_unit, with any basis of measurement ("as Sn", "as N")
# pulled out into criteria_basis so the unit is left convertible.

anzg_95 <- action_level_processor(al_file, name = "ANZG 95% Marine")

anzg_95 %>%
  select(
    criteria_name,
    chem_code,
    chem_name,
    criteria,
    criteria_unit,
    criteria_basis,
    total,
    filtered,
    leached
  ) %>%
  print(n = 10)

# ---------------------------------------------------------------------------
# 2. Read the chemistry it is being compared against

liquid <- data_processor(liquid_file)

# ---------------------------------------------------------------------------
# 3. Join
#
# Analytes match on chem_code (the CAS number), falling back to chem_name
# where a result carries no code. A guideline is only applied to a result it
# actually covers - same matrix, same fraction (filtered / total), same
# leachate status - and is converted into the unit that result is reported
# in, so µg/L guidelines land against mg/L results correctly.
#
# The report printed here says what matched on what, and what did not match.

compared <- join_action_levels(liquid, anzg_95)

compared %>%
  filter(!is.na(criteria)) %>%
  select(
    location_code,
    date,
    chem_name,
    concentration,
    output_unit,
    criteria,
    criteria_unit,
    exceedance_ratio,
    exceedance
  ) %>%
  print(n = 10)

# The analytes that found no guideline come back as an attribute rather than
# being trimmed until they match something.
cat("\nunmatched analytes:\n")
print(attr(compared, "unmatched"), n = 10)

# ---------------------------------------------------------------------------
# 4. What exceeded
#
# Only a detected result exceeds. A non-detect whose LOR sits above the
# guideline is held apart in lor_above_criteria - it says the limit was too
# high to demonstrate compliance, not that the analyte was there. Pass
# lor_as_exceedance = TRUE to join_action_levels() to count those as
# exceedances instead.

compared %>%
  filter(!is.na(criteria)) %>%
  group_by(chem_name, criteria, criteria_unit) %>%
  summarise(
    results = n(),
    exceedances = sum(exceedance, na.rm = TRUE),
    lor_above = sum(lor_above_criteria, na.rm = TRUE),
    max_ratio = max(exceedance_ratio, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(exceedances > 0 | lor_above > 0) %>%
  arrange(desc(max_ratio)) %>%
  print(n = 20)

# ---------------------------------------------------------------------------
# 5. Straight into the rest of the package
#
# `criteria` is the column summary_stats() reads and timeseries_plot() draws.

stats <- summary_stats(compared, include_criteria = TRUE)
print(head(stats))

p <- compared %>%
  filter(chem_name == "Toluene") %>%
  timeseries_plot(criteria_col = criteria, y_unit = "µg/L")
print(p)

# ---------------------------------------------------------------------------
# 6. A second guideline set alongside the first
#
# Each set goes in its own column via `value_col`, so the two sit side by side
# without overwriting each other. There is only one guideline export in
# example_reports/, so a notional stricter set is derived from it here purely
# to show the shape - in practice this would be a second file read with
# action_level_processor().

anzg_99 <- anzg_95 %>%
  mutate(criteria_name = "ANZG 99% Marine", criteria = criteria / 5)

both <- compared %>%
  join_action_levels(anzg_99, value_col = "criteria_99", quiet = TRUE)

both %>%
  filter(!is.na(criteria)) %>%
  select(
    chem_name,
    concentration,
    output_unit,
    criteria,
    exceedance,
    criteria_99,
    criteria_99_exceedance
  ) %>%
  print(n = 10)

# ---------------------------------------------------------------------------
# 7. Stack the sets for analysis
#
# The wide form above reads well in a table but is the wrong shape to analyse
# across sets. criteria_long() gives every result one row per guideline set,
# with a single criteria / exceedance column, so one group_by covers the lot.

long <- criteria_long(both)

long %>%
  filter(!is.na(criteria)) %>%
  group_by(criteria_set, criteria_name) %>%
  summarise(
    comparisons = n(),
    exceedances = sum(exceedance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

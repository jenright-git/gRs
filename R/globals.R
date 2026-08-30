# Column names referenced inside tidy-eval expressions, declared here so
# R CMD check does not read them as undefined globals. Grouped by where they
# come from.
utils::globalVariables(c(
  # chemistry export
  "chem_group",
  "chem_name",
  "concentration",
  "date",
  "detect_flag",
  "fraction",
  "location_code",
  "monitoring_zone",
  "output_unit",
  "prefix",
  "result_unit",
  "sample_type",
  "sampled_date_time",
  "site",
  "site_id",

  # water level (gauging) export
  "depth_unit",
  "dry_indicator_yn",
  "exact_elev",
  "reference_elev",
  "water_depth",
  "water_level",
  "water_level_depth",

  # action level (guideline) export
  "chem_code",
  "criteria",
  "criteria_basis",
  "criteria_name",
  "criteria_unit",
  "exceedance",
  "exceedance_ratio",
  "lor_above_criteria",
  "matrix_code",
  "result_type",

  # computed inside historical_range()
  "max_ratio",

  # computed inside summary_stats(), mann_kendall_test() and
  # select_max_concentration()
  ".has_duplicate",
  ".is_detect",
  ".label",
  "COV",
  "SD",
  "n_detects",
  "n_non_detects",
  "n_samples",
  "nd_pct",
  "p_value",
  "results",
  "tau_statistic",
  "trend"
))

# The real esdat/EQuIS exports live in example_reports/, which is not shipped
# inside the package. Tests that read them run from the source tree and skip
# elsewhere; everything else is built from the fixtures below so the bulk of
# the suite runs anywhere.

example_report <- function(file) {
  path <- testthat::test_path("..", "..", "example_reports", file)
  testthat::skip_if_not(file.exists(path), paste0("no example_reports/", file))
  path
}

# A minimal chemistry table with the shape data_processor() returns.
chem_fixture <- function(...) {
  out <- dplyr::tibble(
    date = as.POSIXct("2024-01-15", tz = "UTC") + (0:5) * 86400 * 30,
    sampled_date_time = date,
    site_id = "SITE",
    location_code = rep(c("MW01", "MW02"), each = 3),
    monitoring_zone = "Zone A",
    chem_group = "Metals",
    chem_name = "Copper",
    chem_code = "7440-50-8",
    fraction = "T",
    prefix = c(NA, NA, "<", NA, NA, "<"),
    detect_flag = c("Y", "Y", "N", "Y", "Y", "N"),
    concentration = c(1.5, 2.5, 0.5, 4, 8, 0.5),
    output_unit = "mg/L",
    sample_type = "Normal",
    matrix_code = "WATER"
  )
  args <- list(...)
  for (nm in names(args)) out[[nm]] <- args[[nm]]
  out
}

# A minimal action level table with the shape action_level_processor() returns.
action_level_fixture <- function(...) {
  out <- dplyr::tibble(
    criteria_name = "Test Guideline",
    chem_code = "7440-50-8",
    chem_name = "Copper",
    matrix_code = "WATER",
    criteria = 1000,
    criteria_unit = "ug/L",
    criteria_basis = NA_character_,
    criteria_text = "1000 ug/L",
    leached = FALSE,
    total = TRUE,
    filtered = TRUE,
    conditions = NA_character_,
    comments = NA_character_
  )
  args <- list(...)
  for (nm in names(args)) out[[nm]] <- args[[nm]]
  out
}

test_that("the chemistry exports are all detected by content", {
  files <- c(
    "Analytical Results II.xlsx",
    "ChemistryList.xlsx",
    "davLChem1_Chemistry (28).xlsx",
    "davSChem1_Chemistry.xlsx"
  )
  for (file in files) {
    out <- suppressMessages(data_processor(example_report(file)))
    expect_equal(attr(out, "report_type"), "chemistry", info = file)
    expect_true(all(REQUIRED_COLUMNS %in% names(out)), info = file)
    expect_gt(nrow(out), 0)
  }
})

test_that("the gauging reports are detected and share a schema", {
  esdat <- suppressMessages(data_processor(
    example_report("GW4_URS_Gauging_Report_esdat.xlsx")
  ))
  equis <- suppressMessages(data_processor(
    example_report("Water Levels II_equis.xlsx")
  ))

  expect_equal(attr(esdat, "report_type"), "water_level")
  expect_equal(attr(equis, "report_type"), "water_level")
  expect_true(all(names(WATER_LEVEL_SCHEMA) %in% names(esdat)))
  expect_equal(
    nrow(dplyr::bind_rows(esdat, equis)),
    nrow(esdat) + nrow(equis)
  )
})

test_that("a soil export keeps the solid-phase results by default", {
  soil <- suppressMessages(data_processor(
    example_report("davSChem1_Chemistry.xlsx")
  ))

  expect_equal(unique(soil$result_type), "REG")
  expect_true(all(is.na(soil$fraction)))
  expect_false(any(grepl("^Dissolved", soil$chem_name)))
})

test_that("result_type selects the leachate results instead", {
  path <- example_report("davSChem1_Chemistry.xlsx")
  leachate <- suppressMessages(data_processor(path, result_type = "LEACHED_REG"))
  both <- suppressMessages(data_processor(path, result_type = "all"))
  solid <- suppressMessages(data_processor(path))

  expect_equal(unique(leachate$result_type), "LEACHED_REG")
  expect_equal(nrow(both), nrow(solid) + nrow(leachate))
})

test_that("asking for a result type that is absent errors", {
  path <- example_report("davSChem1_Chemistry.xlsx")

  expect_error(
    suppressMessages(data_processor(path, result_type = "NOT_A_TYPE")),
    "No rows left after filtering"
  )
})

test_that("an action level export is not read as chemistry", {
  path <- example_report(
    "ANZG Marine Water Toxicant DGVs LOSP 95_ (March 2026).xlsx"
  )

  expect_warning(
    out <- data_processor(path),
    "action level export"
  )
  expect_null(out)
})

test_that("a missing file is named in the error", {
  expect_error(data_processor("no-such-file.xlsx"), "File does not exist")
})

# --- normalisation, tested without touching a file -------------------------

test_that("aliases resolve to canonical names", {
  raw <- dplyr::tibble(sys_loc_code = "MW01", cas_rn = "7440-50-8")
  out <- resolve_columns(raw, COLUMN_ALIASES)

  expect_equal(names(out), c("location_code", "chem_code"))
})

test_that("an already-Dissolved analyte is not prefixed twice", {
  raw <- dplyr::tibble(
    sample_date = as.POSIXct("2024-01-15", tz = "UTC"),
    site = "SITE",
    sys_loc_code = "MW01",
    chemical_name = c("Total Phosphorus", "Dissolved Total Phosphorus"),
    total_or_filtered = "F",
    result = 1,
    result_unit = "mg/L",
    qualifier = NA_character_
  )
  out <- process_chemistry(raw)

  expect_equal(
    out$chem_name,
    c("Dissolved Total Phosphorus", "Dissolved Total Phosphorus")
  )
})

test_that("detect_flag is derived from prefix and back again", {
  raw <- dplyr::tibble(
    sample_date = as.POSIXct("2024-01-15", tz = "UTC"),
    site = "SITE",
    sys_loc_code = "MW01",
    chemical_name = "Copper",
    result = c(1, 0.5),
    result_unit = "mg/L",
    qualifier = c(NA, "<")
  )
  out <- process_chemistry(raw)

  expect_equal(out$detect_flag, c("Y", "N"))
})

test_that("coercion tolerates the shapes the exports arrive in", {
  expect_equal(coerce_numeric(c("1.5", " 2 ", "x")), c(1.5, 2, NA))
  expect_equal(coerce_numeric(c(NA, NA)), c(NA_real_, NA_real_))
  expect_equal(normalise_yn(c("Dry", "", "yes", NA)), c("Y", "N", "Y", "N"))
})

test_that("a guideline is converted into the result's own unit", {
  out <- join_action_levels(
    chem_fixture(),
    action_level_fixture(criteria = 1000, criteria_unit = "ug/L"),
    quiet = TRUE
  )

  expect_equal(unique(out$criteria), 1)
  expect_equal(unique(out$criteria_unit), "mg/L")
})

test_that("only a detected result exceeds a guideline", {
  out <- join_action_levels(
    chem_fixture(),
    action_level_fixture(criteria = 0.4, criteria_unit = "mg/L"),
    quiet = TRUE
  )

  # The two non-detects are reported at 0.5, above the guideline of 0.4.
  expect_equal(out$exceedance, c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE))
  expect_equal(out$lor_above_criteria, c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE))
})

test_that("lor_as_exceedance folds high LORs into the exceedance count", {
  out <- join_action_levels(
    chem_fixture(),
    action_level_fixture(criteria = 0.4, criteria_unit = "mg/L"),
    lor_as_exceedance = TRUE,
    quiet = TRUE
  )

  expect_equal(sum(out$exceedance), 6)
  expect_equal(sum(out$lor_above_criteria), 2)
})

test_that("a solid guideline is not compared against a water result", {
  out <- join_action_levels(
    chem_fixture(),
    action_level_fixture(criteria_unit = "mg/kg", matrix_code = NA),
    match_matrix = FALSE,
    quiet = TRUE
  )

  expect_true(all(is.na(out$criteria)))
})

test_that("a filtered result takes only a guideline flagged filtered", {
  out <- join_action_levels(
    chem_fixture(fraction = "F"),
    action_level_fixture(filtered = FALSE, total = TRUE),
    quiet = TRUE
  )

  expect_true(all(is.na(out$criteria)))
})

test_that("the Dissolved prefix does not stop a name match", {
  levels <- action_level_fixture(chem_code = NA_character_)
  data <- chem_fixture(chem_code = NA_character_, chem_name = "Dissolved Copper")
  out <- join_action_levels(data, levels, quiet = TRUE)

  expect_false(any(is.na(out$criteria)))
})

test_that("a suffixed CAS number is a different analyte from the bare one", {
  out <- join_action_levels(
    chem_fixture(chem_code = "7440-50-8_VOC", chem_name = "Copper by VOC"),
    action_level_fixture(),
    match_name = FALSE,
    quiet = TRUE
  )

  expect_true(all(is.na(out$criteria)))
})

test_that("unmatched analytes come back as an attribute", {
  out <- join_action_levels(
    chem_fixture(chem_code = "1234-56-7", chem_name = "Unobtainium"),
    action_level_fixture(),
    quiet = TRUE
  )

  expect_equal(nrow(attr(out, "unmatched")), 1)
  expect_equal(attr(out, "unmatched")$n_results, 6)
})

test_that("a unitless guideline set is an error, not empty columns", {
  expect_snapshot(
    join_action_levels(
      chem_fixture(),
      action_level_fixture(criteria_unit = NA_character_)
    ),
    error = TRUE
  )
})

test_that("two guideline sets at once is an error naming both", {
  levels <- dplyr::bind_rows(
    action_level_fixture(criteria_name = "Set A"),
    action_level_fixture(criteria_name = "Set B")
  )

  expect_snapshot(join_action_levels(chem_fixture(), levels), error = TRUE)
})

test_that("row count and order are unchanged by the join", {
  data <- chem_fixture()
  out <- join_action_levels(data, action_level_fixture(), quiet = TRUE)

  expect_equal(nrow(out), nrow(data))
  expect_equal(out$concentration, data$concentration)
})

test_that("an empty chemistry table still gains the guideline columns", {
  out <- join_action_levels(
    chem_fixture()[0, ],
    action_level_fixture(),
    quiet = TRUE
  )

  expect_equal(nrow(out), 0)
  expect_true(all(c("criteria", "exceedance") %in% names(out)))
})

test_that("criteria_long stacks two sets into one criteria column", {
  compared <- chem_fixture() %>%
    join_action_levels(action_level_fixture(), quiet = TRUE) %>%
    join_action_levels(
      action_level_fixture(criteria_name = "Stricter", criteria = 100),
      value_col = "criteria_99",
      quiet = TRUE
    )

  long <- criteria_long(compared)

  expect_equal(nrow(long), nrow(compared) * 2)
  expect_equal(sort(unique(long$criteria_set)), c("criteria", "criteria_99"))
  expect_false(any(c("criteria_99_exceedance") %in% names(long)))
})

test_that("criteria_long names the sets present when asked for one that is not", {
  compared <- join_action_levels(
    chem_fixture(),
    action_level_fixture(),
    quiet = TRUE
  )

  expect_snapshot(criteria_long(compared, sets = "criteria_99"), error = TRUE)
})

test_that("unit conversion stays inside a dimension", {
  expect_equal(unit_conversion_factor("ug/L", "mg/L"), 1e-3)
  expect_equal(unit_conversion_factor("mg/kg", "ug/kg"), 1e3)
  expect_equal(unit_conversion_factor("mg/kg", "mg/L"), NA_real_)
  # ppm belongs to both, and takes whichever dimension the other unit is in.
  expect_equal(unit_conversion_factor("ppm", "ug/L"), 1e3)
  expect_equal(unit_conversion_factor("ppm", "ug/kg"), 1e3)
})

test_that("the value, unit and basis are split out of one cell", {
  parsed <- parse_action_level(c(
    "80 µg/L",
    "0.006 µg Sn/L",
    "1,000 ug/L",
    "<0.1 mg/L",
    "6.5 - 8.5",
    "5"
  ))

  expect_equal(parsed$value, c(80, 0.006, 1000, 0.1, NA, 5))
  expect_equal(parsed$basis[[2]], "Sn")
  expect_equal(parsed$unit[[3]], "ug/L")
  # A range has no single value to compare against, so it is left unread.
  expect_equal(parsed$unit[[5]], NA_character_)
})

test_that("matrix and fraction spellings fold together", {
  expect_equal(normalise_matrix(c("WATER", "Water", "SEDIMENT", "Soil")),
               c("WATER", "WATER", "SOIL", "SOIL"))
  expect_equal(normalise_fraction(c("T", "D", "F", "N", NA)),
               c("T", "F", "F", NA, NA))
})

test_that("action_level_processor reads the ANZG export", {
  path <- example_report(
    "ANZG Marine Water Toxicant DGVs LOSP 95_ (March 2026).xlsx"
  )
  levels <- suppressWarnings(
    action_level_processor(path, name = "ANZG 95% Marine")
  )

  expect_equal(nrow(levels), 63)
  expect_equal(unique(levels$criteria_name), "ANZG 95% Marine")
  expect_equal(attr(levels, "report_type"), "action_level")
  expect_type(levels$criteria, "double")
  expect_equal(
    levels$criteria_basis[levels$chem_name == "Tributyltin"],
    "Sn"
  )
})

test_that("the guideline set name defaults to the file name", {
  path <- example_report(
    "ANZG Marine Water Toxicant DGVs LOSP 95_ (March 2026).xlsx"
  )
  levels <- suppressWarnings(action_level_processor(path))

  expect_equal(
    unique(levels$criteria_name),
    "ANZG Marine Water Toxicant DGVs LOSP 95_ (March 2026)"
  )
})

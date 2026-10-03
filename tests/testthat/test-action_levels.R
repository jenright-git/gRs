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

test_that("a data frame of action levels reads like the file it came from", {
  path <- example_report(
    "ANZG Marine Water Toxicant DGVs LOSP 95_ (March 2026).xlsx"
  )
  from_file <- suppressWarnings(
    action_level_processor(path, name = "ANZG 95% Marine")
  )
  raw <- suppressMessages(readxl::read_excel(path))
  from_df <- suppressWarnings(
    action_level_processor(raw, name = "ANZG 95% Marine")
  )

  expect_equal(from_df, from_file)
})

test_that("a data frame of action levels needs a name", {
  raw <- dplyr::tibble(ChemCode = "7440-50-8", Action_Level = "1.3 ug/L")

  expect_error(action_level_processor(raw), "`name` is required")
  expect_equal(
    action_level_processor(raw, name = "Test")$criteria_unit,
    "ug/L"
  )
})

test_that("a data frame that is not action levels is an error", {
  expect_error(
    action_level_processor(dplyr::tibble(Location_Code = "MW01"), name = "x"),
    "not a table of action levels"
  )
})

# --- guideline tables read from ESdat and EQuIS ----------------------------

# ESdat_Environmental_Standards as esdatr::get_esdat_odata() returns it: the
# value a number, its unit, bound and flags in columns of their own, and the
# set named on every row.
esdat_standards <- function(...) {
  out <- dplyr::tibble(
    Chem_Code = c("7440-50-8", "688-73-3_as_Sn", "PH", "7440-66-6"),
    Chem_Name = c("Copper", "Tributyltin", "pH", "Zinc"),
    Matrix_Type = "Water",
    Action_Level_Source = "ANZG 95% Marine",
    Action_Level = c(1.3, 0.006, 8.5, NA),
    Action_Level_Min = c(NA, NA, 6.5, NA),
    Units = c("\u00b5g/L", "\u00b5g Sn/L", "pH units", "\u00b5g/L"),
    Leached = FALSE,
    Applies_To_Total_Result = TRUE,
    Applies_To_Filtered_Result = c(TRUE, FALSE, TRUE, TRUE),
    Action_Level_Prefix = c(NA, NA, NA, "NL"),
    Comments = NA_character_
  )
  args <- list(...)
  for (nm in names(args)) out[[nm]] <- args[[nm]]
  out
}

# EQuIS DT_ACTION_LEVEL_PARAMETER as AEQuIS::get_equis_odata_all() returns
# it: the value as text, and one fraction per guideline.
equis_action_levels <- function() {
  dplyr::tibble(
    PARAM_CODE = c("7440-50-8", "7440-50-8", "7440-66-6", "PH"),
    ACTION_LEVEL_CODE = "ANZG MW 95",
    ACTION_LEVEL = c("1.3", "5", "8", "8.5"),
    ACTION_LEVEL_MIN = c(NA, NA, NA, "6.5"),
    UNIT = c("ug/l", "ug/l", "ug/l", "pH units"),
    MATRIX = c("WATER", "WATER", NA, "WATER"),
    FRACTION = c("D", "T", "N", NA),
    REMARK = NA_character_
  )
}

test_that("an ESdat feed reads like the export it came from", {
  path <- example_report(
    "ANZG Marine Water Toxicant DGVs LOSP 95_ (March 2026).xlsx"
  )
  raw <- suppressMessages(readxl::read_excel(path))
  cell <- raw$`Action Level`
  feed <- dplyr::tibble(
    Chem_Code = raw$ChemCode,
    Chem_Name = raw$ChemName,
    Matrix_Type = raw$MatrixType,
    Action_Level_Source = "ANZG 95% Marine",
    Action_Level = as.numeric(sub(" .*$", "", cell)),
    Units = sub("^[^ ]+ ", "", cell),
    Leached = normalise_logical(raw$Leached),
    Applies_To_Total_Result = normalise_logical(raw$Total),
    Applies_To_Filtered_Result = normalise_logical(raw$Filtered),
    Comments = raw$Comments
  )

  from_file <- suppressWarnings(
    action_level_processor(path, name = "ANZG 95% Marine")
  )
  from_feed <- suppressWarnings(action_level_processor(feed))

  expect_equal(from_feed, from_file)
})

test_that("an ESdat feed's value, unit, basis and flags are read", {
  levels <- suppressMessages(action_level_processor(esdat_standards()))

  expect_equal(levels$chem_code, c("7440-50-8", "688-73-3_as_Sn"))
  expect_equal(levels$criteria, c(1.3, 0.006))
  expect_equal(levels$criteria_unit, c("\u00b5g/L", "\u00b5g/L"))
  expect_equal(levels$criteria_basis, c(NA, "Sn"))
  expect_equal(levels$criteria_text, c("1.3 \u00b5g/L", "0.006 \u00b5g Sn/L"))
  expect_equal(levels$total, c(TRUE, TRUE))
  expect_equal(levels$filtered, c(TRUE, FALSE))
  expect_equal(unique(levels$criteria_name), "ANZG 95% Marine")
})

test_that("a range is dropped rather than read as one of its bounds", {
  expect_message(
    levels <- action_level_processor(esdat_standards()),
    "Dropped 2 action level rows.*6.5 - 8.5 pH units, NL"
  )
  expect_false(any(levels$chem_name %in% c("pH", "Zinc")))
  expect_false("criteria_min" %in% names(levels))
})

test_that("a feed holding several sets keeps each set's own name", {
  two <- dplyr::bind_rows(
    esdat_standards(Action_Level_Source = "NEMP 95%"),
    esdat_standards(Action_Level_Source = "NEMP 99%")
  )
  levels <- suppressMessages(action_level_processor(two))

  expect_equal(sort(unique(levels$criteria_name)), c("NEMP 95%", "NEMP 99%"))
  expect_error(
    suppressMessages(action_level_processor(two, name = "NEMP")),
    "holds 2 guideline sets: NEMP 95%, NEMP 99%"
  )
})

test_that("`name` relabels a feed's one set", {
  levels <- suppressMessages(
    action_level_processor(esdat_standards(), name = "ANZG 95")
  )

  expect_equal(unique(levels$criteria_name), "ANZG 95")
})

test_that("an EQuIS action level table is read by its parameter code", {
  levels <- suppressMessages(action_level_processor(equis_action_levels()))

  expect_equal(levels$chem_code, c("7440-50-8", "7440-50-8", "7440-66-6"))
  expect_true(all(is.na(levels$chem_name)))
  expect_equal(unique(levels$criteria_name), "ANZG MW 95")
  expect_equal(levels$criteria, c(1.3, 5, 8))
  expect_equal(levels$criteria_unit, rep("ug/l", 3))
  expect_equal(levels$criteria_text, c("1.3 ug/l", "5 ug/l", "8 ug/l"))
  # One fraction per guideline: D is filtered alone, T total alone, N either.
  expect_equal(levels$filtered, c(TRUE, FALSE, NA))
  expect_equal(levels$total, c(FALSE, TRUE, NA))
})

test_that("an EQuIS guideline applies only to its own fraction", {
  levels <- suppressMessages(action_level_processor(equis_action_levels()))
  chem <- chem_fixture(
    fraction = rep(c("D", "T"), 3),
    output_unit = "ug/L",
    matrix_code = "WQ"
  )
  out <- join_action_levels(chem, levels, quiet = TRUE)

  expect_equal(out$criteria, rep(c(1.3, 5), 3))
})

test_that("EQuIS's QA/QC matrix codes fold into water and soil", {
  expect_equal(
    normalise_matrix(c("WQ", "WG", "SQ", "SO")),
    c("WATER", "WATER", "SOIL", "SOIL")
  )
})

test_that("a guideline value is written back as the number it is", {
  expect_equal(
    format_action_level(c(80, 0.006, 1900, 1e-4, 0.1 + 0.2, NA)),
    c("80", "0.006", "1900", "0.0001", "0.3", NA)
  )
})

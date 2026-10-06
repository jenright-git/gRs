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

# --- data frames: the ESdat OData feeds ------------------------------------

# The OData feeds are the views the Excel exports are made from: the same
# columns, spelt with underscores, and date-times sent as text.
as_odata_feed <- function(path) {
  raw <- suppressMessages(readxl::read_excel(path, skip = 1, guess_max = 1e6))
  names(raw) <- gsub(" ", "_", names(raw))
  dplyr::mutate(
    raw,
    dplyr::across(
      dplyr::where(~ inherits(.x, "POSIXct")),
      ~ format(.x, "%Y-%m-%dT%H:%M:%S")
    )
  )
}

test_that("an OData chemistry feed gives the same table as its Excel export", {
  for (file in c("davLChem1_Chemistry (28).xlsx", "davSChem1_Chemistry.xlsx")) {
    path <- example_report(file)
    from_file <- suppressMessages(data_processor(path, result_type = "all"))
    from_feed <- suppressMessages(
      data_processor(as_odata_feed(path), result_type = "all")
    )

    expect_equal(from_feed, from_file, info = file)
  }
})

test_that("a data frame is detected as chemistry or water level", {
  chem <- dplyr::tibble(
    Site = "SITE",
    Location_Code = "MW01",
    Sampled_Date_Time = "2024-03-01T14:30:00",
    Chem_Name = "Copper",
    Chem_Code = "7440-50-8",
    Chem_Group = "Metals",
    Total_or_Filtered = "T",
    Prefix = NA_character_,
    Result = 1.5,
    Result_Unit = "mg/L"
  )
  depths <- dplyr::tibble(
    Site = "SITE",
    Location_Code = "MW01",
    Date_Time = "2024-03-01T09:00:00",
    Water_Depth_bgl = 2.99,
    Dry = c(FALSE, TRUE)
  )

  chem_out <- data_processor(chem)
  depth_out <- data_processor(depths)

  expect_equal(attr(chem_out, "report_type"), "chemistry")
  expect_equal(
    chem_out$sampled_date_time,
    as.POSIXct("2024-03-01 14:30:00", tz = "UTC")
  )
  expect_equal(chem_out$detect_flag, "Y")

  expect_equal(attr(depth_out, "report_type"), "water_level")
  expect_equal(depth_out$water_depth, c(2.99, 2.99))
  expect_equal(depth_out$dry_indicator_yn, c("N", "Y"))
  expect_true(all(names(WATER_LEVEL_SCHEMA) %in% names(depth_out)))
})

test_that("EQuIS chemistry read with AEQuIS is read as chemistry", {
  # AEQuIS::get_equis_chemistry() joins DT_SAMPLE, DT_TEST and DT_RESULT,
  # which carry the facility's numeric ID rather than its code.
  equis <- dplyr::tibble(
    FACILITY_ID = 19784298,
    SYS_LOC_CODE = "MW01",
    SAMPLE_DATE = "2025-09-29T11:50:00Z",
    MATRIX_CODE = "WQ",
    SAMPLE_TYPE_CODE = "N",
    CAS_RN = "7440-50-8",
    CHEMICAL_NAME = "Copper",
    FRACTION = c("T", "D"),
    RESULT_NUMERIC = c(2, 1),
    RESULT_UNIT = "ug/l",
    DETECT_FLAG = c("Y", "N"),
    RESULT_TYPE_CODE = "TRG"
  )

  expect_warning(out <- data_processor(equis), "chem_group")

  expect_equal(attr(out, "report_type"), "chemistry")
  expect_equal(out$site_id, c("19784298", "19784298"))
  expect_equal(
    out$sampled_date_time,
    rep(as.POSIXct("2025-09-29 11:50:00", tz = "UTC"), 2)
  )
  # EQuIS files a dissolved result as "D", ESDAT as "F".
  expect_equal(out$chem_name, c("Copper", "Dissolved Copper"))
  # the prefix is made from the flag as ESdat writes it: nothing on a detect
  expect_equal(out$prefix, c(NA, "<"))
  expect_equal(out$detect_flag, c("Y", "N"))
})

test_that("a data frame matching no report family warns and returns NULL", {
  expect_warning(
    out <- data_processor(dplyr::tibble(Location_Code = "MW01")),
    "matches no report family"
  )
  expect_null(out)

  expect_warning(
    data_processor(dplyr::tibble(Chem_Code = "7440-50-8", Action_Level = 1)),
    "action_level_processor"
  )
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

test_that("only a < prefix makes a non-detect", {
  # ">2000" is above the lab's range, so it was detected
  raw <- dplyr::tibble(
    sample_date = as.POSIXct("2024-01-15", tz = "UTC"),
    site = "SITE",
    sys_loc_code = "MW01",
    chemical_name = "E. coli",
    result = c(2000, 10, 5, 1),
    result_unit = "MPN/100mL",
    qualifier = c(">", "=", NA, "<")
  )
  out <- suppressWarnings(process_chemistry(raw))

  expect_equal(out$detect_flag, c("Y", "Y", "Y", "N"))
  # the prefix itself is left as the export wrote it
  expect_equal(out$prefix, c(">", "=", NA, "<"))
})

test_that("coercion tolerates the shapes the exports arrive in", {
  expect_equal(coerce_numeric(c("1.5", " 2 ", "x")), c(1.5, 2, NA))
  expect_equal(coerce_numeric(c(NA, NA)), c(NA_real_, NA_real_))
  expect_equal(normalise_yn(c("Dry", "", "yes", NA)), c("Y", "N", "Y", "N"))
})

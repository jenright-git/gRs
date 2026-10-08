# Merged cells read back with their value on every row, as written.
read_summary_sheet <- function(path) {
  openxlsx::read.xlsx(
    path,
    sheet = "Summary Statistics",
    sep.names = " ",
    fillMergedCells = TRUE
  )
}

merged_ranges <- function(path) {
  sub(
    '.*ref="([^"]+)".*',
    "\\1",
    openxlsx::loadWorkbook(path)$worksheets[[1]]$mergeCells
  )
}

write_summary_stats <- function(data = summary_stats(chem_fixture()), ...) {
  path <- withr::local_tempfile(
    fileext = ".xlsx",
    .local_envir = parent.frame()
  )
  suppressMessages(summary_stats_to_excel(data, save_path = path, ...))
  path
}

two_sets_summary <- function(data = two_sets_fixture()) {
  summary_stats(
    data,
    include_criteria = TRUE,
    criteria_col = c(criteria_95, criteria_99)
  )
}

test_that("the sheet has a row per location and analyte, with every statistic", {
  stats <- read_summary_sheet(write_summary_stats())

  expect_named(
    stats,
    c(
      "Monitoring Well",
      "Analyte",
      "Unit",
      "Samples",
      "Detects",
      "Non-Detects",
      "% Detects",
      "% Non-Detects",
      "Minimum",
      "Mean (as reported)",
      "Maximum",
      "Standard Deviation (as reported)",
      paste0(PERCENTILES, "th Percentile")
    )
  )
  expect_equal(stats[["Monitoring Well"]], c("MW01", "MW02"))
  expect_equal(stats$Analyte, c("Copper", "Copper"))
  # three results a well - fewer than mann_kendall_test() needs, all counted
  expect_equal(stats$Samples, c(3, 3))
  expect_equal(stats$Detects, c(2, 2))
})

test_that("`data` is returned invisibly and unchanged", {
  summ <- summary_stats(chem_fixture())

  expect_invisible(
    out <- suppressMessages(summary_stats_to_excel(
      summ,
      save_path = withr::local_tempfile(fileext = ".xlsx")
    ))
  )
  expect_identical(out, summ)
})

test_that("a non-detect minimum is written with its <", {
  stats <- read_summary_sheet(write_summary_stats())

  expect_equal(stats$Minimum, c("<0.5", "<0.5"))
  expect_equal(stats$Maximum, c(2.5, 8))
})

test_that("the maximum is the highest detect, with < only where none was", {
  data <- chem_fixture(
    detect_flag = c("Y", "Y", "N", "N", "N", "N"),
    concentration = c(1, 2, 5, 1, 2, 5)
  )
  stats <- read_summary_sheet(write_summary_stats(summary_stats(data)))

  expect_equal(stats$Maximum, c("2", "<5"))
})

test_that("a lor_multiplier is named in the headings of the figures it changed", {
  stats <- read_summary_sheet(write_summary_stats(
    summary_stats(chem_fixture(), lor_multiplier = 0.5)
  ))

  expect_equal(stats[["Mean (ND at 0.5x LOR)"]][[1]], (1.5 + 2.5 + 0.25) / 3)
  expect_contains(
    names(stats),
    c(
      "Standard Deviation (ND at 0.5x LOR)",
      "95th Percentile (ND at 0.5x LOR)"
    )
  )
  # the minimum is still the reported <0.5
  expect_equal(stats$Minimum, c("<0.5", "<0.5"))
})

test_that("an analyte in two units gets a row for each, its unit beside it", {
  data <- chem_fixture(
    output_unit = c("mg/L", "mg/L", "ug/L", "mg/L", "mg/L", "mg/L"),
    concentration = c(1.5, 2.5, 500, 4, 8, 0.5)
  )
  stats <- read_summary_sheet(write_summary_stats(summary_stats(data)))

  expect_equal(stats[["Monitoring Well"]], c("MW01", "MW01", "MW02"))
  expect_equal(stats$Unit, c("mg/L", "ug/L", "mg/L"))
  expect_equal(stats$Samples, c(2, 1, 3))
  # a single result has no standard deviation
  expect_equal(stats[["Standard Deviation (as reported)"]][2], "-")
})

test_that("each set adds a guideline and a count, headed with its name", {
  stats <- read_summary_sheet(write_summary_stats(two_sets_summary()))

  expect_equal(
    utils::tail(names(stats), 4),
    c(
      "NEMP 95% Guideline",
      "NEMP 95% Exceedances",
      "NEMP 99% Guideline",
      "NEMP 99% Exceedances"
    )
  )
  expect_equal(stats[["NEMP 95% Guideline"]], c(0.002, 0.002))
  expect_equal(stats[["NEMP 95% Exceedances"]], c(2, 2))
  expect_equal(stats[["NEMP 99% Exceedances"]], c(0, 1))
})

test_that("the sets are written in the order summary_stats() was given them", {
  summ <- summary_stats(
    two_sets_fixture(),
    include_criteria = TRUE,
    criteria_col = c(criteria_99, criteria_95)
  )
  stats <- read_summary_sheet(write_summary_stats(summ))

  expect_equal(
    grep("Guideline", names(stats), value = TRUE),
    c("NEMP 99% Guideline", "NEMP 95% Guideline")
  )
})

test_that("set names survive dplyr verbs, and fall back to columns without", {
  filtered <- dplyr::filter(two_sets_summary(), location_code == "MW02")
  stats <- read_summary_sheet(write_summary_stats(filtered))
  expect_equal(stats[["Monitoring Well"]], "MW02")
  expect_equal(utils::tail(names(stats), 1), "NEMP 99% Exceedances")

  unnamed <- two_sets_summary()
  attr(unnamed, "criteria_names") <- NULL
  stats <- read_summary_sheet(write_summary_stats(unnamed))
  expect_equal(
    utils::tail(names(stats), 4),
    c(
      "criteria_95 Guideline",
      "criteria_95 Exceedances",
      "criteria_99 Guideline",
      "criteria_99 Exceedances"
    )
  )
})

test_that("a location with no guideline in a set reads -, not 0", {
  data <- two_sets_fixture()
  mw01 <- data$location_code == "MW01"
  data$criteria_99[mw01] <- NA
  data$criteria_99_exceedance[mw01] <- NA

  stats <- read_summary_sheet(write_summary_stats(two_sets_summary(data)))

  expect_equal(stats[["NEMP 99% Guideline"]], c("-", "5"))
  expect_equal(stats[["NEMP 99% Exceedances"]], c("-", "1"))
})

test_that("a set joined as `criteria` is written under its name", {
  data <- join_action_levels(
    chem_fixture(),
    action_level_fixture(criteria_name = "ANZG 95%", criteria = 2),
    quiet = TRUE
  )
  stats <- read_summary_sheet(write_summary_stats(
    summary_stats(data, include_criteria = TRUE)
  ))

  expect_equal(
    utils::tail(names(stats), 2),
    c("ANZG 95% Guideline", "ANZG 95% Exceedances")
  )
  expect_equal(stats[["ANZG 95% Exceedances"]], c(2, 2))

  # a summary without the guideline writes none
  stats <- read_summary_sheet(write_summary_stats(summary_stats(data)))
  expect_equal(grep("Guideline", names(stats), value = TRUE), character(0))
})

test_that("include_zone puts the zone first and sorts by it", {
  data <- chem_fixture(monitoring_zone = rep(c("Zone B", "Zone A"), each = 3))
  stats <- read_summary_sheet(write_summary_stats(
    summary_stats(data, group_vars = "monitoring_zone"),
    include_zone = TRUE
  ))

  expect_equal(
    names(stats)[1:3],
    c("Monitoring Zone", "Monitoring Well", "Analyte")
  )
  expect_equal(stats[["Monitoring Zone"]], c("Zone A", "Zone B"))
  expect_equal(stats[["Monitoring Well"]], c("MW02", "MW01"))
})

test_that("include_zone without the zone says how to group by it", {
  expect_snapshot(
    write_summary_stats(include_zone = TRUE),
    error = TRUE
  )
})

test_that("a further grouping column is written beside the analyte", {
  data <- chem_fixture(fraction = rep(c("T", "D"), 3))
  stats <- read_summary_sheet(write_summary_stats(
    summary_stats(data, group_vars = "fraction")
  ))

  expect_equal(
    names(stats)[1:5],
    c(
      "Monitoring Well",
      "Analyte",
      "Unit",
      "Fraction",
      "Samples"
    )
  )
  expect_equal(stats$Fraction, c("D", "T", "D", "T"))
  expect_equal(stats$Samples, c(1, 2, 2, 1))
})

test_that("repeated identifiers merge, each within the column to its left", {
  data <- chem_fixture(fraction = rep(c("T", "D"), 3))
  path <- write_summary_stats(
    summary_stats(data, group_vars = "fraction"),
    merge_cells = TRUE
  )

  # MW01 Copper D, MW01 Copper T, MW02 Copper D, MW02 Copper T: each well
  # merges over its two rows, and the analyte and unit within each well -
  # never across the two wells, though both measured Copper in mg/L
  expect_setequal(
    merged_ranges(path),
    c("A2:A3", "A4:A5", "B2:B3", "B4:B5", "C2:C3", "C4:C5")
  )
})

test_that("a zone merges over its wells, and each well within its zone", {
  data <- chem_fixture(
    monitoring_zone = "Zone A",
    fraction = rep(c("T", "D"), 3)
  )
  path <- write_summary_stats(
    summary_stats(data, group_vars = c("monitoring_zone", "fraction")),
    include_zone = TRUE,
    merge_cells = TRUE
  )

  expect_true(
    all(c("A2:A5", "B2:B3", "B4:B5", "C2:C3", "C4:C5") %in% merged_ranges(path))
  )
})

test_that("a blank identifier does not merge with a missing one, written as -", {
  data <- chem_fixture(fraction = rep(c("", NA), 3))
  path <- write_summary_stats(
    summary_stats(data, group_vars = "fraction"),
    merge_cells = TRUE
  )
  stats <- read_summary_sheet(path)

  # each well's blank fraction, then its missing one, which reads -
  expect_equal(stats$Fraction[c(2, 4)], c("-", "-"))
  expect_false(any(startsWith(merged_ranges(path), "D")))
})

test_that("by default nothing merges, so the sheet sorts and filters", {
  data <- chem_fixture(fraction = rep(c("T", "D"), 3))
  path <- write_summary_stats(summary_stats(data, group_vars = "fraction"))

  expect_length(merged_ranges(path), 0)
})

test_that("merge_cells must be TRUE or FALSE", {
  expect_error(
    write_summary_stats(merge_cells = NA),
    "`merge_cells` must be TRUE or FALSE."
  )
})

test_that("results are turned away, with the summary_stats() call to make", {
  expect_snapshot(write_summary_stats(chem_fixture()), error = TRUE)
  expect_snapshot(write_summary_stats(two_sets_fixture()), error = TRUE)
})

test_that("a table stacked by criteria_long() points at criteria_col", {
  expect_snapshot(
    write_summary_stats(
      summary_stats(criteria_long(two_sets_fixture()), include_criteria = TRUE)
    ),
    error = TRUE
  )
})

test_that("summary_stats returns one row per location and chemical", {
  out <- summary_stats(chem_fixture())

  expect_equal(nrow(out), 2)
  expect_equal(out$n_samples, c(3, 3))
  expect_equal(out$n_detects, c(2, 2))
  expect_equal(out$n_non_detects, c(1, 1))
  expect_equal(out$pct_detects, c(66.7, 66.7))
})

test_that("percentiles tolerate a missing concentration", {
  out <- summary_stats(chem_fixture(concentration = c(1, 2, NA, 4, 8, 0.5)))

  expect_equal(out$max, c(2, 8))
  expect_equal(out$p50, c(1.5, 4))
})

test_that("a group with no readable concentration reports NA, not Inf", {
  out <- summary_stats(chem_fixture(
    concentration = c(NA, NA, NA, 4, 8, 0.5)
  ))

  expect_equal(out$min[[1]], NA_real_)
  expect_equal(out$max[[1]], NA_real_)
})

test_that("min_nd and max_nd flag a non-detect at either end of the range", {
  out <- summary_stats(chem_fixture())
  expect_equal(out$min_nd, c(TRUE, TRUE))
  expect_equal(out$max_nd, c(FALSE, FALSE))

  # MW01: every result a non-detect. MW02: a detect and a non-detect tie at
  # the top, and the detect outranks it.
  out <- summary_stats(chem_fixture(
    detect_flag = c("N", "N", "N", "Y", "Y", "N"),
    concentration = c(1, 2, 2, 0.5, 2, 2)
  ))
  expect_equal(out$min_nd, c(TRUE, FALSE))
  expect_equal(out$max_nd, c(TRUE, FALSE))
})

test_that("max is the highest detect, however high a non-detect's LOR", {
  out <- summary_stats(chem_fixture(
    detect_flag = c("Y", "Y", "N", "N", "N", "N"),
    concentration = c(1, 2, 5, 1, 2, 5)
  ))

  # MW02 detected nothing, so its highest result stands in, flagged
  expect_equal(out$max, c(2, 5))
  expect_equal(out$max_nd, c(FALSE, TRUE))
  # the percentiles are still of every result as reported
  expect_equal(out$p99, c(4.94, 4.94))
})

test_that("lor_multiplier substitutes into the mean, SD and percentiles only", {
  out <- summary_stats(chem_fixture(), lor_multiplier = 0.5)

  # MW01: 1.5, 2.5 and <0.5, taken as 0.25
  expect_equal(out$mean[[1]], (1.5 + 2.5 + 0.25) / 3)
  expect_equal(out$std_dev[[1]], sd(c(1.5, 2.5, 0.25)))
  expect_equal(out$p5[[1]], unname(quantile(c(1.5, 2.5, 0.25), 0.05)))
  # the reported figures are left as the lab reported them
  expect_equal(out$min, c(0.5, 0.5))
  expect_equal(out$min_nd, c(TRUE, TRUE))
  expect_equal(out$max, c(2.5, 8))
  expect_equal(out$n_non_detects, c(1, 1))
  expect_equal(attr(out, "lor_multiplier"), 0.5)

  # the default leaves the LOR in place
  expect_equal(summary_stats(chem_fixture())$mean[[1]], (1.5 + 2.5 + 0.5) / 3)
})

test_that("lor_multiplier reads detect_flag where there is no prefix", {
  data <- chem_fixture()
  data$prefix <- NULL

  out <- summary_stats(data, lor_multiplier = 0)
  expect_equal(out$mean[[1]], (1.5 + 2.5) / 3)
})

test_that("lor_multiplier must be a single number, 0 or more", {
  expect_snapshot(
    summary_stats(chem_fixture(), lor_multiplier = -1),
    error = TRUE
  )
})

test_that("group_vars adds grouping columns ahead of the location", {
  data <- chem_fixture(monitoring_zone = rep(c("Zone B", "Zone A"), each = 3))
  out <- summary_stats(data, group_vars = "monitoring_zone")

  expect_equal(
    names(out)[1:4],
    c(
      "monitoring_zone",
      "location_code",
      "chem_name",
      "output_unit"
    )
  )
  expect_equal(out$monitoring_zone, c("Zone A", "Zone B"))
  expect_equal(out$n_samples, c(3, 3))
})

test_that("group_vars names the columns `data` lacks", {
  expect_snapshot(
    summary_stats(chem_fixture(), group_vars = "zone"),
    error = TRUE
  )
})

test_that("each set's recorded name is attached as criteria_names", {
  out <- summary_stats(
    two_sets_fixture(),
    include_criteria = TRUE,
    criteria_col = c(criteria_99, criteria_95)
  )
  expect_equal(
    attr(out, "criteria_names"),
    c(criteria_99 = "NEMP 99%", criteria_95 = "NEMP 95%")
  )

  expect_null(attr(summary_stats(two_sets_fixture()), "criteria_names"))
})

test_that("chem_group is optional, as data_processor()'s warning promises", {
  data <- chem_fixture()
  data$chem_group <- NULL

  expect_equal(nrow(summary_stats(data)), 2)
})

test_that("summary_stats names the columns it cannot do without", {
  data <- chem_fixture()
  data$concentration <- NULL

  expect_snapshot(summary_stats(data), error = TRUE)
})

test_that("include_criteria counts join_action_levels()'s verdict", {
  compared <- join_action_levels(
    chem_fixture(),
    action_level_fixture(criteria = 2),
    quiet = TRUE
  )
  out <- summary_stats(compared, include_criteria = TRUE)

  expect_equal(out$criteria, c(0.002, 0.002))
  expect_equal(out$exceedance_count, c(2, 2))
})

test_that("a set joined under its own name keeps that name in the summary", {
  compared <- join_action_levels(
    chem_fixture(),
    action_level_fixture(),
    value_col = "criteria_99",
    quiet = TRUE
  )
  out <- summary_stats(
    compared,
    include_criteria = TRUE,
    criteria_col = criteria_99
  )

  expect_true(all(
    c("criteria_99", "criteria_99_exceedance_count") %in% names(out)
  ))
})

test_that("results in two units get a row each rather than being pooled", {
  data <- chem_fixture(
    output_unit = c("mg/L", "mg/L", "ug/L", "mg/L", "mg/L", "mg/L"),
    concentration = c(1.5, 2.5, 500, 4, 8, 0.5)
  )
  out <- summary_stats(data)

  expect_equal(nrow(out), 3)
  expect_equal(out$location_code, c("MW01", "MW01", "MW02"))
  expect_equal(out$output_unit, c("mg/L", "ug/L", "mg/L"))
  expect_equal(out$n_samples, c(2, 1, 3))
  expect_equal(out$max, c(2.5, 500, 8))
})

test_that("criteria_long() gives a row per set, counting each result once", {
  compared <- chem_fixture() %>%
    join_action_levels(action_level_fixture(criteria = 2), quiet = TRUE) %>%
    join_action_levels(
      action_level_fixture(criteria_name = "Set 99", criteria = 5000),
      value_col = "criteria_99",
      quiet = TRUE
    )

  expect_no_warning(
    out <- summary_stats(criteria_long(compared), include_criteria = TRUE)
  )

  expect_equal(nrow(out), 4)
  expect_equal(out$criteria_set, rep(c("criteria", "criteria_99"), 2))
  expect_equal(out$n_samples, rep(3, 4))
  expect_equal(out$criteria, c(0.002, 5, 0.002, 5))
  expect_equal(out$exceedance_count, c(2, 0, 2, 1))
})

test_that("several sets get a guideline and a count each, in the order named", {
  out <- summary_stats(
    two_sets_fixture(),
    include_criteria = TRUE,
    criteria_col = c(criteria_99, criteria_95)
  )

  expect_equal(
    utils::tail(names(out), 4),
    c(
      "criteria_99",
      "criteria_99_exceedance_count",
      "criteria_95",
      "criteria_95_exceedance_count"
    )
  )
  expect_equal(nrow(out), 2)
  expect_equal(out$criteria_99, c(5, 5))
  expect_equal(out$criteria_95, c(0.002, 0.002))
  # each counted against its own verdicts: only MW02's 8 mg/L is over 5
  expect_equal(out$criteria_99_exceedance_count, c(0, 1))
  expect_equal(out$criteria_95_exceedance_count, c(2, 2))
})

test_that("a helper picks the sets and not the columns beside them", {
  out <- summary_stats(
    two_sets_fixture(),
    include_criteria = TRUE,
    criteria_col = starts_with("criteria_9")
  )

  expect_equal(
    utils::tail(names(out), 4),
    c(
      "criteria_95",
      "criteria_95_exceedance_count",
      "criteria_99",
      "criteria_99_exceedance_count"
    )
  )
  expect_false("criteria_95_name" %in% names(out))
})

test_that("one missing set among several is an error naming it", {
  expect_error(
    summary_stats(
      two_sets_fixture(),
      include_criteria = TRUE,
      criteria_col = c(criteria_95, criteria_80)
    ),
    "'criteria_80'"
  )
})

test_that("include_criteria says which column to name when it finds none", {
  expect_snapshot(
    summary_stats(chem_fixture(), include_criteria = TRUE),
    error = TRUE
  )
})

test_that("save_path and tidy_path write both shapes", {
  dir <- withr::local_tempdir()
  wide <- file.path(dir, "nested", "summary.xlsx")
  tidy <- file.path(dir, "nested", "summary-tidy.xlsx")

  suppressMessages(summary_stats(
    chem_fixture(),
    save_path = wide,
    tidy_path = tidy
  ))

  expect_true(file.exists(wide))
  expect_true(file.exists(tidy))

  # the unit is a grouping column, so it stays beside the stat it describes
  # rather than being stacked into `value` with the numbers
  expect_named(
    readxl::read_excel(tidy),
    c("location_code", "chem_name", "output_unit", "stat", "value")
  )
  # the non-detect flags are not statistics, and stay out of `value`
  expect_equal(
    intersect(c("min_nd", "max_nd"), readxl::read_excel(tidy)$stat),
    character(0)
  )
})

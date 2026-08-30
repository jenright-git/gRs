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
})

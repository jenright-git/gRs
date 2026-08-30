trend_fixture <- function(conc = 1:8, prefix = NA_character_) {
  dplyr::tibble(
    location_code = "MW01",
    chem_name = "Copper",
    date = as.POSIXct("2024-01-15", tz = "UTC") + (seq_along(conc) - 1) * 8.64e6,
    concentration = as.numeric(conc),
    prefix = rep_len(prefix, length(conc))
  )
}

test_that("a rising series is called Increasing", {
  out <- mann_kendall_test(trend_fixture())

  expect_equal(out$trend, "Increasing")
  expect_lt(out$p_value, 0.05)
  expect_gt(out$tau_statistic, 0)
})

test_that("a falling series is called Decreasing", {
  out <- mann_kendall_test(trend_fixture(8:1))

  expect_equal(out$trend, "Decreasing")
})

test_that("traditional collapses the six categories to three", {
  data <- dplyr::bind_rows(
    trend_fixture(),
    dplyr::mutate(trend_fixture(c(5, 5.1, 4.9, 5, 5.2, 4.8, 5, 5.1)),
                  chem_name = "Zinc")
  )
  out <- mann_kendall_test(data, traditional = TRUE)

  expect_true(all(out$trend %in% TREND_LEVELS_TRADITIONAL))
})

test_that("lor_multiplier is applied before the test", {
  data <- trend_fixture(c(4, 4, 4, 4, 4, 4, 4, 4), rep("<", 8))
  out <- mann_kendall_test(data, lor_multiplier = 0.5)

  expect_equal(out$sample_mean, 2)
})

test_that("series shorter than four samples are dropped", {
  expect_error(
    mann_kendall_test(trend_fixture(1:3)),
    "No data with more than 3 samples"
  )
})

test_that("nd_threshold excludes mostly-non-detect combinations", {
  data <- trend_fixture(1:8, c(rep("<", 7), NA))

  expect_error(
    suppressMessages(mann_kendall_test(data, nd_threshold = 0.5)),
    "No data remaining after applying nd_threshold"
  )
})

test_that("min_detects excludes combinations with too few detects", {
  data <- trend_fixture(1:8, c(rep("<", 7), NA))

  expect_error(
    suppressMessages(mann_kendall_test(data, min_detects = 4)),
    "No data remaining after applying min_detects"
  )
})

test_that("nd_threshold and min_detects cannot both be given", {
  expect_snapshot(
    mann_kendall_test(trend_fixture(), nd_threshold = 0.5, min_detects = 2),
    error = TRUE
  )
})

test_that("mk_analysis returns the statistics the trend rules read", {
  out <- mk_analysis(trend_fixture())

  expect_named(
    out,
    c("p_value", "tau_statistic", "S_statistic", "sample_mean", "SD", "COV")
  )
  expect_equal(out$sample_mean, 4.5)
})

test_that("a flat series has a COV of zero rather than NaN", {
  out <- mk_analysis(trend_fixture(rep(0, 8)))

  expect_equal(out$COV, 0)
})

test_that("both heatmaps build from what mann_kendall_test returns", {
  data <- dplyr::bind_rows(
    trend_fixture(),
    dplyr::mutate(trend_fixture(8:1), chem_name = "Zinc")
  )

  expect_no_error(
    ggplot2::ggplot_build(mann_kendall_heatmap(mann_kendall_test(data)))
  )
  expect_no_error(
    ggplot2::ggplot_build(mann_kendall_heatmap_bw(
      mann_kendall_test(data, traditional = TRUE)
    ))
  )
})

test_that("nd_threshold keeps the combinations under the threshold", {
  data <- dplyr::bind_rows(
    # 6 of 8 non-detect - excluded at 0.5
    dplyr::mutate(trend_fixture(1:8, c(rep("<", 6), NA, NA)), chem_name = "Benzene"),
    # 2 of 6 non-detect - retained
    dplyr::mutate(trend_fixture(1:6, c("<", "<", NA, NA, NA, NA)), chem_name = "Toluene")
  )
  out <- suppressMessages(mann_kendall_test(data, nd_threshold = 0.5))

  expect_equal(out$chem_name, "Toluene")
})

test_that("min_detects keeps the combinations with enough detects", {
  data <- dplyr::bind_rows(
    dplyr::mutate(trend_fixture(1:8, c(rep("<", 6), NA, NA)), chem_name = "Benzene"),
    dplyr::mutate(trend_fixture(1:6, c("<", "<", NA, NA, NA, NA)), chem_name = "Toluene")
  )
  out <- suppressMessages(mann_kendall_test(data, min_detects = 3))

  expect_equal(out$chem_name, "Toluene")
})

test_that("an NA prefix counts as a detect", {
  data <- trend_fixture(1:8, c(rep("<", 4), rep(NA, 4)))
  out <- suppressMessages(mann_kendall_test(data, min_detects = 4))

  expect_equal(nrow(out), 1)
})

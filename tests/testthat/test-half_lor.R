test_that("only non-detects are substituted", {
  out <- half_lor(chem_fixture(), lor_multiplier = 0.5)

  expect_equal(out$concentration, c(1.5, 2.5, 0.25, 4, 8, 0.25))
  expect_equal(out$lor_multiplier_applied, c(NA, NA, 0.5, NA, NA, 0.5))
})

test_that("zero substitution zeroes the non-detects", {
  out <- half_lor(chem_fixture(), lor_multiplier = 0)

  expect_equal(out$concentration, c(1.5, 2.5, 0, 4, 8, 0))
})

test_that("the default leaves concentrations as reported", {
  data <- chem_fixture()

  expect_equal(half_lor(data)$concentration, data$concentration)
})

test_that("the flag and prefix are kept, so a substituted result is still a non-detect", {
  data <- chem_fixture()
  out <- half_lor(data, lor_multiplier = 0.5)

  expect_equal(out$prefix, data$prefix)
  expect_equal(out$detect_flag, data$detect_flag)
})

test_that("non-detects are read from detect_flag, not prefix", {
  # the prefixes say the first and third were not detected; the flags differ
  data <- chem_fixture(
    prefix = c("<", NA, "<", NA, NA, NA),
    detect_flag = c("Y", "N", "Y", "Y", NA, "Y")
  )
  out <- half_lor(data, lor_multiplier = 0.5)

  # the "N", and the missing flag, which is not a detect either
  expect_equal(out$concentration, c(1.5, 1.25, 0.5, 4, 4, 0.5))
})

test_that("a grouped table is substituted as an ungrouped one is", {
  data <- chem_fixture()
  out <- half_lor(dplyr::group_by(data, location_code), lor_multiplier = 0.5)

  expect_equal(dplyr::group_vars(out), "location_code")
  expect_equal(
    out$concentration,
    half_lor(data, lor_multiplier = 0.5)$concentration
  )
})

test_that("a table with no detect_flag is an error", {
  data <- chem_fixture()
  data$detect_flag <- NULL

  expect_error(half_lor(data, 0.5), "has no `detect_flag` column")
})

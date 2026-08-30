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

test_that("the prefix is kept, so a substituted result is still a non-detect", {
  out <- half_lor(chem_fixture(), lor_multiplier = 0.5)

  expect_equal(out$prefix, c("=", "=", "<", "=", "=", "<"))
})

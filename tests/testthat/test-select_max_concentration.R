dup_fixture <- function(...) {
  out <- dplyr::tibble(
    location_code = c("MW01", "MW01", "MW02", "MW02", "MW03"),
    date = as.Date("2024-01-15"),
    chem_name = "Benzene",
    concentration = c(5.2, 6.1, 1.0, 3.0, 2.0),
    prefix = c("=", "=", "=", "<", "="),
    sample_type = c("Normal", "Field_D", "Normal", "Field_D", "Normal")
  )
  args <- list(...)
  for (nm in names(args)) out[[nm]] <- args[[nm]]
  out
}

test_that("the highest of a duplicate pair is kept", {
  out <- select_max_concentration(dup_fixture())

  expect_equal(nrow(out), 3)
  expect_equal(out$concentration[out$location_code == "MW01"], 6.1)
})

test_that("a detect beats a non-detect reported higher", {
  out <- select_max_concentration(dup_fixture())

  expect_equal(out$concentration[out$location_code == "MW02"], 1.0)
  expect_equal(out$prefix[out$location_code == "MW02"], "=")
})

test_that("an all-non-detect group keeps the highest LOR", {
  out <- select_max_concentration(dup_fixture(prefix = rep("<", 5)))

  expect_equal(out$concentration[out$location_code == "MW02"], 3.0)
})

test_that("a single result per group is returned as it stands", {
  data <- dup_fixture(
    location_code = c("MW01", "MW02", "MW03", "MW04", "MW05"),
    sample_type = rep("Normal", 5)
  )

  expect_equal(nrow(select_max_concentration(data)), 5)
})

test_that("several unflagged results in one group still collapse to the max", {
  data <- dup_fixture(sample_type = rep("Normal", 5))
  out <- select_max_concentration(data)

  expect_equal(nrow(out), 3)
  expect_equal(out$concentration[out$location_code == "MW01"], 6.1)
})

test_that("duplicate_types is honoured", {
  data <- dup_fixture(
    sample_type = c("Normal", "Field_T", "Normal", "Normal", "Normal")
  )
  out <- select_max_concentration(data, duplicate_types = "Field_T")

  expect_equal(out$concentration[out$location_code == "MW01"], 6.1)
})

test_that("a table with no sample_type column is returned, with a warning", {
  data <- dup_fixture()
  data$sample_type <- NULL

  expect_snapshot(out <- select_max_concentration(data))
  expect_equal(nrow(out), nrow(data))
})

test_that("the helper columns are not left behind", {
  out <- select_max_concentration(dup_fixture())

  expect_false(any(c(".has_duplicate", ".is_detect") %in% names(out)))
})

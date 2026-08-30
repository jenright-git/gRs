# Copper exceeds, Zinc has no guideline at all. chem_fixture() holds two
# locations, two detects and one non-detect each.
exceedance_data <- function(...) {
  out <- dplyr::bind_rows(
    chem_fixture(),
    chem_fixture(chem_name = "Zinc", chem_code = "7440-66-6")
  )
  out$monitoring_round <- "2024 Q1"
  args <- list(...)
  for (nm in names(args)) out[[nm]] <- args[[nm]]
  out
}

exceedance_compared <- function(data = exceedance_data(), ...) {
  join_action_levels(data, action_level_fixture(...), quiet = TRUE)
}


test_that("exceedance_summary returns one row per exceeding result", {
  out <- exceedance_summary(exceedance_compared(), quiet = TRUE)

  # 1.5, 2.5 (MW01) and 4, 8 (MW02) are detected above 1 mg/L
  expect_equal(nrow(out), 4)
  expect_true(all(out$chem_name == "Copper"))
  expect_equal(sort(out$concentration), c(1.5, 2.5, 4, 8))
})

test_that("the count agrees with analyte_summary()", {
  compared <- exceedance_compared()
  rows <- exceedance_summary(compared, quiet = TRUE)
  summary <- analyte_summary(compared, quiet = TRUE)

  expect_equal(nrow(rows), sum(summary$n_exceedances, na.rm = TRUE))
  expect_equal(
    length(unique(rows$location_code)),
    sum(summary$n_exceedance_locations, na.rm = TRUE)
  )
})

test_that("rows are ordered worst first within each analyte", {
  out <- exceedance_summary(exceedance_compared(), quiet = TRUE)

  expect_equal(out$concentration, c(8, 4, 2.5, 1.5))
})

test_that("the reporting columns are moved to the front", {
  out <- exceedance_summary(exceedance_compared(), quiet = TRUE)

  expect_equal(
    names(out)[1:6],
    c("location_code", "date", "chem_name", "prefix", "concentration",
      "output_unit")
  )
  # everything else is kept
  expect_true("sample_type" %in% names(out))
})

test_that("a non-detect is never an exceedance", {
  # the non-detect reports an LOR of 50, well above the 1 mg/L guideline
  data <- exceedance_data(
    concentration = rep(c(1.5, 2.5, 50, 0.1, 0.2, 0.3), 2)
  )
  out <- exceedance_summary(exceedance_compared(data), quiet = TRUE)

  expect_equal(nrow(out), 2)
  expect_true(all(out$detect_flag == "Y"))
})

test_that("include_lor adds the limits of reporting above the guideline", {
  data <- exceedance_data(
    concentration = rep(c(1.5, 2.5, 50, 0.1, 0.2, 0.3), 2)
  )
  compared <- exceedance_compared(data)

  out <- exceedance_summary(compared, include_lor = TRUE, quiet = TRUE)

  expect_equal(nrow(out), 3)
  expect_equal(sum(out$lor_above_criteria), 1)
  expect_equal(out$concentration[out$lor_above_criteria], 50)
})

test_that("locations are attached per analyte, sorted and de-duplicated", {
  out <- exceedance_summary(exceedance_compared(), quiet = TRUE)
  locs <- attr(out, "locations")

  expect_equal(locs$Copper, c("MW01", "MW02"))
  # an analyte with a guideline but no exceedance gives character(0), not NULL
  expect_equal(
    attr(exceedance_summary(exceedance_compared(criteria = 1e6), quiet = TRUE),
         "locations")$Copper,
    character(0)
  )
})

test_that("an analyte with no guideline is absent from the locations", {
  locs <- attr(exceedance_summary(exceedance_compared(), quiet = TRUE), "locations")

  expect_equal(names(locs), "Copper")
  expect_null(locs$Zinc)
})

test_that("a round with no exceedance returns no rows, not NULL", {
  out <- exceedance_summary(exceedance_compared(criteria = 1e6), quiet = TRUE)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0)
  expect_true("chem_name" %in% names(out))
})

test_that("what was found is reported", {
  expect_message(
    exceedance_summary(exceedance_compared(), round = "2024 Q1"),
    "4 results exceeded their guideline"
  )
  expect_message(
    exceedance_summary(exceedance_compared(criteria = 1e6), round = "2024 Q1"),
    "nothing in monitoring_round = 2024 Q1 exceeded"
  )
})

test_that("only the round being reported on is returned", {
  data <- exceedance_data(
    monitoring_round = rep(rep(c("R1", "R2"), each = 3), 2)
  )
  out <- exceedance_summary(exceedance_compared(data), quiet = TRUE)

  expect_true(all(out$monitoring_round == "R2"))
  expect_equal(attr(out, "round")$round, "R2")
})

test_that("a set joined under its own name is read by criteria_col", {
  compared <- join_action_levels(
    exceedance_data(),
    action_level_fixture(),
    value_col = "criteria_99",
    quiet = TRUE
  )
  out <- exceedance_summary(compared, criteria_col = criteria_99, quiet = TRUE)

  expect_equal(nrow(out), 4)
  expect_equal(names(out)[7], "criteria_99")
})

test_that("criteria_long() input carries the set each row was judged against", {
  compared <- exceedance_data() %>%
    join_action_levels(action_level_fixture(), quiet = TRUE) %>%
    join_action_levels(
      action_level_fixture(criteria_name = "Strict", criteria = 100),
      value_col = "criteria_99",
      quiet = TRUE
    )
  out <- exceedance_summary(criteria_long(compared), quiet = TRUE)

  # 4 above 1 mg/L, and all 6 Copper results above 0.1 mg/L bar the two 0.5s
  expect_true(all(c("criteria", "criteria_99") %in% out$criteria_set))
  expect_equal(sum(out$criteria_set == "criteria"), 4)

  strict <- out[out$criteria_set == "criteria_99", ]
  expect_true(all(strict$concentration > 0.1))
})

test_that("exceedance_summary needs a guideline to have been joined", {
  expect_snapshot(exceedance_summary(exceedance_data()), error = TRUE)
})

test_that("exceedance_summary names the columns it cannot do without", {
  data <- exceedance_compared()
  data$detect_flag <- NULL

  expect_snapshot(exceedance_summary(data), error = TRUE)
})

test_that("save_path writes the rows", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "nested", "exceedances.xlsx")

  suppressMessages(exceedance_summary(exceedance_compared(), save_path = path))

  expect_true(file.exists(path))
})

test_that("create_gt formats the exceeding rows", {
  skip_if_not_installed("gt")

  html <- exceedance_compared() %>%
    exceedance_summary(quiet = TRUE) %>%
    dplyr::select(location_code, chem_name, concentration, criteria,
                  exceedance_ratio) %>%
    create_gt() %>%
    gt::as_raw_html() %>%
    as.character()

  expect_match(html, "Location")
  expect_match(html, "x Criteria")
})

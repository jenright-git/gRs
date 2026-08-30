# Two analytes, one round, so the round machinery stays out of the way of the
# tests that are about the summary itself. Only Copper has a guideline in
# action_level_fixture().
two_analytes <- function(...) {
  out <- dplyr::bind_rows(
    chem_fixture(),
    chem_fixture(chem_name = "Zinc", chem_code = "7440-66-6")
  )
  out$monitoring_round <- "2024 Q1"
  args <- list(...)
  for (nm in names(args)) out[[nm]] <- args[[nm]]
  out
}

compared_fixture <- function(data = two_analytes(), ...) {
  join_action_levels(data, action_level_fixture(...), quiet = TRUE)
}


test_that("analyte_summary returns one row per analyte", {
  out <- analyte_summary(two_analytes(), quiet = TRUE)

  expect_equal(nrow(out), 2)
  expect_equal(out$chem_name, c("Copper", "Zinc"))
  expect_equal(out$n_samples, c(6, 6))
  expect_equal(out$n_detects, c(4, 4))
  expect_equal(out$pct_detects, c(66.7, 66.7))
})

test_that("an analyte reported in two units is not pooled", {
  out <- analyte_summary(
    two_analytes(output_unit = rep(c("mg/L", "ug/L"), each = 6)),
    quiet = TRUE
  )

  expect_equal(nrow(out), 2)
  expect_equal(out$output_unit, c("mg/L", "ug/L"))
})

test_that("the maximum is the highest detection, not the highest result", {
  # the third result is a non-detect reported at an LOR above every detection
  out <- analyte_summary(
    two_analytes(concentration = rep(c(1.5, 2.5, 50, 4, 8, 0.5), 2)),
    quiet = TRUE
  )

  expect_equal(out$max_conc, c(8, 8))
  expect_true(all(is.na(out$max_prefix)))
  expect_equal(out$max_location, c("MW02", "MW02"))
})

test_that("an analyte never detected reports its highest result with the <", {
  out <- analyte_summary(
    two_analytes(
      detect_flag = rep("N", 12),
      prefix = rep("<", 12),
      concentration = rep(c(1, 2, 3, 4, 5, 6), 2)
    ),
    quiet = TRUE
  )

  expect_equal(out$max_conc, c(6, 6))
  expect_equal(out$max_prefix, c("<", "<"))
  expect_equal(out$n_detects, c(0, 0))
})

test_that("the minimum is the lowest result, detected or not", {
  out <- analyte_summary(two_analytes(), quiet = TRUE)

  expect_equal(out$min_conc, c(0.5, 0.5))
  expect_equal(out$min_prefix, c("<", "<"))
})

test_that("exceeding locations are counted, de-duplicated and sorted", {
  out <- analyte_summary(compared_fixture(), quiet = TRUE)
  copper <- out[out$chem_name == "Copper", ]

  # 1000 ug/L against results in mg/L
  expect_equal(copper$criteria, 1)
  # 1.5, 2.5 (MW01) and 4, 8 (MW02) are detected above it
  expect_equal(copper$n_exceedances, 4L)
  expect_equal(copper$n_exceedance_locations, 2L)
  expect_equal(copper$exceedance_locations, "MW01, MW02")
})

test_that("an analyte with no guideline reports NA, not zero", {
  out <- analyte_summary(compared_fixture(), quiet = TRUE)
  zinc <- out[out$chem_name == "Zinc", ]

  expect_true(is.na(zinc$criteria))
  expect_true(is.na(zinc$n_exceedances))
  expect_true(is.na(zinc$n_exceedance_locations))
  expect_true(is.na(zinc$exceedance_locations))
})

test_that("a guideline with nothing above it reports 0, not NA", {
  out <- analyte_summary(
    compared_fixture(criteria = 1e6),
    quiet = TRUE
  )
  copper <- out[out$chem_name == "Copper", ]

  expect_equal(copper$n_exceedances, 0L)
  expect_equal(copper$n_exceedance_locations, 0L)
  expect_equal(copper$exceedance_locations, "")
})

test_that("the exceedance count agrees with join_action_levels()", {
  compared <- compared_fixture()
  out <- analyte_summary(compared, quiet = TRUE)

  expect_equal(
    sum(out$n_exceedances, na.rm = TRUE),
    sum(compared$exceedance, na.rm = TRUE)
  )
})

test_that("include_criteria is off when no guideline column is present", {
  out <- analyte_summary(two_analytes(), quiet = TRUE)

  expect_false("criteria" %in% names(out))
  expect_false("n_exceedances" %in% names(out))
})

test_that("a set joined under its own name keeps that name", {
  compared <- join_action_levels(
    two_analytes(),
    action_level_fixture(),
    value_col = "criteria_99",
    quiet = TRUE
  )
  out <- analyte_summary(compared, criteria_col = criteria_99, quiet = TRUE)

  expect_true(all(
    c("criteria_99", "criteria_99_n_exceedances",
      "criteria_99_exceedance_locations") %in% names(out)
  ))
})

test_that("criteria_long() input gives one row per analyte per set", {
  compared <- two_analytes() %>%
    join_action_levels(action_level_fixture(), quiet = TRUE) %>%
    join_action_levels(
      action_level_fixture(criteria_name = "Strict", criteria = 100),
      value_col = "criteria_99",
      quiet = TRUE
    )
  out <- analyte_summary(criteria_long(compared), quiet = TRUE)

  expect_equal(nrow(out), 4)
  expect_equal(sort(unique(out$criteria_set)), c("criteria", "criteria_99"))

  strict <- out[out$chem_name == "Copper" & out$criteria_set == "criteria_99", ]
  expect_equal(strict$criteria, 0.1)
})


# Round resolution ----------------------------------------------------------

test_that("the latest round is used by default and reported", {
  data <- two_analytes(monitoring_round = rep(rep(c("R1", "R2"), each = 3), 2))

  expect_message(analyte_summary(data), "monitoring_round = R2")
  expect_equal(analyte_summary(data, quiet = TRUE)$n_samples, c(3, 3))
})

test_that("a text round is ordered by its dates, not alphabetically", {
  # "2024 Q9" sorts after "2024 Q10" as text; the dates say otherwise
  data <- two_analytes(
    monitoring_round = rep(rep(c("2024 Q9", "2024 Q10"), each = 3), 2)
  )
  out <- analyte_summary(data, quiet = TRUE)

  expect_equal(attr(out, "round")$round, "2024 Q10")
})

test_that("an explicit round is honoured", {
  data <- two_analytes(monitoring_round = rep(rep(c("R1", "R2"), each = 3), 2))
  out <- analyte_summary(data, round = "R1", quiet = TRUE)

  expect_equal(out$n_samples, c(3, 3))
  expect_equal(out$max_location, c("MW01", "MW01"))
})

test_that("round_col can name any column, quoted or not", {
  data <- two_analytes()
  data$event <- rep(rep(c("A", "B"), each = 3), 2)

  expect_equal(
    analyte_summary(data, round_col = event, round = "A", quiet = TRUE),
    analyte_summary(data, round_col = "event", round = "A", quiet = TRUE)
  )
})

test_that("a date column stands in where there is no round column", {
  data <- two_analytes()
  data$monitoring_round <- NULL
  out <- analyte_summary(data, quiet = TRUE)

  expect_equal(attr(out, "round")$column, "date")
  expect_equal(out$n_samples, c(1, 1))
})

test_that("a round that matches nothing says which rounds are present", {
  expect_snapshot(
    analyte_summary(two_analytes(), round = "2099 Q4"),
    error = TRUE
  )
})

test_that("analyte_summary names the columns it cannot do without", {
  data <- two_analytes()
  data$detect_flag <- NULL

  expect_snapshot(analyte_summary(data), error = TRUE)
})

test_that("group_vars must name real columns", {
  expect_snapshot(
    analyte_summary(two_analytes(), group_vars = "zone", quiet = TRUE),
    error = TRUE
  )
})

test_that("group_vars adds a level to the summary", {
  out <- analyte_summary(
    two_analytes(monitoring_zone = rep(rep(c("North", "South"), each = 3), 2)),
    group_vars = "monitoring_zone",
    quiet = TRUE
  )

  expect_equal(nrow(out), 4)
  expect_equal(names(out)[1:2], c("monitoring_zone", "chem_name"))
})

test_that("save_path writes the table and creates its directory", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "nested", "round-summary.xlsx")

  suppressMessages(analyte_summary(two_analytes(), save_path = path))

  expect_true(file.exists(path))
})


# create_gt -----------------------------------------------------------------

test_that("create_gt formats a summary and merges the prefix pairs", {
  skip_if_not_installed("gt")

  tbl <- create_gt(analyte_summary(two_analytes(), quiet = TRUE))

  expect_s3_class(tbl, "gt_tbl")
  html <- as.character(gt::as_raw_html(tbl))
  expect_match(html, "&lt;0.5|<0.5")
  expect_match(html, "Analyte")
  expect_match(html, "Samples \\(n\\)")
})

test_that("create_gt labels can be overridden", {
  skip_if_not_installed("gt")

  html <- analyte_summary(two_analytes(), quiet = TRUE) %>%
    create_gt(labels = c(max_location = "Peak Location")) %>%
    gt::as_raw_html() %>%
    as.character()

  expect_match(html, "Peak Location")
})

test_that("prefix_value_pairs finds only complete pairs", {
  expect_equal(
    prefix_value_pairs(c("min_prefix", "min_conc", "max_prefix")),
    list(c("min_prefix", "min_conc"))
  )
  expect_equal(prefix_value_pairs(c("a", "b")), list())
})

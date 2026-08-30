# One location, one analyte, four rounds - so the history/current split is the
# only thing under test. R4 is the round reported on by default.
history_fixture <- function(...) {
  out <- dplyr::tibble(
    date = as.POSIXct(
      c("2021-01-15", "2022-01-15", "2023-01-15", "2024-01-15"),
      tz = "UTC"
    ),
    monitoring_round = c("R1", "R2", "R3", "R4"),
    location_code = "MW01",
    chem_name = "Copper",
    chem_code = "7440-50-8",
    prefix = c(NA, NA, NA, NA),
    detect_flag = c("Y", "Y", "Y", "Y"),
    concentration = c(2, 5, 3, 8),
    output_unit = "mg/L",
    matrix_code = "WATER"
  )
  args <- list(...)
  for (nm in names(args)) out[[nm]] <- args[[nm]]
  out
}


test_that("historical_range splits history from the current round", {
  out <- historical_range(history_fixture(), quiet = TRUE)

  expect_equal(nrow(out), 1)
  expect_equal(out$n_samples, 4L)
  expect_equal(out$n_current, 1L)
  expect_equal(out$hist_min_conc, 2)
  expect_equal(out$hist_max_conc, 5)
  expect_equal(out$current_conc, 8)
})

test_that("a new maximum is flagged and its ratio reported", {
  out <- historical_range(history_fixture(), quiet = TRUE)

  expect_true(out$new_max)
  expect_false(out$new_min)
  expect_equal(out$max_ratio, 8 / 5)
  expect_false(out$spike)
})

test_that("a new minimum can be set by a non-detect", {
  out <- historical_range(
    history_fixture(
      prefix = c(NA, NA, NA, "<"),
      detect_flag = c("Y", "Y", "Y", "N"),
      concentration = c(2, 5, 3, 0.1)
    ),
    quiet = TRUE
  )

  expect_true(out$new_min)
  expect_false(out$new_max)
  expect_equal(out$current_prefix, "<")
})

test_that("a non-detect can never set a new maximum", {
  # the current LOR is above every historical detection
  out <- historical_range(
    history_fixture(
      prefix = c(NA, NA, NA, "<"),
      detect_flag = c("Y", "Y", "Y", "N"),
      concentration = c(2, 5, 3, 50)
    ),
    quiet = TRUE
  )

  expect_false(out$new_max)
  expect_true(is.na(out$max_ratio))
})

test_that("the historical maximum ignores a non-detect reported high", {
  # an old "<40" must not stand as a maximum that was never measured
  out <- historical_range(
    history_fixture(
      prefix = c(NA, "<", NA, NA),
      detect_flag = c("Y", "N", "Y", "Y"),
      concentration = c(2, 40, 3, 8)
    ),
    quiet = TRUE
  )

  expect_equal(out$hist_max_conc, 3)
  expect_true(is.na(out$hist_max_prefix))
  # the historical minimum does look at every result, so the 2 still wins
  expect_equal(out$hist_min_conc, 2)
  expect_true(out$new_max)
})

test_that("a spike is a new maximum well above the previous one", {
  out <- historical_range(
    history_fixture(concentration = c(2, 5, 3, 500)),
    quiet = TRUE
  )

  expect_true(out$spike)
  expect_equal(out$max_ratio, 100)
  expect_equal(nrow(historical_range(history_fixture(), keep = "spike")), 0)
})

test_that("spike_factor tunes the check and NULL switches it off", {
  data <- history_fixture(concentration = c(2, 5, 3, 500))

  expect_true(historical_range(data, spike_factor = 50, quiet = TRUE)$spike)
  expect_false(historical_range(data, spike_factor = 200, quiet = TRUE)$spike)
  expect_false(historical_range(data, spike_factor = NULL, quiet = TRUE)$spike)
})

test_that("a group first sampled this round has no history and no new maximum", {
  out <- historical_range(history_fixture()[4, ], quiet = TRUE)

  expect_true(is.na(out$hist_min_conc))
  expect_true(is.na(out$hist_max_conc))
  expect_false(out$new_max)
  expect_false(out$new_min)
  # NA, never the Inf an empty min() gives
  expect_false(is.infinite(out$hist_min_conc))
})

test_that("a group not sampled this round keeps its history", {
  data <- dplyr::bind_rows(
    history_fixture(),
    history_fixture(location_code = "MW02")[1:3, ]
  )
  out <- historical_range(data, quiet = TRUE)

  mw02 <- out[out$location_code == "MW02", ]
  expect_equal(mw02$n_current, 0L)
  expect_true(is.na(mw02$current_conc))
  expect_equal(mw02$hist_max_conc, 5)
  expect_false(mw02$new_max)
})

test_that("history is what came before a mid-record round", {
  out <- historical_range(history_fixture(), round = "R3", quiet = TRUE)

  # R4's 8 was not collected when R3 was reported, so it is left out entirely
  expect_equal(out$n_samples, 3L)
  expect_equal(out$current_conc, 3)
  expect_equal(out$hist_max_conc, 5)
  expect_false(out$new_max)
})

test_that("the current result is the maximum of the round, and duplicates are named", {
  data <- dplyr::bind_rows(history_fixture(), history_fixture()[4, ])
  data$concentration[5] <- 12

  expect_warning(
    out <- historical_range(data, quiet = TRUE),
    "select_max_concentration"
  )
  expect_equal(out$n_current, 2L)
  expect_equal(out$current_conc, 12)
})

test_that("keep filters to the screening list, worst first", {
  data <- dplyr::bind_rows(
    history_fixture(),
    history_fixture(location_code = "MW02", concentration = c(2, 5, 3, 500)),
    history_fixture(location_code = "MW03", concentration = c(2, 5, 3, 1))
  )

  all_rows <- historical_range(data, quiet = TRUE)
  expect_equal(nrow(all_rows), 3)

  new_max <- historical_range(data, keep = "new_max", quiet = TRUE)
  expect_equal(new_max$location_code, c("MW02", "MW01"))

  spikes <- historical_range(data, keep = "spike", quiet = TRUE)
  expect_equal(spikes$location_code, "MW02")
})

test_that("an empty screening list says so rather than returning silently", {
  expect_message(
    historical_range(history_fixture(), keep = "spike"),
    "nothing in monitoring_round = R4"
  )
})

test_that("group_vars can drop the location and summarise per analyte", {
  data <- dplyr::bind_rows(
    history_fixture(),
    history_fixture(location_code = "MW02", concentration = c(2, 5, 3, 20))
  )
  out <- historical_range(data, group_vars = character(0), quiet = TRUE)

  expect_equal(nrow(out), 1)
  expect_equal(out$n_samples, 8L)
  expect_equal(out$current_conc, 20)
  expect_equal(out$n_current, 2L)
})

test_that("the guideline and the current verdict are carried through", {
  compared <- join_action_levels(
    history_fixture(),
    action_level_fixture(criteria = 4000),
    quiet = TRUE
  )
  out <- historical_range(compared, quiet = TRUE)

  expect_equal(out$criteria, 4)
  expect_true(out$current_exceedance)
  expect_equal(nrow(historical_range(compared, keep = "exceedance", quiet = TRUE)), 1)
})

test_that("keep = exceedance needs a guideline", {
  expect_snapshot(
    historical_range(history_fixture(), keep = "exceedance", quiet = TRUE),
    error = TRUE
  )
})

test_that("a set joined under its own name keeps that name", {
  compared <- join_action_levels(
    history_fixture(),
    action_level_fixture(),
    value_col = "criteria_99",
    quiet = TRUE
  )
  out <- historical_range(compared, criteria_col = criteria_99, quiet = TRUE)

  expect_true(all(
    c("criteria_99", "criteria_99_current_exceedance") %in% names(out)
  ))
})

test_that("criteria_long() input gives one row per group per set", {
  compared <- history_fixture() %>%
    join_action_levels(action_level_fixture(), quiet = TRUE) %>%
    join_action_levels(
      action_level_fixture(criteria_name = "Strict", criteria = 100),
      value_col = "criteria_99",
      quiet = TRUE
    )
  out <- historical_range(criteria_long(compared), quiet = TRUE)

  expect_equal(nrow(out), 2)
  expect_equal(sort(out$criteria), c(0.1, 1))
})

test_that("historical_range names the columns it cannot do without", {
  data <- history_fixture()
  data$detect_flag <- NULL

  expect_snapshot(historical_range(data), error = TRUE)
})

test_that("a missing date column is a warning, not a silent reordering", {
  data <- history_fixture()
  data$date <- NULL

  expect_warning(
    historical_range(data, round = "R4", quiet = TRUE),
    "no 'date' column"
  )
})

test_that("spike_factor must be a positive number", {
  expect_snapshot(
    historical_range(history_fixture(), spike_factor = -1, quiet = TRUE),
    error = TRUE
  )
})

test_that("save_path writes the table", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "nested", "history.xlsx")

  suppressMessages(historical_range(history_fixture(), save_path = path))

  expect_true(file.exists(path))
})

test_that("create_gt formats a historical range table", {
  skip_if_not_installed("gt")

  rendered <- function(...) {
    history_fixture() %>%
      historical_range(quiet = TRUE) %>%
      create_gt(...) %>%
      gt::as_raw_html() %>%
      as.character()
  }

  html <- rendered()

  # the two ends of the history are read as the one range they describe
  expect_match(html, "Historical Range")
  expect_match(html, "2 - 5")
  expect_match(html, "Current Concentration")
  expect_match(html, "x Previous Maximum")
  # the ratio is rounded to one place, not the concentration decimals
  expect_match(html, "1.6")

  # and apart again where the two ends are wanted in columns of their own
  apart <- rendered(merge_range = FALSE)
  expect_match(apart, "Historical Minimum")
  expect_match(apart, "Historical Maximum")
})

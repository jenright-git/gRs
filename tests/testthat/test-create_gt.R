# A summary carrying a guideline set joined under its own name, which is the
# shape that used to lose every guideline label.
summary_fixture <- function(value_col = "criteria") {
  chem <- chem_fixture()
  levels <- action_level_fixture(criteria = 2000)
  joined <- join_action_levels(
    chem,
    levels,
    value_col = value_col,
    quiet = TRUE
  )
  analyte_summary(joined, criteria_col = !!rlang::sym(value_col), quiet = TRUE)
}

test_that("criteria_stems() finds the default set and named sets", {
  expect_equal(
    criteria_stems(c("chem_name", "criteria", "n_exceedances")),
    "criteria"
  )
  expect_equal(
    criteria_stems(c("chem_name", "criteria_99", "criteria_99_n_exceedances")),
    "criteria_99"
  )
  expect_equal(
    criteria_stems(c(
      "criteria",
      "n_exceedances",
      "criteria_99",
      "criteria_99_n_exceedances"
    )),
    c("criteria", "criteria_99")
  )
})

test_that("criteria_stems() ignores a criteria column with nothing beside it", {
  # A column added by hand carries no counts, so there is no set to span.
  expect_equal(criteria_stems(c("chem_name", "criteria")), character(0))
  expect_equal(
    criteria_stems(c("chem_name", "criteria", "criteria_set", "criteria_unit")),
    character(0)
  )
})

test_that("a named set's columns are labelled as the default set's are", {
  out <- summary_fixture("criteria_99")
  lab <- resolve_labels(
    names(out),
    NULL,
    criteria_stems(names(out)),
    spanned = FALSE
  )

  expect_equal(lab[["criteria_99"]], "Criteria")
  expect_equal(lab[["criteria_99_n_exceedances"]], "Exceedances (n)")
  expect_equal(
    lab[["criteria_99_n_exceedance_locations"]],
    "Exceeding Locations (n)"
  )
  expect_equal(
    lab[["criteria_99_exceedance_locations"]],
    "Exceeding Locations"
  )
  # the columns that never depended on the set are untouched
  expect_equal(lab[["chem_name"]], "Analyte")
  expect_equal(lab[["max_conc"]], "Maximum Concentration")
})

test_that("historical_range()'s named-set columns are labelled too", {
  chem <- chem_fixture()
  levels <- action_level_fixture(criteria = 2000)
  joined <- join_action_levels(
    chem,
    levels,
    value_col = "criteria_99",
    quiet = TRUE
  )
  out <- historical_range(joined, criteria_col = criteria_99, quiet = TRUE)
  lab <- resolve_labels(
    names(out),
    NULL,
    criteria_stems(names(out)),
    spanned = FALSE
  )

  expect_equal(lab[["criteria_99"]], "Criteria")
  expect_equal(lab[["criteria_99_current_exceedance"]], "Exceeds Criteria")
  expect_equal(lab[["max_ratio"]], "x Previous Maximum")
})

test_that("a set's value column takes the caller's label, or names a spanner", {
  nms <- c("criteria_99", "criteria_99_n_exceedances")
  labels <- c(criteria_99 = "ANZG 99%")

  # one set: the label names the column, since there is no spanner to carry it
  lab <- resolve_labels(nms, labels, "criteria_99", spanned = FALSE)
  expect_equal(lab[["criteria_99"]], "ANZG 99%")

  # two sets: the label names the spanner and the column says what it holds
  lab <- resolve_labels(nms, labels, "criteria_99", spanned = TRUE)
  expect_equal(lab[["criteria_99"]], "Criteria")
  expect_equal(stem_label("criteria_99", labels), "ANZG 99%")
  expect_equal(stem_label("criteria_99", NULL), "Criteria 99")
})

test_that("counts of a named set are still counts", {
  out <- summary_fixture("criteria_99")
  numeric_cols <- names(out)[vapply(out, is.numeric, logical(1))]

  expect_true("criteria_99_n_exceedances" %in% grep(
    "(^|_)n_",
    numeric_cols,
    value = TRUE
  ))
  # and nothing that is not a count is swept in with them
  expect_false(any(
    c("min_conc", "max_conc", "criteria_99") %in%
      grep("(^|_)n_", numeric_cols, value = TRUE)
  ))
})

test_that("the round is named as a source note", {
  out <- summary_fixture()
  expect_match(round_note_text(out), "^Date: ")

  out <- analyte_summary(
    chem_fixture(monitoring_round = rep(c("2025 Q1", "2025 Q2"), 3)),
    quiet = TRUE
  )
  expect_equal(round_note_text(out), "Monitoring round: 2025 Q2")
})

test_that("round_note_text() is NULL where there is no round to name", {
  expect_null(round_note_text(dplyr::tibble(chem_name = "Copper")))
  expect_null(round_note_text(structure(
    dplyr::tibble(chem_name = "Copper"),
    round = list(column = "date", round = NA)
  )))
})

test_that("create_gt() formats a summary without touching its columns", {
  skip_if_not_installed("gt")
  out <- summary_fixture("criteria_99")
  tbl <- create_gt(out)

  expect_s3_class(tbl, "gt_tbl")

  # Nothing is lost: each merge carries its value into the cell that survives.
  # The concentration halves go into their prefix cell, and the maximum then
  # goes into the minimum's cell as the upper end of the range.
  merged <- c(
    vapply(prefix_value_pairs(names(out)), `[[`, character(1), 2),
    vapply(range_pairs(names(out)), `[[`, character(1), 2)
  )
  body <- gt::extract_body(tbl)
  expect_equal(names(body), setdiff(names(out), merged))
  expect_match(body$min_prefix[[1]], "0.5", fixed = TRUE)

  expect_equal(body$criteria_99_n_exceedances, "0")
})

test_that("create_gt() spans two sets and leaves one set unspanned", {
  skip_if_not_installed("gt")

  one <- create_gt(summary_fixture())
  expect_length(gt:::dt_spanners_get(one)$spanner_label, 0)

  two <- summary_fixture()
  two$criteria_99 <- 1000
  two$criteria_99_n_exceedances <- 1L
  spanners <- gt:::dt_spanners_get(
    create_gt(two, labels = c(criteria = "ANZG 95%", criteria_99 = "ANZG 99%"))
  )
  expect_equal(
    unlist(spanners$spanner_label),
    c("ANZG 95%", "ANZG 99%")
  )
})

test_that("a spanned default set does not name itself twice", {
  # The spanner says which set it is; the heading beneath says what the value
  # is. The default set is the one that would otherwise do both.
  nms <- c(
    "criteria",
    "n_exceedances",
    "criteria_99",
    "criteria_99_n_exceedances"
  )
  labels <- c(criteria = "ANZG 95%", criteria_99 = "ANZG 99%")
  stems <- c("criteria", "criteria_99")
  lab <- resolve_labels(nms, labels, stems, spanned = TRUE)

  expect_equal(lab[["criteria"]], "Criteria")
  expect_equal(lab[["criteria_99"]], "Criteria")
  expect_equal(lab[["n_exceedances"]], "Exceedances (n)")
  expect_equal(lab[["criteria_99_n_exceedances"]], "Exceedances (n)")
})

test_that("create_gt() runs on a table carrying no guideline at all", {
  skip_if_not_installed("gt")
  out <- analyte_summary(chem_fixture(), quiet = TRUE)
  expect_s3_class(create_gt(out), "gt_tbl")
  expect_s3_class(create_gt(out, round_note = FALSE), "gt_tbl")
})


# ---------------------------------------------------------------------------
# highlight
# ---------------------------------------------------------------------------

# The styling is only visible in the rendered table, so these read the HTML.
# gt writes colours back in upper case, hence the fold.
rendered <- function(...) tolower(gt::as_raw_html(create_gt(...)))

styled_history <- function(concentration, ...) {
  historical_range(
    chem_fixture(
      concentration = concentration,
      detect_flag = rep("Y", 6),
      prefix = rep(NA, 6)
    ),
    quiet = TRUE,
    ...
  )
}

test_that("a new maximum is bold and a spike is shaded and footnoted", {
  skip_if_not_installed("gt")
  out <- styled_history(c(1, 2, 0.5, 4, 8, 200))
  expect_equal(sum(out$new_max), 1)
  expect_equal(sum(out$spike), 1)

  html <- rendered(out)
  expect_true(grepl("font-weight: bold", html, fixed = TRUE))
  expect_true(grepl("f8d7da", html, fixed = TRUE))
  expect_true(grepl("10x the previous detected maximum", html, fixed = TRUE))
})

test_that("a new minimum is italic and underlined", {
  skip_if_not_installed("gt")
  out <- styled_history(c(5, 6, 7, 8, 9, 0.01))
  expect_equal(sum(out$new_min), 1)
  expect_true(grepl("underline", rendered(out), fixed = TRUE))
})

test_that("the flags the styling has said are hidden, and max_ratio is kept", {
  skip_if_not_installed("gt")
  html <- rendered(styled_history(c(1, 2, 0.5, 4, 8, 200)))

  expect_false(grepl(">spike<", html, fixed = TRUE))
  expect_false(grepl(">new maximum<", html, fixed = TRUE))
  expect_false(grepl(">new minimum<", html, fixed = TRUE))
  # a number the reader wants, not a flag the styling has replaced
  expect_true(grepl("x previous maximum", html, fixed = TRUE))
})

test_that("the flags are hidden whether or not the round tripped them", {
  skip_if_not_installed("gt")
  # One shape from round to round: a quiet round must not grow three columns.
  html <- rendered(styled_history(c(9, 8, 7, 6, 5, 4)))
  expect_false(grepl(">spike<", html, fixed = TRUE))
  expect_false(grepl(">new maximum<", html, fixed = TRUE))
})

test_that("highlight = FALSE leaves the table unstyled and the flags shown", {
  skip_if_not_installed("gt")
  out <- styled_history(c(1, 2, 0.5, 4, 8, 200))
  html <- rendered(out, highlight = FALSE)

  expect_false(grepl("f8d7da", html, fixed = TRUE))
  expect_false(grepl("verify against the lab report", html, fixed = TRUE))
  expect_true(grepl(">spike<", html, fixed = TRUE))
})

test_that("an exceedance is bold, for every set the table carries", {
  skip_if_not_installed("gt")
  chem <- chem_fixture(
    concentration = c(1, 2, 0.5, 4, 8, 50),
    detect_flag = rep("Y", 6),
    prefix = rep(NA, 6)
  )
  levels <- action_level_fixture(criteria = 1000)

  default <- analyte_summary(
    join_action_levels(chem, levels, quiet = TRUE),
    quiet = TRUE
  )
  expect_equal(default$n_exceedances, 1)
  expect_true(grepl("font-weight: bold", rendered(default), fixed = TRUE))

  # a set joined under its own name is followed just the same
  named <- analyte_summary(
    join_action_levels(chem, levels, value_col = "criteria_99", quiet = TRUE),
    criteria_col = criteria_99,
    quiet = TRUE
  )
  expect_equal(named$criteria_99_n_exceedances, 1)
  expect_true(grepl("font-weight: bold", rendered(named), fixed = TRUE))

  # and the count stays on the table: how many exceeded is worth reading
  expect_true(grepl("exceedances (n)", rendered(default), fixed = TRUE))
})

test_that("nothing is emphasised where nothing was found", {
  skip_if_not_installed("gt")
  out <- analyte_summary(
    join_action_levels(
      chem_fixture(),
      action_level_fixture(criteria = 1e9),
      quiet = TRUE
    ),
    quiet = TRUE
  )
  expect_equal(out$n_exceedances, 0)
  expect_false(grepl("font-weight: bold", rendered(out), fixed = TRUE))
})

test_that("the spike footnote does not claim a factor there was none of", {
  skip_if_not_installed("gt")
  out <- styled_history(c(1, 2, 0.5, 4, 8, 200), spike_factor = NULL)
  expect_equal(sum(out$spike), 0)
  expect_false(
    grepl("verify against the lab report", rendered(out), fixed = TRUE)
  )
})

test_that("highlight is safe on a table carrying none of the flags", {
  skip_if_not_installed("gt")
  out <- summary_stats(gRs_data)
  expect_s3_class(create_gt(out), "gt_tbl")
})


# ---------------------------------------------------------------------------
# merge_range, and the blanked prefix behind it
# ---------------------------------------------------------------------------

test_that("range_pairs() finds both tables' minimum/maximum pairs", {
  expect_equal(
    range_pairs(c("min_prefix", "max_prefix", "chem_name")),
    list(c("min_prefix", "max_prefix"))
  )
  expect_equal(
    range_pairs(c("hist_min_prefix", "hist_max_prefix")),
    list(c("hist_min_prefix", "hist_max_prefix"))
  )
  # a minimum with no maximum beside it is not a range
  expect_equal(range_pairs(c("min_prefix", "chem_name")), list())
})

test_that("a merged range is labelled for what it holds, not where it began", {
  expect_equal(
    range_labels(list(c("hist_min_prefix", "hist_max_prefix")), NULL)[[
      "hist_min_prefix"
    ]],
    "Historical Range"
  )
  expect_equal(
    range_labels(list(c("min_prefix", "max_prefix")), NULL)[["min_prefix"]],
    "Concentration Range"
  )
  # the caller's own label still wins
  expect_equal(
    range_labels(
      list(c("min_prefix", "max_prefix")),
      c(min_prefix = "Spread")
    )[["min_prefix"]],
    "Spread"
  )
})

test_that("the two ends are merged into one cell", {
  skip_if_not_installed("gt")
  out <- styled_history(c(1, 2, 0.5, 4, 8, 200))
  body <- gt::extract_body(create_gt(out))

  expect_false("hist_max_prefix" %in% names(body))
  expect_match(body$hist_min_prefix[[1]], " - ", fixed = TRUE)
  expect_true(grepl("historical range", rendered(out), fixed = TRUE))
})

test_that("merge_range = FALSE keeps the two ends apart", {
  skip_if_not_installed("gt")
  out <- styled_history(c(1, 2, 0.5, 4, 8, 200))
  body <- gt::extract_body(create_gt(out, merge_range = FALSE))

  expect_true("hist_max_prefix" %in% names(body))
  apart <- rendered(out, merge_range = FALSE)
  expect_true(grepl("historical minimum", apart, fixed = TRUE))
})

test_that("a detected result is not pushed onto a second line", {
  skip_if_not_installed("gt")
  # gt renders a blanked cell as a line break; merged in front of the value it
  # would sit the number under an empty line.
  out <- styled_history(c(1, 2, 0.5, 4, 8, 200))
  cells <- gt::extract_body(create_gt(out))

  expect_equal(cells$current_prefix[[2]], "200")
  expect_false(grepl("<br", cells$current_prefix[[2]], fixed = TRUE))
  expect_false(grepl("<br", cells$hist_min_prefix[[2]], fixed = TRUE))
})

test_that("a non-detect still carries its < through the merges", {
  skip_if_not_installed("gt")
  out <- historical_range(
    chem_fixture(
      concentration = c(1, 2, 3, 4, 8, 0.5),
      detect_flag = c("Y", "Y", "Y", "Y", "Y", "N"),
      prefix = c(NA, NA, NA, NA, NA, "<")
    ),
    quiet = TRUE
  )
  cells <- gt::extract_body(create_gt(out))
  expect_match(cells$current_prefix[[2]], "0.5", fixed = TRUE)
  expect_match(cells$current_prefix[[2]], "lt;", fixed = TRUE)
})


# A range with only one end to report: the separator has nothing to separate.
range_cell <- function(concentration, detect_flag, prefix, ...) {
  n <- length(concentration)
  out <- historical_range(
    dplyr::tibble(
      date = as.POSIXct("2024-01-15", tz = "UTC") + (0:(n - 1)) * 86400 * 30,
      sampled_date_time = date,
      site_id = "S",
      location_code = "MW01",
      chem_name = "Copper",
      chem_code = "7440-50-8",
      prefix = prefix,
      detect_flag = detect_flag,
      concentration = concentration,
      output_unit = "mg/L"
    ),
    quiet = TRUE
  )
  gt::extract_body(create_gt(out, ...))$hist_min_prefix
}

test_that("one result reads as itself, not as a range across nothing", {
  skip_if_not_installed("gt")
  # The single historical result is both ends of the range.
  expect_equal(
    range_cell(c(5, 9), c("Y", "Y"), c(NA, NA)),
    "5"
  )
})

test_that("a range with no detected maximum drops its separator", {
  skip_if_not_installed("gt")
  # The maximum is the highest *detected* result, so a history of non-detects
  # has a minimum and no maximum. The cell must not trail a dash.
  expect_equal(
    range_cell(c(5, 9), c("N", "Y"), c("<", NA)),
    "&lt;5"
  )
})

test_that("no history at all is an empty cell, not a lone separator", {
  skip_if_not_installed("gt")
  expect_equal(range_cell(9, "Y", NA), "")
})

test_that("two different ends still read as a range", {
  skip_if_not_installed("gt")
  expect_equal(
    range_cell(c(2, 5, 9), rep("Y", 3), rep(NA, 3)),
    "2 - 5"
  )
})

test_that("a range is formatted as the columns beside it are", {
  skip_if_not_installed("gt")
  # gt::fmt_number()'s thousands separator and dropped trailing zeros, which
  # format_concentration() has to match for the table to read as one.
  expect_equal(
    range_cell(c(1000, 2500, 9), rep("Y", 3), rep(NA, 3)),
    "1,000 - 2,500"
  )
  expect_equal(format_concentration(c(5, 0.5, 1000, NA), 5),
               c("5", "0.5", "1,000", NA))
  expect_equal(format_concentration(0.000004, 5), "0")
})

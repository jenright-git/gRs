# Two analytes, one round, so the round machinery stays out of the way of the
# tests that are about the ranking itself. MW01 holds 1.5, 2.5 and <0.5;
# MW02 holds 4, 8 and <0.5.
ranked_fixture <- function(...) {
  out <- dplyr::bind_rows(
    chem_fixture(),
    chem_fixture(chem_name = "Zinc", chem_code = "7440-66-6")
  )
  out$monitoring_round <- "2024 Q1"
  args <- list(...)
  for (nm in names(args)) out[[nm]] <- args[[nm]]
  out
}


test_that("each analyte reports its highest and lowest result, and where", {
  out <- min_max_locations(ranked_fixture(), quiet = TRUE)

  expect_equal(nrow(out), 4)
  expect_equal(out$chem_name, c("Copper", "Copper", "Zinc", "Zinc"))
  expect_equal(out$extreme, rep(c("Maximum", "Minimum"), 2))
  expect_equal(out$rank, rep(1L, 4))
  expect_equal(out$concentration, c(8, 0.5, 8, 0.5))
  expect_equal(out$location_code, c("MW02", "MW01", "MW02", "MW01"))
})

test_that("n_max and n_min widen each end of the range", {
  out <- min_max_locations(ranked_fixture(), n_max = 2, n_min = 3, quiet = TRUE)
  copper <- out[out$chem_name == "Copper", ]

  expect_equal(nrow(out), 10)
  expect_equal(copper$extreme, c(rep("Maximum", 2), rep("Minimum", 3)))
  # the two non-detects are both 0.5, so they share the lowest rank
  expect_equal(copper$rank, c(1L, 2L, 1L, 1L, 2L))
  expect_equal(copper$concentration, c(8, 4, 0.5, 0.5, 1.5))
})

test_that("the maximum is the highest detection, not the highest result", {
  # the third result is a non-detect reported at an LOR above every detection
  out <- min_max_locations(
    ranked_fixture(concentration = rep(c(1.5, 2.5, 50, 4, 8, 0.5), 2)),
    quiet = TRUE
  )
  top <- out[out$extreme == "Maximum", ]

  expect_equal(top$concentration, c(8, 8))
  expect_equal(top$location_code, c("MW02", "MW02"))
  expect_equal(top$prefix, c(NA_character_, NA_character_))
})

test_that("an analyte never detected reports its highest result with the <", {
  out <- min_max_locations(
    ranked_fixture(
      detect_flag = rep("N", 12),
      prefix = rep("<", 12),
      concentration = rep(c(1, 2, 3, 4, 5, 6), 2)
    ),
    quiet = TRUE
  )
  top <- out[out$extreme == "Maximum", ]

  expect_equal(top$concentration, c(6, 6))
  expect_equal(top$prefix, c("<", "<"))
})

test_that("the minimum is the lowest result, detected or not", {
  out <- min_max_locations(ranked_fixture(), quiet = TRUE)
  bottom <- out[out$extreme == "Minimum", ]

  expect_equal(bottom$concentration, c(0.5, 0.5))
  expect_equal(bottom$prefix, c("<", "<"))
  expect_equal(bottom$detect_flag, c("N", "N"))
})

test_that("results of equal concentration share a rank", {
  out <- min_max_locations(ranked_fixture(), n_min = 2, quiet = TRUE)
  bottom <- out[out$extreme == "Minimum" & out$chem_name == "Copper", ]

  expect_equal(bottom$concentration, c(0.5, 0.5))
  expect_equal(bottom$rank, c(1L, 1L))
  # the tie is broken on location_code, so the table is the same every run
  expect_equal(bottom$location_code, c("MW01", "MW02"))
})

test_that("with_ties keeps the results tied with the last one taken", {
  data <- ranked_fixture(concentration = rep(c(1.5, 8, 0.5, 4, 8, 0.5), 2))

  expect_equal(nrow(min_max_locations(data, quiet = TRUE)), 4)
  expect_equal(nrow(min_max_locations(data, with_ties = TRUE, quiet = TRUE)), 8)

  tied <- min_max_locations(data, with_ties = TRUE, quiet = TRUE)
  copper <- tied[tied$chem_name == "Copper" & tied$extreme == "Maximum", ]
  expect_equal(copper$concentration, c(8, 8))
  expect_equal(copper$rank, c(1L, 1L))
})

test_that("an analyte reported in two units is not ranked across them", {
  # MW01 in mg/L, MW02 in ug/L, for both analytes
  out <- min_max_locations(
    ranked_fixture(output_unit = rep(rep(c("mg/L", "ug/L"), each = 3), 2)),
    quiet = TRUE
  )

  expect_equal(nrow(out), 8)
  expect_equal(out$output_unit, rep(c("mg/L", "mg/L", "ug/L", "ug/L"), 2))
  expect_equal(out$concentration, rep(c(2.5, 0.5, 8, 0.5), 2))
})

test_that("group_vars ranks within the group", {
  out <- min_max_locations(
    ranked_fixture(monitoring_zone = rep(rep(c("A", "B"), each = 3), 2)),
    group_vars = "monitoring_zone",
    quiet = TRUE
  )
  zone_a <- out[out$monitoring_zone == "A" & out$chem_name == "Copper", ]

  expect_equal(nrow(out), 8)
  expect_equal(zone_a$concentration, c(2.5, 0.5))
  expect_equal(zone_a$location_code, c("MW01", "MW01"))
})

test_that("the maximum agrees with analyte_summary()", {
  data <- ranked_fixture()
  top <- min_max_locations(data, quiet = TRUE)
  top <- top[top$extreme == "Maximum", ]
  summary <- analyte_summary(data, quiet = TRUE)

  expect_equal(top$concentration, summary$max_conc)
  expect_equal(top$prefix, summary$max_prefix)
  expect_equal(top$location_code, summary$max_location)
})

test_that("n_max = 0 and n_min = 0 report one end alone", {
  data <- ranked_fixture()

  expect_equal(
    unique(min_max_locations(data, n_max = 0, quiet = TRUE)$extreme),
    "Minimum"
  )
  expect_equal(
    unique(min_max_locations(data, n_min = 0, quiet = TRUE)$extreme),
    "Maximum"
  )
})

test_that("asking for more than the analyte holds returns what it has", {
  out <- min_max_locations(
    ranked_fixture(),
    n_max = 50,
    n_min = 50,
    quiet = TRUE
  )

  # per analyte: four detections at the top, all six results at the bottom
  expect_equal(nrow(out), 20)
  expect_equal(sum(out$extreme == "Maximum"), 8)
})

test_that("the round is resolved and attached", {
  data <- ranked_fixture()
  data$monitoring_round <- rep(rep(c("2024 Q1", "2024 Q2"), each = 3), 2)
  out <- min_max_locations(data, quiet = TRUE)

  expect_equal(attr(out, "round"), list(
    column = "monitoring_round",
    round = "2024 Q2"
  ))
  expect_equal(unique(out$monitoring_round), "2024 Q2")

  earlier <- min_max_locations(data, round = "2024 Q1", quiet = TRUE)
  expect_equal(unique(earlier$monitoring_round), "2024 Q1")
})

test_that("the result carries the columns it was given", {
  out <- min_max_locations(ranked_fixture(), quiet = TRUE)

  expect_true(all(names(ranked_fixture()) %in% names(out)))
  expect_equal(
    names(out)[1:5],
    c("chem_name", "extreme", "rank", "location_code", "date")
  )
})

test_that("NULL data returns NULL", {
  expect_null(min_max_locations(NULL))
})

test_that("the round found and the rows reported are named", {
  expect_snapshot(min_max_locations(ranked_fixture(), n_min = 2))
})

test_that("bad input is refused", {
  expect_snapshot(error = TRUE, {
    min_max_locations(ranked_fixture()["chem_name"], quiet = TRUE)
    min_max_locations(ranked_fixture(), n_max = 0, n_min = 0, quiet = TRUE)
    min_max_locations(ranked_fixture(), n_max = -1, quiet = TRUE)
    min_max_locations(ranked_fixture(), n_min = 1.5, quiet = TRUE)
    min_max_locations(ranked_fixture(), group_vars = "zone", quiet = TRUE)
  })
})

test_that("the result formats with create_gt()", {
  skip_if_not_installed("gt")

  tbl <- min_max_locations(ranked_fixture(), quiet = TRUE) %>%
    dplyr::select(
      chem_name,
      extreme,
      rank,
      location_code,
      prefix,
      concentration
    ) %>%
    create_gt()
  html <- as.character(gt::as_raw_html(tbl))

  expect_s3_class(tbl, "gt_tbl")
  expect_match(html, "Rank")
  expect_match(html, "Extreme")
  # a rank is a position, not a measurement, so it is not given decimals
  expect_no_match(html, ">1[.]00000<")
})

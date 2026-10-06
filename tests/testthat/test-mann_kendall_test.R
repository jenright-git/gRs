# One series, with detect_flag made from prefix as data_processor() makes it
# for an ESdat export, unless a flag is given.
trend_fixture <- function(
  conc = 1:8,
  prefix = NA_character_,
  detect_flag = NULL
) {
  prefix <- rep_len(prefix, length(conc))
  dplyr::tibble(
    location_code = "MW01",
    chem_name = "Copper",
    date = as.POSIXct("2024-01-15", tz = "UTC") +
      (seq_along(conc) - 1) * 8.64e6,
    concentration = as.numeric(conc),
    prefix = prefix,
    detect_flag = if (is.null(detect_flag)) {
      detect_flag_from_prefix(prefix)
    } else {
      rep_len(detect_flag, length(conc))
    }
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
    dplyr::mutate(
      trend_fixture(c(5, 5.1, 4.9, 5, 5.2, 4.8, 5, 5.1)),
      chem_name = "Zinc"
    )
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
    "No series with at least 4 samples"
  )
})

test_that("min_samples sets the shortest series tested", {
  data <- dplyr::bind_rows(
    trend_fixture(1:5),
    dplyr::mutate(trend_fixture(1:8), chem_name = "Zinc")
  )

  expect_equal(mann_kendall_test(data, min_samples = 6)$chem_name, "Zinc")
  expect_equal(nrow(mann_kendall_test(data, min_samples = 5)), 2)
  expect_error(
    mann_kendall_test(data, min_samples = 9),
    "No series with at least 9 samples"
  )
  # three is the fewest trend::mk.test() will take
  expect_equal(nrow(mann_kendall_test(trend_fixture(1:3), min_samples = 3)), 1)
})

test_that("min_samples must be a whole number of 3 or more", {
  for (bad in list(2, 3.5, "4", c(4, 5), NA_real_)) {
    expect_error(
      mann_kendall_test(trend_fixture(), min_samples = bad),
      "`min_samples` must be a single whole number of 3 or more"
    )
  }
})

test_that("missing concentrations are not counted towards min_samples", {
  data <- trend_fixture(c(1:4, NA, NA))

  expect_error(
    mann_kendall_test(data, min_samples = 5),
    "No series with at least 5 samples"
  )
  expect_equal(mann_kendall_test(data)$n_samples, 4L)
})

test_that("each trend carries the number of results it was tested on", {
  data <- dplyr::bind_rows(
    trend_fixture(1:5),
    dplyr::mutate(trend_fixture(1:8), chem_name = "Zinc")
  )
  out <- mann_kendall_test(data)

  expect_equal(out$n_samples, c(5L, 8L))
  expect_equal(out$n_samples, vapply(out$data, nrow, integer(1)))
  expect_equal(
    names(out)[match("trend", names(out)) + 0:4],
    c("trend", "n_samples", "lor_changed", "lor_min", "lor_max")
  )
})

test_that("mk_analysis() needs at least three results", {
  expect_error(
    mk_analysis(trend_fixture(c(1, 2, NA))),
    "needs at least 3 results with a concentration"
  )
  expect_no_error(mk_analysis(trend_fixture(1:3)))
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
    dplyr::mutate(
      trend_fixture(1:8, c(rep("<", 6), NA, NA)),
      chem_name = "Benzene"
    ),
    # 2 of 6 non-detect - retained
    dplyr::mutate(
      trend_fixture(1:6, c("<", "<", NA, NA, NA, NA)),
      chem_name = "Toluene"
    )
  )
  out <- suppressMessages(mann_kendall_test(data, nd_threshold = 0.5))

  expect_equal(out$chem_name, "Toluene")
})

test_that("min_detects keeps the combinations with enough detects", {
  data <- dplyr::bind_rows(
    dplyr::mutate(
      trend_fixture(1:8, c(rep("<", 6), NA, NA)),
      chem_name = "Benzene"
    ),
    dplyr::mutate(
      trend_fixture(1:6, c("<", "<", NA, NA, NA, NA)),
      chem_name = "Toluene"
    )
  )
  out <- suppressMessages(mann_kendall_test(data, min_detects = 3))

  expect_equal(out$chem_name, "Toluene")
})

test_that("each unit is tested as a separate series, and the split is said", {
  # the lab switched from mg/L to ug/L halfway: 1-5 mg/L then 6000-6004 ug/L
  data <- dplyr::mutate(
    trend_fixture(c(1:5, 6000:6004)),
    output_unit = rep(c("mg/L", "ug/L"), each = 5)
  )

  expect_message(
    out <- mann_kendall_test(data),
    "MW01 / Copper \\(mg/L, ug/L\\)"
  )
  expect_equal(nrow(out), 2)
  expect_equal(out$output_unit, c("mg/L", "ug/L"))
  expect_equal(vapply(out$data, nrow, integer(1)), c(5L, 5L))
  expect_equal(out$sample_mean, c(3, 6002))
})

test_that("a table in one unit is tested as before, with no message", {
  data <- dplyr::mutate(trend_fixture(), output_unit = "mg/L")

  expect_no_message(out <- mann_kendall_test(data))
  expect_equal(nrow(out), 1)
  expect_equal(out$output_unit, "mg/L")
})

test_that("exclusion messages name the unit where an analyte has two", {
  data <- dplyr::bind_rows(
    dplyr::mutate(trend_fixture(1:5, "<"), output_unit = "mg/L"),
    dplyr::mutate(trend_fixture(1:5), output_unit = "ug/L")
  )

  msgs <- capture_messages(mann_kendall_test(data, min_detects = 1))

  expect_match(
    paste(msgs, collapse = ""),
    "MW01 / Copper (mg/L) (0 detect(s) out of 5",
    fixed = TRUE
  )
})

test_that("unit_labelled() names the unit only where an analyte has two", {
  expect_equal(
    unit_labelled(
      c("Zinc", "Zinc", "Copper", "Copper"),
      c("mg/L", "ug/L", "mg/L", "mg/L")
    ),
    c("Zinc (mg/L)", "Zinc (ug/L)", "Copper", "Copper")
  )
  expect_equal(unit_labelled(c("Zinc", "Copper"), NULL), c("Zinc", "Copper"))
})

test_that("results with no unit are a series of their own, named as such", {
  # the first five results lost their unit
  data <- dplyr::mutate(
    trend_fixture(c(1:5, 6:10)),
    output_unit = rep(c(NA, "mg/L"), each = 5)
  )

  expect_message(
    out <- mann_kendall_test(data),
    "MW01 / Copper (mg/L, no unit)",
    fixed = TRUE
  )
  expect_equal(nrow(out), 2)
  expect_setequal(
    unit_labelled(out$chem_name, out$output_unit),
    c("Copper (mg/L)", "Copper (no unit)")
  )
  expect_equal(
    unit_labelled(c("Zinc", "Zinc"), c(NA, "mg/L")),
    c("Zinc (no unit)", "Zinc (mg/L)")
  )
  # an analyte whose only unit is missing keeps its plain name
  expect_equal(unit_labelled(c("Zinc", "Zinc"), c(NA, NA)), c("Zinc", "Zinc"))
})

test_that("the heatmaps give each unit of an analyte its own row", {
  data <- dplyr::mutate(
    trend_fixture(c(1:5, 6000:6004)),
    output_unit = rep(c("mg/L", "ug/L"), each = 5)
  )
  trends <- suppressMessages(mann_kendall_test(data))
  trad <- suppressMessages(mann_kendall_test(data, traditional = TRUE))

  for (p in list(mann_kendall_heatmap(trends), mann_kendall_heatmap_bw(trad))) {
    expect_no_error(ggplot2::ggplot_build(p))
    expect_equal(p$data$chem_name, c("Copper (mg/L)", "Copper (ug/L)"))
  }
})

test_that("only a Y detect_flag counts as a detect", {
  # one N, and a missing flag, which is not a detect either
  data <- trend_fixture(1:8, detect_flag = c("N", NA, rep("Y", 6)))

  expect_equal(
    nrow(suppressMessages(mann_kendall_test(data, min_detects = 6))),
    1
  )
  expect_error(
    suppressMessages(mann_kendall_test(data, min_detects = 7)),
    "No data remaining after applying min_detects"
  )
})

test_that("detects are read from detect_flag, not prefix", {
  # every prefix says detected; the flags say otherwise
  data <- trend_fixture(rep(4, 8), prefix = NA, detect_flag = "N")

  expect_equal(mann_kendall_test(data, lor_multiplier = 0.5)$sample_mean, 2)
  expect_error(
    suppressMessages(mann_kendall_test(data, nd_threshold = 0.5)),
    "No data remaining after applying nd_threshold"
  )
  expect_error(
    suppressMessages(mann_kendall_test(data, min_detects = 1)),
    "No data remaining after applying min_detects"
  )

  # and the other way round: a "<" flagged as detected is not substituted
  data <- trend_fixture(rep(4, 8), prefix = "<", detect_flag = "Y")
  expect_equal(mann_kendall_test(data, lor_multiplier = 0.5)$sample_mean, 4)
  expect_equal(mk_analysis(data, lor_multiplier = 0.5)$sample_mean, 4)
})

test_that("a table with no detect_flag is an error", {
  data <- trend_fixture()
  data$detect_flag <- NULL

  expect_error(mann_kendall_test(data), "has no `detect_flag` column")
  expect_error(mk_analysis(data), "has no `detect_flag` column")
  # mk_analysis() reads the flag only to substitute
  expect_no_error(mk_analysis(data, lor_multiplier = NULL))
})

test_that("non-detects at more than one LOR are flagged", {
  # the lab dropped its LOR from 0.01 to 0.001 after four rounds
  data <- trend_fixture(rep(c(0.01, 0.001), each = 4), prefix = "<")
  out <- mann_kendall_test(data, lor_multiplier = 0.5)

  expect_true(out$lor_changed)
  # as reported, before the multiplier
  expect_equal(out$lor_min, 0.001)
  expect_equal(out$lor_max, 0.01)
})

test_that("non-detects at a single LOR are not flagged", {
  data <- trend_fixture(
    c(0.01, 0.01, 0.05, 0.08, 0.01, 0.2),
    prefix = c("<", "<", NA, NA, "<", NA)
  )
  out <- mann_kendall_test(data)

  expect_false(out$lor_changed)
  expect_equal(c(out$lor_min, out$lor_max), c(0.01, 0.01))
})

test_that("an LOR read back with floating-point noise is still one LOR", {
  data <- trend_fixture(
    c(0.001, 0.0010000000000000002, 0.001, 0.001, 0.5),
    prefix = c("<", "<", "<", "<", NA)
  )

  expect_false(mann_kendall_test(data)$lor_changed)
})

test_that("a series with no non-detects has no LOR to flag", {
  out <- mann_kendall_test(trend_fixture())

  expect_false(out$lor_changed)
  expect_true(is.na(out$lor_min))
  expect_true(is.na(out$lor_max))
})

test_that("LORs are read back from a table half_lor() substituted", {
  data <- trend_fixture(rep(c(0.01, 0.001), each = 4), prefix = "<")
  out <- mann_kendall_test(half_lor(data, lor_multiplier = 0.5))

  expect_equal(out$lor_changed, TRUE)
  expect_equal(c(out$lor_min, out$lor_max), c(0.001, 0.01))
})

test_that("zero substitution beforehand leaves no LOR to flag", {
  data <- trend_fixture(rep(c(0.01, 0.001), each = 4), prefix = "<")
  expect_snapshot(out <- mann_kendall_test(half_lor(data, lor_multiplier = 0)))

  expect_equal(out$lor_changed, NA)
  expect_equal(c(out$lor_min, out$lor_max), c(NA_real_, NA_real_))
})

test_that("only non-detects are read for the LOR", {
  # detects at many values, non-detects all at 0.5
  data <- trend_fixture(
    c(0.5, 0.5, 1, 2, 3, 0.5, 4),
    prefix = c("<", "<", NA, NA, NA, "<", NA)
  )

  expect_false(mann_kendall_test(data)$lor_changed)
})

test_that("the lor_multiplier used is recorded on the result", {
  expect_equal(attr(mann_kendall_test(trend_fixture()), "lor_multiplier"), 1)

  out <- mann_kendall_test(trend_fixture(), lor_multiplier = 0.5)
  expect_equal(attr(out, "lor_multiplier"), 0.5)
  # the nested data is left as reported
  expect_equal(out$data[[1]]$concentration, as.numeric(1:8))
})

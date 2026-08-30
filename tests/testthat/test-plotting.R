test_that("timeseries_plot builds for one and for many analytes", {
  expect_no_error(ggplot2::ggplot_build(
    timeseries_plot(chem_fixture())
  ))
  expect_no_error(ggplot2::ggplot_build(
    timeseries_plot(gRs_data, filter_analyte = unique(gRs_data$chem_name)[1:3])
  ))
})

test_that("facet_by = 'location' colours by analyte instead", {
  expect_no_error(ggplot2::ggplot_build(
    timeseries_plot(
      gRs_data,
      filter_analyte = unique(gRs_data$chem_name)[1:2],
      facet_by = "location"
    )
  ))
})

test_that("a missing column is named rather than failing inside ggplot2", {
  data <- chem_fixture()
  data$concentration <- NULL

  expect_error(timeseries_plot(data), "Missing required columns: concentration")
})

test_that("filtering to nothing is an error, not an empty plot", {
  expect_error(
    suppressWarnings(timeseries_plot(chem_fixture(), filter_location = "MW99")),
    "No data remaining after filtering"
  )
})

test_that("the lowest criteria value is drawn, and the rest warned about", {
  data <- chem_fixture(criteria = c(1, 1, 1, 1000, 1000, 1000))

  expect_warning(
    p <- timeseries_plot(data, criteria_col = criteria),
    "Using the lowest: 1"
  )
  hlines <- Filter(
    function(l) inherits(l$geom, "GeomHline"),
    p$layers
  )
  expect_equal(hlines[[1]]$data$yintercept, 1)
})

test_that("a criteria column that is not there is skipped, not fatal", {
  expect_warning(
    p <- timeseries_plot(chem_fixture(), criteria_col = not_a_column),
    "Skipping criteria line"
  )
  expect_s3_class(p, "ggplot")
})

test_that("scale_x_limitval draws vertical lines", {
  p <- ggplot2::ggplot(chem_fixture(), ggplot2::aes(concentration, chem_name)) +
    ggplot2::geom_point() +
    scale_x_limitval(2)

  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("scale_y_limitval draws horizontal lines", {
  p <- ggplot2::ggplot(chem_fixture(), ggplot2::aes(date, concentration)) +
    ggplot2::geom_point() +
    scale_y_limitval(c(2, 5))

  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("get_plotting_variables returns the whole set", {
  vars <- get_plotting_variables(chem_fixture())

  expect_equal(vars$locations_vec, c("MW01", "MW02"))
  expect_equal(vars$n_analytes, 1)
  expect_named(vars$location_colours, c("MW01", "MW02"))
})

test_that("picking colours leaves the caller's random stream alone", {
  set.seed(42)
  before <- runif(1)
  set.seed(42)
  invisible(gRs_colours(letters[1:5]))
  after <- runif(1)

  expect_identical(before, after)
})

test_that("plot_by_analyte passes its dots through to timeseries_plot", {
  # Every plot used to error here: purrr passed the analyte and zone into
  # timeseries_plot() in place of these arguments.
  out <- suppressMessages(plot_by_analyte(
    chem_fixture(),
    show_progress = FALSE,
    ymax = 100,
    date_break = "3 months"
  ))

  expect_equal(out$status, "not_saved")
})

test_that("plot_by_analyte writes into a directory per zone", {
  dir <- withr::local_tempdir()
  out <- suppressMessages(plot_by_analyte(
    chem_fixture(),
    save_path = dir,
    show_progress = FALSE
  ))

  expect_equal(out$status, "saved")
  expect_equal(list.files(dir, recursive = TRUE), "Zone A/Copper-plot.png")
})

test_that("create_dirs = FALSE writes flat, with the zone in the name", {
  dir <- withr::local_tempdir()
  out <- suppressMessages(plot_by_analyte(
    chem_fixture(),
    save_path = dir,
    create_dirs = FALSE,
    show_progress = FALSE
  ))

  expect_equal(out$status, "saved")
  expect_equal(list.files(dir), "Zone A-Copper-plot.png")
})

test_that("an analyte name that is not a legal filename is still written", {
  dir <- withr::local_tempdir()
  out <- suppressMessages(plot_by_analyte(
    chem_fixture(chem_name = "Nitrate/Nitrite as N"),
    save_path = dir,
    show_progress = FALSE
  ))

  expect_equal(out$status, "saved")
  expect_equal(
    list.files(dir, recursive = TRUE),
    "Zone A/Nitrate-Nitrite as N-plot.png"
  )
})

test_that("safe_filename replaces rather than drops", {
  expect_equal(safe_filename("a/b:c*d?e\"f<g>h|i"), "a-b-c-d-e-f-g-h-i")
  expect_equal(safe_filename("trailing dot."), "trailing dot")
  expect_equal(safe_filename("///"), "unnamed")
})

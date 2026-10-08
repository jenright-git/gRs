mka_fixture <- function() {
  dplyr::tibble(
    location_code = c("MW02", "MW02", "MW01", "MW01"),
    chem_name = c("Zinc", "Copper", "Zinc", "Copper"),
    trend = c("Increasing", "Stable", "Decreasing", "No Significant Trend"),
    p_value = c(0.01, 0.5, 0.02, 0.6)
  )
}

test_that("the export pivots analytes across and trends into the cells", {
  out <- mka_to_excel(
    mka_fixture(),
    save_path = withr::local_tempfile(
      fileext = ".xlsx"
    )
  )

  expect_named(out, c("Monitoring Well", "Copper", "Zinc"))
  expect_equal(out[["Monitoring Well"]], c("MW01", "MW02"))
  expect_equal(out$Zinc, c("Decreasing", "Increasing"))
  expect_equal(out$Copper, c("No Significant Trend", "Stable"))
})

test_that("the workbook is written with a summary and a legend sheet", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(mka_fixture(), save_path = path)

  expect_true(file.exists(path))
  expect_equal(openxlsx::getSheetNames(path), c("Trend Summary", "Legend"))
})

test_that("legend = FALSE drops the second sheet", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(mka_fixture(), save_path = path, legend = FALSE)

  expect_equal(openxlsx::getSheetNames(path), "Trend Summary")
})

test_that("untested location/analyte pairs are labelled, not left blank", {
  data <- mka_fixture()[1:3, ]
  out <- mka_to_excel(
    data,
    save_path = withr::local_tempfile(
      fileext = ".xlsx"
    )
  )

  expect_equal(out$Copper, c("NC", "Stable"))

  out <- mka_to_excel(
    data,
    save_path = withr::local_tempfile(fileext = ".xlsx"),
    na_label = "not tested"
  )
  expect_equal(out$Copper, c("not tested", "Stable"))
})


test_that("the directory is created when it does not exist", {
  dir <- file.path(withr::local_tempdir(), "nested", "output")
  path <- file.path(dir, "trends.xlsx")

  expect_message(
    mka_to_excel(mka_fixture(), save_path = path),
    "Created directory"
  )
  expect_true(file.exists(path))
})

test_that("named colours override only the categories they name", {
  fills <- resolve_trend_colours(
    c(Increasing = "#000000"),
    TREND_FILL_DEFAULT,
    "trend_colours"
  )

  expect_equal(fills[["Increasing"]], "#000000")
  expect_equal(fills[["Decreasing"]], TREND_FILL_DEFAULT[["Decreasing"]])
  expect_named(fills, names(TREND_FILL_DEFAULT))
})

test_that("colours can be given as a list", {
  fills <- resolve_trend_colours(
    list(Increasing = "#000000", Stable = "#111111"),
    TREND_FILL_DEFAULT,
    "trend_colours"
  )

  expect_equal(
    unname(fills[c("Increasing", "Stable")]),
    c("#000000", "#111111")
  )
})

test_that("an unnamed vector must be a complete set, in level order", {
  full <- rep("#123456", length(TREND_FILL_DEFAULT))
  fills <- resolve_trend_colours(full, TREND_FILL_DEFAULT, "trend_colours")

  expect_named(fills, names(TREND_FILL_DEFAULT))
  expect_true(all(fills == "#123456"))

  expect_error(
    resolve_trend_colours(c("#123456"), TREND_FILL_DEFAULT, "trend_colours"),
    "unnamed"
  )
})

test_that("a trend value with no colour warns rather than failing", {
  data <- dplyr::mutate(mka_fixture(), trend = "Wobbling")

  expect_warning(
    mka_to_excel(data, save_path = withr::local_tempfile(fileext = ".xlsx")),
    "Wobbling"
  )
})

test_that("repeated location/analyte pairs are an error", {
  data <- dplyr::bind_rows(mka_fixture(), mka_fixture()[1, ])

  expect_error(
    mka_to_excel(data, save_path = withr::local_tempfile(fileext = ".xlsx")),
    "MW02 / Zinc"
  )
})

test_that("missing columns are named in the error", {
  data <- dplyr::select(mka_fixture(), -trend)

  expect_error(
    mka_to_excel(data, save_path = withr::local_tempfile(fileext = ".xlsx")),
    "trend"
  )
})

test_that("the trend columns can be named", {
  data <- dplyr::rename(
    mka_fixture(),
    well = location_code,
    analyte = chem_name,
    direction = trend
  )

  out <- mka_to_excel(
    data,
    save_path = withr::local_tempfile(fileext = ".xlsx"),
    location_col = well,
    chem_name_col = analyte,
    trend_col = direction
  )

  expect_named(out, c("Monitoring Well", "Copper", "Zinc"))
})

test_that("the headings and location names are white on the header fill", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(mka_fixture(), save_path = path)

  styles <- openxlsx::loadWorkbook(path)$styleObjects
  header <- Filter(
    function(s) identical(s$style$fill$fillFg[[1]], "FF008768"),
    styles
  )

  # the header row across the top, and the location names down the side
  expect_length(header, 3)
  expect_true(all(vapply(
    header,
    function(s) identical(s$style$fontColour[["rgb"]], "FFFFFFFF"),
    logical(1)
  )))
})

test_that("the header colours can be changed", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(
    mka_fixture(),
    save_path = path,
    header_fill = "#000080",
    header_font = "#FFFF00"
  )

  styles <- openxlsx::loadWorkbook(path)$styleObjects
  fills <- vapply(
    styles,
    function(s) s$style$fill$fillFg[[1]] %||% NA_character_,
    character(1)
  )

  expect_true(any(fills == "FF000080", na.rm = TRUE))
  expect_false(any(fills == "FF008768", na.rm = TRUE))
})

mka_nested_fixture <- function(zones = c("Lower", "Lower", "Upper", "Upper")) {
  data <- mka_fixture()
  data$data <- lapply(zones, function(z) {
    dplyr::tibble(monitoring_zone = z, concentration = 1:4)
  })
  data
}

test_that("include_zone adds the zone from the nested data, sorted by zone", {
  out <- mka_to_excel(
    mka_nested_fixture(),
    save_path = withr::local_tempfile(fileext = ".xlsx"),
    include_zone = TRUE
  )

  expect_named(out, c("Monitoring Zone", "Monitoring Well", "Copper", "Zinc"))
  expect_equal(out[["Monitoring Zone"]], c("Lower", "Upper"))
  expect_equal(out[["Monitoring Well"]], c("MW02", "MW01"))
  expect_equal(out$Zinc, c("Increasing", "Decreasing"))
})

test_that("a zone column on the data itself is used as it stands", {
  data <- mka_fixture()
  data$monitoring_zone <- c("Lower", "Lower", "Upper", "Upper")

  out <- mka_to_excel(
    data,
    save_path = withr::local_tempfile(fileext = ".xlsx"),
    include_zone = TRUE,
    zone_label = "Zone"
  )

  expect_named(out, c("Zone", "Monitoring Well", "Copper", "Zinc"))
  expect_equal(out$Zone, c("Lower", "Upper"))
})

test_that("the zone column can be named", {
  data <- mka_fixture()
  data$aquifer <- c("Lower", "Lower", "Upper", "Upper")

  out <- mka_to_excel(
    data,
    save_path = withr::local_tempfile(fileext = ".xlsx"),
    include_zone = TRUE,
    zone_col = aquifer
  )

  expect_equal(out[["Monitoring Zone"]], c("Lower", "Upper"))
})

test_that("include_zone with no zone anywhere is an error", {
  expect_error(
    mka_to_excel(
      mka_fixture(),
      save_path = withr::local_tempfile(fileext = ".xlsx"),
      include_zone = TRUE
    ),
    "monitoring_zone"
  )
})

test_that("a location in two zones is an error, not two rows", {
  expect_error(
    mka_to_excel(
      mka_nested_fixture(c("Lower", "Upper", "Upper", "Upper")),
      save_path = withr::local_tempfile(fileext = ".xlsx"),
      include_zone = TRUE
    ),
    "MW02"
  )
})

test_that("a location with no zone recorded is left blank", {
  data <- mka_fixture()
  data$monitoring_zone <- c(NA, NA, "Upper", "Upper")

  out <- mka_to_excel(
    data,
    save_path = withr::local_tempfile(fileext = ".xlsx"),
    include_zone = TRUE
  )

  expect_equal(out[["Monitoring Zone"]], c("", "Upper"))
})

test_that("repeated zone cells are merged only with merge_cells = TRUE", {
  zones <- c("Lower", "Lower", "Lower", "Lower")

  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(
    mka_nested_fixture(zones),
    save_path = path,
    include_zone = TRUE,
    merge_cells = TRUE
  )
  expect_match(
    openxlsx::loadWorkbook(path)$worksheets[[1]]$mergeCells,
    "A2:A3",
    fixed = TRUE
  )

  # by default every row keeps its zone, so the sheet sorts and filters
  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(
    mka_nested_fixture(zones),
    save_path = path,
    include_zone = TRUE
  )
  expect_length(openxlsx::loadWorkbook(path)$worksheets[[1]]$mergeCells, 0)
})

test_that("merge_cells must be TRUE or FALSE", {
  expect_error(
    mka_to_excel(
      mka_nested_fixture(),
      save_path = withr::local_tempfile(fileext = ".xlsx"),
      merge_cells = NA
    ),
    "`merge_cells` must be TRUE or FALSE."
  )
})

test_that("the trend cells shift right to make room for the zone", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(
    mka_nested_fixture(),
    save_path = path,
    include_zone = TRUE,
    legend = FALSE
  )

  styles <- openxlsx::loadWorkbook(path)$styleObjects
  decreasing <- Filter(
    function(s) identical(s$style$fill$fillFg[[1]], "FF63BE7B"),
    styles
  )

  # MW01 / Zinc: the second data row, and the last of four columns
  expect_length(decreasing, 1)
  expect_equal(decreasing[[1]]$rows, 3)
  expect_equal(decreasing[[1]]$cols, 4)
})

cells_filled <- function(path, fill, sheet = NULL) {
  styles <- openxlsx::loadWorkbook(path)$styleObjects
  hits <- Filter(
    function(s) {
      identical(s$style$fill$fillFg[[1]], fill) &&
        (is.null(sheet) || identical(s$sheet, sheet))
    },
    styles
  )

  unique(do.call(
    rbind,
    lapply(hits, function(s) {
      cbind(row = s$rows, col = s$cols)
    })
  ))
}

test_that("the well names are set a shade back from the zone beside them", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(
    mka_nested_fixture(),
    save_path = path,
    include_zone = TRUE,
    legend = FALSE
  )

  # the whole header row, and the zone column below it, stay the deep green
  green <- cells_filled(path, "FF008768")
  expect_equal(sort(unique(green[green[, "row"] == 1, "col"])), 1:4)
  expect_equal(sort(unique(green[green[, "col"] == 1, "row"])), 1:3)

  # only the well names take the sage, never their heading
  sage <- cells_filled(path, "FF9BBEAF")
  expect_equal(sort(unique(sage[, "col"])), 2L)
  expect_equal(sort(unique(sage[, "row"])), 2:3)
})

test_that("without a zone the well column keeps the original green", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(mka_fixture(), save_path = path, legend = FALSE)

  green <- cells_filled(path, "FF008768")
  expect_equal(sort(unique(green[green[, "col"] == 1, "row"])), 1:3)
  expect_null(cells_filled(path, "FF9BBEAF"))
})

test_that("the well column colours can be changed", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(
    mka_nested_fixture(),
    save_path = path,
    include_zone = TRUE,
    legend = FALSE,
    location_fill = "#000080",
    location_font = "#FFFF00"
  )

  styles <- openxlsx::loadWorkbook(path)$styleObjects
  navy <- Filter(
    function(s) identical(s$style$fill$fillFg[[1]], "FF000080"),
    styles
  )

  expect_true(length(navy) > 0)
  expect_true(all(vapply(
    navy,
    function(s) identical(s$style$fontColour[["rgb"]], "FFFFFF00"),
    logical(1)
  )))
  expect_null(cells_filled(path, "FF9BBEAF"))

  # the heading above them is part of the header row, and is not recoloured
  navy_cells <- cells_filled(path, "FF000080")
  expect_false(any(navy_cells[, "row"] == 1))
})

test_that("include_zone must be a single TRUE or FALSE", {
  expect_error(
    mka_to_excel(
      mka_fixture(),
      save_path = withr::local_tempfile(fileext = ".xlsx"),
      include_zone = "yes"
    ),
    "TRUE or FALSE"
  )
})

mka_stats_fixture <- function() {
  data <- mka_nested_fixture()
  data$data[[3]] <- dplyr::tibble(
    monitoring_zone = "Upper",
    concentration = 1:6
  )
  data$p_value <- c(0.0123456, 0.5, 0.02, 0.6)
  data$tau_statistic <- stats::setNames(c(0.8, 0, -0.7, 0.1), rep("tau", 4))
  data$S_statistic <- c(5, 0, -9, 1)
  data$sample_mean <- c(1.5, 2, 0.00042, 4)
  data$SD <- c(0.5, 0, 0.0001, 6)
  data$COV <- data$SD / data$sample_mean
  data
}

read_stats_sheet <- function(path) {
  openxlsx::read.xlsx(path, sheet = "Statistics", sep.names = " ")
}

test_that("include_stats adds a statistics sheet ahead of the legend", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(mka_stats_fixture(), save_path = path, include_stats = TRUE)

  expect_equal(
    openxlsx::getSheetNames(path),
    c("Trend Summary", "Statistics", "Legend")
  )
})

test_that("the statistics sheet has a row per pair, sorted as the summary", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(mka_stats_fixture(), save_path = path, include_stats = TRUE)
  stats <- read_stats_sheet(path)

  expect_named(
    stats,
    c(
      "Monitoring Well",
      "Analyte",
      "Trend",
      "Samples",
      "S Statistic",
      "Kendall's Tau",
      "p-value",
      "Mean",
      "Standard Deviation",
      "Coefficient of Variation"
    )
  )
  expect_equal(stats[["Monitoring Well"]], c("MW01", "MW01", "MW02", "MW02"))
  expect_equal(stats$Analyte, c("Copper", "Zinc", "Copper", "Zinc"))
  expect_equal(
    stats$Trend,
    c("No Significant Trend", "Decreasing", "Stable", "Increasing")
  )
  # counted from the nested data, which holds six samples for MW01 / Zinc
  expect_equal(stats$Samples, c(4, 6, 4, 4))
  expect_equal(stats[["Kendall's Tau"]], c(0.1, -0.7, 0, 0.8))
})

test_that("statistics are written unrounded", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(mka_stats_fixture(), save_path = path, include_stats = TRUE)
  stats <- read_stats_sheet(path)

  expect_equal(stats[["p-value"]][4], 0.0123456)
  expect_equal(stats$Mean[2], 0.00042)
})

test_that("statistics the data does not carry are left out", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(mka_fixture(), save_path = path, include_stats = TRUE)

  # no nested data to count samples from, and only a p-value
  expect_named(
    read_stats_sheet(path),
    c("Monitoring Well", "Analyte", "Trend", "p-value")
  )
})

test_that("include_stats with no statistics at all is an error", {
  data <- dplyr::select(mka_fixture(), -p_value)
  path <- withr::local_tempfile(fileext = ".xlsx")

  expect_error(
    mka_to_excel(data, save_path = path, include_stats = TRUE),
    "none of the statistics"
  )
  expect_false(file.exists(path))
})

test_that("the statistics trend cells are coloured as the summary's", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(
    mka_stats_fixture(),
    save_path = path,
    include_stats = TRUE,
    trend_colours = c(Increasing = "#E06666")
  )

  # MW01 / Zinc is the second row, and the trend the third column
  decreasing <- cells_filled(path, "FF63BE7B", sheet = "Statistics")
  expect_equal(unname(decreasing[, "row"]), 3)
  expect_equal(unname(decreasing[, "col"]), 3)

  # a recoloured category is recoloured on both sheets
  increasing <- cells_filled(path, "FFE06666", sheet = "Statistics")
  expect_equal(unname(increasing[, "row"]), 5)
  expect_true(!is.null(cells_filled(path, "FFE06666", sheet = "Trend Summary")))

  styles <- openxlsx::loadWorkbook(path)$styleObjects
  on_sheet <- function(sheet) {
    Filter(
      function(s) {
        identical(s$sheet, sheet) &&
          identical(s$style$fill$fillFg[[1]], "FF63BE7B")
      },
      styles
    )[[1]]$style
  }
  expect_equal(on_sheet("Statistics"), on_sheet("Trend Summary"))
})

test_that("a pair with no trend reads as na_label on the statistics sheet", {
  data <- mka_stats_fixture()
  data$trend[1] <- NA

  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(
    data,
    save_path = path,
    include_stats = TRUE,
    na_label = "not tested"
  )

  expect_equal(read_stats_sheet(path)$Trend[4], "not tested")
  nc <- cells_filled(path, "FFFFFFFF", sheet = "Statistics")
  expect_true(any(nc[, "row"] == 5 & nc[, "col"] == 3))
})

test_that("na_label can be blank", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  out <- mka_to_excel(mka_fixture()[1:3, ], save_path = path, na_label = "")

  expect_equal(out$Copper, c("", "Stable"))

  # MW01 / Copper, still styled as not calculated, and still explained
  nc <- cells_filled(path, "FFFFFFFF", sheet = "Trend Summary")
  expect_true(any(nc[, "row"] == 2 & nc[, "col"] == 2))
  legend <- openxlsx::read.xlsx(path, sheet = "Legend")
  expect_match(legend$Meaning[nrow(legend)], "^Not calculated")
})

test_that("the statistics sheet carries the zone, merged when asked", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(
    mka_stats_fixture(),
    save_path = path,
    include_stats = TRUE,
    include_zone = TRUE,
    merge_cells = TRUE
  )

  stats <- read_stats_sheet(path)
  expect_equal(
    names(stats)[1:3],
    c("Monitoring Zone", "Monitoring Well", "Analyte")
  )
  expect_equal(stats[["Monitoring Well"]], c("MW02", "MW02", "MW01", "MW01"))

  # each zone, and each well over its analytes within it
  wb <- openxlsx::loadWorkbook(path)
  expect_equal(
    wb$worksheets[[2]]$mergeCells,
    c(
      '<mergeCell ref="A2:A3"/>', '<mergeCell ref="A4:A5"/>',
      '<mergeCell ref="B2:B3"/>', '<mergeCell ref="B4:B5"/>'
    )
  )

  # the well names take the sage, as they do beside the zone on the summary
  sage <- cells_filled(path, "FF9BBEAF", sheet = "Statistics")
  expect_equal(sort(unique(sage[, "col"])), 2L)

  # by default nothing merges
  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(
    mka_stats_fixture(),
    save_path = path,
    include_stats = TRUE,
    include_zone = TRUE
  )
  expect_length(openxlsx::loadWorkbook(path)$worksheets[[2]]$mergeCells, 0)

  # without a zone, the summary has a row per well and nothing to merge; the
  # statistics merge each well over its analytes
  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(
    mka_stats_fixture(),
    save_path = path,
    include_stats = TRUE,
    merge_cells = TRUE
  )
  wb <- openxlsx::loadWorkbook(path)
  expect_length(wb$worksheets[[1]]$mergeCells, 0)
  expect_equal(
    wb$worksheets[[2]]$mergeCells,
    c('<mergeCell ref="A2:A3"/>', '<mergeCell ref="A4:A5"/>')
  )
})

test_that("include_stats must be a single TRUE or FALSE", {
  expect_error(
    mka_to_excel(
      mka_fixture(),
      save_path = withr::local_tempfile(fileext = ".xlsx"),
      include_stats = NA
    ),
    "TRUE or FALSE"
  )
})

test_that("the summary sheet cannot take the statistics sheet's name", {
  expect_error(
    mka_to_excel(
      mka_fixture(),
      save_path = withr::local_tempfile(fileext = ".xlsx"),
      include_stats = TRUE,
      sheet_name = "statistics"
    ),
    "statistics sheet takes that name"
  )
})

test_that("the summary sheet cannot take the legend sheet's name", {
  expect_error(
    mka_to_excel(
      mka_fixture(),
      save_path = withr::local_tempfile(fileext = ".xlsx"),
      sheet_name = "legend"
    ),
    "legend sheet takes that name"
  )

  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(
    mka_fixture(),
    save_path = path,
    sheet_name = "Legend",
    legend = FALSE
  )
  expect_equal(openxlsx::getSheetNames(path), "Legend")
})

test_that("a statistic the test could not compute is written as a dash", {
  data <- mka_stats_fixture()
  # MW02 / Copper: every value tested the same, so mk.test() gives NaN
  data$tau_statistic[2] <- NaN
  data$p_value[2] <- NaN
  data$COV[2] <- Inf

  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(data, save_path = path, include_stats = TRUE)
  stats <- read_stats_sheet(path)

  # the third row once sorted
  expect_equal(stats[["Kendall's Tau"]][3], "-")
  expect_equal(stats[["p-value"]][3], "-")
  expect_equal(stats[["Coefficient of Variation"]][3], "-")
  expect_equal(as.numeric(stats[["p-value"]][-3]), c(0.6, 0.02, 0.0123456))

  # and nothing went in as an Excel error such as #NUM!
  xml_dir <- withr::local_tempdir()
  utils::unzip(path, "xl/worksheets/sheet2.xml", exdir = xml_dir)
  xml <- readLines(file.path(xml_dir, "xl/worksheets/sheet2.xml"), warn = FALSE)
  expect_false(any(grepl('t="e"', xml, fixed = TRUE)))
})

# Sorted, the rows run MW01 / Copper, MW01 / Zinc, MW02 / Copper, MW02 / Zinc.
mka_summary_fixture <- function() {
  data <- mka_stats_fixture()
  data$data <- list(
    # MW02 / Zinc: all detects
    dplyr::tibble(concentration = c(1, 2, 3, 4), detect_flag = "Y"),
    # MW02 / Copper: all non-detects at one LOR
    dplyr::tibble(concentration = rep(0.001, 4), detect_flag = "N"),
    # MW01 / Zinc: a non-detect at the lowest value, a detect at the highest
    dplyr::tibble(
      concentration = c(0.001, 0.001, 0.002, 0.005, 0.005, 0.003),
      detect_flag = c("Y", "N", "Y", "N", "Y", "Y")
    ),
    # MW01 / Copper: a non-detect LOR above every detect
    dplyr::tibble(
      concentration = c(0.002, 0.003, 0.01, 0.01),
      detect_flag = c("Y", "Y", "N", "N")
    )
  )
  data
}

write_summary_fixture <- function(data = mka_summary_fixture(), ...) {
  path <- withr::local_tempfile(
    fileext = ".xlsx",
    .local_envir = parent.frame()
  )
  mka_to_excel(
    data,
    save_path = path,
    include_stats = TRUE,
    include_summary = TRUE,
    ...
  )
  path
}

test_that("include_summary adds the summary_stats() columns after the test's", {
  stats <- read_stats_sheet(write_summary_fixture())

  expect_equal(
    names(stats),
    c(
      "Monitoring Well",
      "Analyte",
      "Trend",
      "Samples",
      "S Statistic",
      "Kendall's Tau",
      "p-value",
      "Mean (as tested)",
      "Standard Deviation (as tested)",
      "Coefficient of Variation",
      "Detects",
      "Non-Detects",
      "% Detects",
      "% Non-Detects",
      "Minimum",
      "Mean (as reported)",
      "Maximum",
      "Standard Deviation (as reported)",
      paste0(PERCENTILES, "th Percentile")
    )
  )
  expect_equal(stats$Samples, c(4, 6, 4, 4))
  expect_equal(stats$Detects, c(2, 4, 0, 4))
  expect_equal(stats[["Non-Detects"]], c(2, 2, 4, 0))
  expect_equal(stats[["% Detects"]], c(50, 66.7, 0, 100))
})

test_that("the summary describes the nested series, as summary_stats() would", {
  data <- mka_summary_fixture()
  stats <- read_stats_sheet(write_summary_fixture(data))

  zinc <- summary_stats(dplyr::mutate(
    data$data[[1]],
    location_code = "MW02",
    chem_name = "Zinc"
  ))
  expect_equal(stats[["Mean (as reported)"]][4], zinc$mean)
  expect_equal(stats[["95th Percentile"]][4], zinc$p95)
})

test_that("an analyte tested in two units gets a column for each", {
  # MW01 Zinc changed units partway through; MW02 Zinc did not
  data <- dplyr::tibble(
    location_code = rep(c("MW01", "MW02"), c(10, 5)),
    chem_name = "Zinc",
    date = as.POSIXct("2024-01-15", tz = "UTC") + c(0:9, 0:4) * 8.64e6,
    concentration = c(1:5, 6000:6004, 1:5),
    prefix = NA_character_,
    detect_flag = "Y",
    output_unit = rep(c("mg/L", "ug/L", "mg/L"), each = 5)
  )
  trends <- suppressMessages(mann_kendall_test(data))
  path <- withr::local_tempfile(fileext = ".xlsx")
  out <- suppressMessages(mka_to_excel(
    trends,
    save_path = path,
    include_stats = TRUE,
    include_summary = TRUE
  ))

  expect_named(out, c("Monitoring Well", "Zinc (mg/L)", "Zinc (ug/L)"))
  expect_equal(out[["Zinc (ug/L)"]], c("Increasing", "NC"))

  stats <- read_stats_sheet(path)
  expect_equal(stats$Analyte, c("Zinc (mg/L)", "Zinc (ug/L)", "Zinc (mg/L)"))
  expect_equal(stats$Maximum, c(5, 6004, 5))
})

test_that("an analyte in one unit keeps its plain name", {
  data <- dplyr::mutate(mka_summary_fixture(), output_unit = "mg/L")
  path <- withr::local_tempfile(fileext = ".xlsx")
  out <- suppressMessages(mka_to_excel(data, save_path = path))

  expect_false(any(grepl("mg/L", names(out), fixed = TRUE)))
})

test_that("a nested series holding two units is an error, not pooled", {
  data <- mka_summary_fixture()
  data$data[[3]]$output_unit <- c(rep("mg/L", 2), "ug/L", rep("mg/L", 3))

  expect_error(
    write_summary_fixture(data),
    "more than one unit"
  )
})

test_that("a non-detect minimum or maximum carries its <, as text", {
  path <- write_summary_fixture()
  cells <- readxl::read_excel(path, sheet = "Statistics", col_types = "list")

  expect_equal(
    cells$Minimum,
    list(0.002, "<0.001", "<0.001", 1),
    ignore_attr = TRUE
  )
  # the maximum is the highest detect - MW01 Copper's 0.003, not its <0.01 -
  # and a non-detect only where nothing was detected
  expect_equal(
    cells$Maximum,
    list(0.003, 0.005, "<0.001", 4),
    ignore_attr = TRUE
  )

  # detects stay numbers rather than numbers stored as text
  expect_type(cells$Minimum[[1]], "double")
  expect_type(cells$Maximum[[2]], "double")
  # and the mean, calculated rather than reported, is never marked
  expect_type(cells[["Mean (as reported)"]][[3]], "double")
})

test_that("include_summary needs include_stats", {
  expect_error(
    mka_to_excel(
      mka_summary_fixture(),
      save_path = withr::local_tempfile(fileext = ".xlsx"),
      include_summary = TRUE
    ),
    "include_stats = TRUE"
  )
})

test_that("include_summary needs detect_flag in the nested data", {
  path <- withr::local_tempfile(fileext = ".xlsx")

  expect_error(
    mka_to_excel(
      mka_stats_fixture(),
      save_path = path,
      include_stats = TRUE,
      include_summary = TRUE
    ),
    "detect_flag"
  )
  expect_false(file.exists(path))

  expect_error(
    mka_to_excel(
      mka_fixture(),
      save_path = path,
      include_stats = TRUE,
      include_summary = TRUE
    ),
    "nested `data` column"
  )
})

mka_criteria_fixture <- function(col = "criteria", guideline = 0.003) {
  stem <- if (col == "criteria") "" else paste0(col, "_")
  data <- mka_summary_fixture()
  data$data <- lapply(data$data, function(d) {
    d[[col]] <- guideline
    d[[paste0(stem, "exceedance")]] <- d$detect_flag == "Y" &
      d$concentration > guideline
    d
  })
  data
}

test_that("a guideline joined as `criteria` is picked up without asking", {
  stats <- read_stats_sheet(write_summary_fixture(mka_criteria_fixture()))

  expect_equal(utils::tail(names(stats), 2), c("Guideline", "Exceedances"))
  expect_equal(stats$Guideline, rep(0.003, 4))
  expect_equal(stats$Exceedances, c(0, 1, 0, 4))
})

test_that("criteria_col names a guideline joined under another name", {
  data <- mka_criteria_fixture("criteria_99", guideline = 0.0025)
  stats <- read_stats_sheet(
    write_summary_fixture(data, criteria_col = criteria_99)
  )

  expect_equal(stats$Guideline, rep(0.0025, 4))
  expect_equal(stats$Exceedances, c(1, 2, 0, 4))
})

test_that("criteria_col = NULL leaves the guideline out", {
  stats <- read_stats_sheet(
    write_summary_fixture(mka_criteria_fixture(), criteria_col = NULL)
  )

  expect_false(any(c("Guideline", "Exceedances") %in% names(stats)))
})

test_that("a pair with no guideline reads - for exceedances, not 0", {
  data <- mka_criteria_fixture()
  # MW01 / Zinc has no guideline
  data$data[[3]]$criteria <- NA_real_
  data$data[[3]]$exceedance <- NA

  stats <- read_stats_sheet(write_summary_fixture(data))

  expect_equal(stats$Guideline[2], "-")
  expect_equal(stats$Exceedances[2], "-")
  expect_equal(stats$Exceedances[-2], c("0", "0", "4"))
})

test_that("naming criteria_col without include_summary warns", {
  expect_warning(
    mka_to_excel(
      mka_criteria_fixture(),
      save_path = withr::local_tempfile(fileext = ".xlsx"),
      include_stats = TRUE,
      criteria_col = criteria
    ),
    "include_summary = TRUE"
  )

  # left at its default, it says nothing
  expect_no_warning(mka_to_excel(
    mka_criteria_fixture(),
    save_path = withr::local_tempfile(fileext = ".xlsx"),
    include_stats = TRUE
  ))
})

test_that("a criteria_col the nested data lacks points at join_action_levels()", {
  expect_error(
    mka_to_excel(
      mka_summary_fixture(),
      save_path = withr::local_tempfile(fileext = ".xlsx"),
      include_stats = TRUE,
      include_summary = TRUE,
      criteria_col = "criteria_99"
    ),
    "join_action_levels"
  )
})

test_that("reported values are written as plain decimals after a <", {
  expect_equal(
    format_reported(c(0.001, 0.0000005, 12.5, 0.1 + 0.2, 1500)),
    c("0.001", "0.0000005", "12.5", "0.3", "1500")
  )
})

# Guideline sets laid into the nested data the way join_action_levels() lays
# them in: a value column with _name, _unit and _basis beside it, and the
# set's own exceedance verdict.
mka_sets_fixture <- function(...) {
  sets <- list(...)
  data <- mka_summary_fixture()
  data$data <- lapply(data$data, function(d) {
    for (col in names(sets)) {
      stem <- if (col == "criteria") "" else paste0(col, "_")
      d[[col]] <- sets[[col]]$value
      d[[paste0(col, "_name")]] <- sets[[col]]$name
      d[[paste0(col, "_unit")]] <- "mg/L"
      d[[paste0(col, "_basis")]] <- NA_character_
      d[[paste0(stem, "exceedance")]] <- d$detect_flag == "Y" &
        d$concentration > sets[[col]]$value
    }
    d
  })
  data
}

nemp_sets <- function() {
  mka_sets_fixture(
    criteria_95 = list(value = 0.004, name = "NEMP 95%"),
    criteria_99 = list(value = 0.0025, name = "NEMP 99%")
  )
}

test_that("several guideline sets are written side by side, in the order named", {
  stats <- read_stats_sheet(write_summary_fixture(
    nemp_sets(),
    criteria_col = c(criteria_99, criteria_95)
  ))

  expect_equal(
    utils::tail(names(stats), 4),
    c(
      "NEMP 99% Guideline",
      "NEMP 99% Exceedances",
      "NEMP 95% Guideline",
      "NEMP 95% Exceedances"
    )
  )
  expect_equal(stats[["NEMP 99% Guideline"]], rep(0.0025, 4))
  expect_equal(stats[["NEMP 95% Guideline"]], rep(0.004, 4))
})

test_that("each set's exceedances are counted against that set alone", {
  stats <- read_stats_sheet(write_summary_fixture(
    nemp_sets(),
    criteria_col = c(criteria_95, criteria_99)
  ))

  # MW01 / Zinc's detects run to 0.005: over 0.004 once, over 0.0025 twice
  expect_equal(stats[["NEMP 95% Exceedances"]], c(0, 1, 0, 4))
  expect_equal(stats[["NEMP 99% Exceedances"]], c(1, 2, 0, 4))
})

test_that("the sets can be named as strings or picked with a helper", {
  quoted <- read_stats_sheet(write_summary_fixture(
    nemp_sets(),
    criteria_col = c("criteria_95", "criteria_99")
  ))
  helper <- read_stats_sheet(write_summary_fixture(
    nemp_sets(),
    criteria_col = starts_with("criteria_9")
  ))

  expect_equal(utils::tail(names(quoted), 4), utils::tail(names(helper), 4))
  expect_equal(quoted[["NEMP 99% Exceedances"]], c(1, 2, 0, 4))
})

test_that("sets with no recorded name are told apart by their columns", {
  data <- nemp_sets()
  data$data <- lapply(data$data, function(d) {
    d$criteria_95_name <- NA_character_
    d$criteria_99_name <- NA_character_
    d
  })

  stats <- read_stats_sheet(write_summary_fixture(
    data,
    criteria_col = c(criteria_95, criteria_99)
  ))

  expect_equal(
    utils::tail(names(stats), 4),
    c(
      "criteria_95 Guideline",
      "criteria_95 Exceedances",
      "criteria_99 Guideline",
      "criteria_99 Exceedances"
    )
  )
})

test_that("two sets joined under one name keep distinct headings", {
  data <- mka_sets_fixture(
    criteria_a = list(value = 0.004, name = "NEMP"),
    criteria_b = list(value = 0.0025, name = "NEMP")
  )

  stats <- read_stats_sheet(write_summary_fixture(
    data,
    criteria_col = c(criteria_a, criteria_b)
  ))

  expect_equal(
    utils::tail(names(stats), 4),
    c(
      "NEMP (criteria_a) Guideline",
      "NEMP (criteria_a) Exceedances",
      "NEMP (criteria_b) Guideline",
      "NEMP (criteria_b) Exceedances"
    )
  )
})

test_that("a lone named set is headed with its name", {
  data <- mka_sets_fixture(criteria = list(value = 0.003, name = "ANZG 95%"))
  stats <- read_stats_sheet(write_summary_fixture(data))

  expect_equal(
    utils::tail(names(stats), 2),
    c("ANZG 95% Guideline", "ANZG 95% Exceedances")
  )
})

test_that("left at its default, sets it did not write are named, not dropped", {
  data <- mka_sets_fixture(
    criteria = list(value = 0.003, name = "ANZG 95%"),
    criteria_99 = list(value = 0.0025, name = "NEMP 99%")
  )
  path <- withr::local_tempfile(fileext = ".xlsx")

  expect_message(
    mka_to_excel(
      data,
      save_path = path,
      include_stats = TRUE,
      include_summary = TRUE
    ),
    "criteria_99 joined but not written"
  )
  expect_equal(
    utils::tail(names(read_stats_sheet(path)), 2),
    c("ANZG 95% Guideline", "ANZG 95% Exceedances")
  )

  # with no `criteria` at all, nothing is written but the sets are still named
  expect_message(
    mka_to_excel(
      nemp_sets(),
      save_path = path,
      include_stats = TRUE,
      include_summary = TRUE
    ),
    "criteria_95, criteria_99 joined but not written"
  )
})

test_that("one missing set among several is an error naming it", {
  expect_error(
    mka_to_excel(
      nemp_sets(),
      save_path = withr::local_tempfile(fileext = ".xlsx"),
      include_stats = TRUE,
      include_summary = TRUE,
      criteria_col = c(criteria_95, criteria_80)
    ),
    "criteria_80"
  )
})

test_that("a helper picking only a set's companion columns is an error", {
  expect_error(
    mka_to_excel(
      nemp_sets(),
      save_path = withr::local_tempfile(fileext = ".xlsx"),
      include_stats = TRUE,
      include_summary = TRUE,
      criteria_col = ends_with("_unit")
    ),
    "not a guideline value column"
  )
})

test_that("include_summary substitutes non-detects as the trend test did", {
  data <- dplyr::tibble(
    location_code = "MW01",
    chem_name = "Zinc",
    date = as.POSIXct("2024-01-15", tz = "UTC") + (0:7) * 8.64e6,
    concentration = c(4, 0.5, 3, 0.5, 2, 6, 7, 8),
    prefix = c(NA, "<", NA, "<", NA, NA, NA, NA),
    detect_flag = c("Y", "N", "Y", "N", "Y", "Y", "Y", "Y"),
    output_unit = "mg/L"
  )
  path <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(mka_to_excel(
    mann_kendall_test(data, lor_multiplier = 0.5),
    save_path = path,
    include_stats = TRUE,
    include_summary = TRUE
  ))
  stats <- read_stats_sheet(path)

  substituted <- c(4, 0.25, 3, 0.25, 2, 6, 7, 8)
  expect_equal(stats[["Mean (ND at 0.5x LOR)"]], mean(substituted))
  expect_equal(stats[["Mean (ND at 0.5x LOR)"]], stats[["Mean (as tested)"]])
  expect_equal(
    stats[["Standard Deviation (ND at 0.5x LOR)"]],
    stats[["Standard Deviation (as tested)"]]
  )
  expect_equal(
    stats[["95th Percentile (ND at 0.5x LOR)"]],
    unname(quantile(substituted, 0.95))
  )
  # the reported minimum is still the lab's <0.5
  expect_equal(stats$Minimum, "<0.5")
})

# MW01 / Zinc's non-detects were reported at two LORs; MW02's at one.
mka_lor_fixture <- function() {
  data <- mka_fixture()
  data$lor_changed <- c(FALSE, FALSE, TRUE, FALSE)
  data$lor_min <- c(0.01, NA, 0.001, NA)
  data$lor_max <- c(0.01, NA, 0.01, NA)
  data
}

test_that("a trend whose LOR changed is marked, and keeps its colour", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  out <- mka_to_excel(mka_lor_fixture(), save_path = path, legend = FALSE)

  expect_equal(out$Zinc, c("Decreasing *", "Increasing"))
  expect_equal(out$Copper, c("No Significant Trend", "Stable"))

  # MW01 / Zinc: the first data row, third column, filled as Decreasing
  expect_equal(
    unname(cells_filled(path, "FF63BE7B")),
    matrix(c(2, 3), ncol = 2)
  )
})

test_that("mark_lor_changes = FALSE writes the trends plainly", {
  out <- mka_to_excel(
    mka_lor_fixture(),
    save_path = withr::local_tempfile(fileext = ".xlsx"),
    mark_lor_changes = FALSE
  )

  expect_equal(out$Zinc, c("Decreasing", "Increasing"))
})

test_that("the marker is explained on the legend, only where it is used", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(mka_lor_fixture(), save_path = path)
  legend <- openxlsx::read.xlsx(path, sheet = "Legend", colNames = FALSE)
  expect_true(any(grepl("more than one LOR", legend[[1]], fixed = TRUE)))

  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(mka_fixture(), save_path = path)
  legend <- openxlsx::read.xlsx(path, sheet = "Legend", colNames = FALSE)
  expect_false(any(grepl("more than one LOR", legend[[1]], fixed = TRUE)))
})

test_that("without a legend the marker is explained under the table", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(mka_lor_fixture(), save_path = path, legend = FALSE)
  summary <- openxlsx::read.xlsx(
    path,
    sheet = "Trend Summary",
    colNames = FALSE,
    skipEmptyRows = FALSE
  )

  # two wells, a blank row, then the note
  expect_match(summary[[1]][[5]], "more than one LOR", fixed = TRUE)
})

test_that("the statistics sheet carries the LORs, the trend unmarked", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(mka_lor_fixture(), save_path = path, include_stats = TRUE)
  stats <- read_stats_sheet(path)

  expect_equal(
    names(stats),
    c(
      "Monitoring Well",
      "Analyte",
      "Trend",
      "p-value",
      "LOR Changed",
      "Lowest ND LOR",
      "Highest ND LOR"
    )
  )
  zinc <- stats[stats$Analyte == "Zinc", ]
  expect_equal(zinc$Trend, c("Decreasing", "Increasing"))
  expect_equal(zinc[["LOR Changed"]], c("Yes", "No"))
  expect_equal(zinc[["Lowest ND LOR"]], c("0.001", "0.01"))
  expect_equal(zinc[["Highest ND LOR"]], c("0.01", "0.01"))
  # a series with no non-detects has no LOR to show
  expect_equal(stats[stats$Analyte == "Copper", "Lowest ND LOR"], c("-", "-"))
})

test_that("an LOR that could not be read back is neither marked nor shown", {
  # as mann_kendall_test() returns a series substituted at zero beforehand
  data <- mka_lor_fixture()
  data$lor_changed[[3]] <- NA
  data$lor_min[[3]] <- NA
  data$lor_max[[3]] <- NA
  path <- withr::local_tempfile(fileext = ".xlsx")
  out <- mka_to_excel(data, save_path = path, include_stats = TRUE)

  expect_equal(out$Zinc, c("Decreasing", "Increasing"))
  stats <- read_stats_sheet(path)
  expect_equal(stats[stats$Analyte == "Zinc", "LOR Changed"], c("-", "No"))
})

test_that("mann_kendall_test() output is marked end to end", {
  # the lab dropped its LOR from 0.01 to 0.001 partway through
  data <- dplyr::tibble(
    location_code = "MW01",
    chem_name = "Zinc",
    date = as.POSIXct("2024-01-15", tz = "UTC") + (0:7) * 8.64e6,
    concentration = c(0.01, 0.01, 0.01, 0.01, 0.001, 0.001, 0.001, 0.001),
    prefix = "<",
    detect_flag = "N",
    output_unit = "mg/L"
  )
  trends <- mann_kendall_test(data)
  out <- mka_to_excel(
    trends,
    save_path = withr::local_tempfile(fileext = ".xlsx")
  )

  expect_equal(out$Zinc, paste0(trends$trend, " *"))
})

test_that("mark_lor_changes must be a single TRUE or FALSE", {
  expect_error(
    mka_to_excel(
      mka_lor_fixture(),
      save_path = withr::local_tempfile(fileext = ".xlsx"),
      mark_lor_changes = NA
    ),
    "`mark_lor_changes` must be TRUE or FALSE"
  )
})

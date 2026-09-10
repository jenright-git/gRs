mka_fixture <- function() {
  dplyr::tibble(
    location_code = c("MW02", "MW02", "MW01", "MW01"),
    chem_name = c("Zinc", "Copper", "Zinc", "Copper"),
    trend = c("Increasing", "Stable", "Decreasing", "No Significant Trend"),
    p_value = c(0.01, 0.5, 0.02, 0.6)
  )
}

test_that("the export pivots analytes across and trends into the cells", {
  out <- mka_to_excel(mka_fixture(), save_path = withr::local_tempfile(
    fileext = ".xlsx"
  ))

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
  out <- mka_to_excel(data, save_path = withr::local_tempfile(
    fileext = ".xlsx"
  ))

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

  expect_equal(unname(fills[c("Increasing", "Stable")]), c("#000000", "#111111"))
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

test_that("repeated zone cells are merged, unless merge_zones is FALSE", {
  zones <- c("Lower", "Lower", "Lower", "Lower")

  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(
    mka_nested_fixture(zones),
    save_path = path,
    include_zone = TRUE
  )
  expect_match(
    openxlsx::loadWorkbook(path)$worksheets[[1]]$mergeCells,
    "A2:A3",
    fixed = TRUE
  )

  path <- withr::local_tempfile(fileext = ".xlsx")
  mka_to_excel(
    mka_nested_fixture(zones),
    save_path = path,
    include_zone = TRUE,
    merge_zones = FALSE
  )
  expect_length(openxlsx::loadWorkbook(path)$worksheets[[1]]$mergeCells, 0)
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

cells_filled <- function(path, fill) {
  styles <- openxlsx::loadWorkbook(path)$styleObjects
  hits <- Filter(function(s) identical(s$style$fill$fillFg[[1]], fill), styles)

  unique(do.call(rbind, lapply(hits, function(s) {
    cbind(row = s$rows, col = s$cols)
  })))
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

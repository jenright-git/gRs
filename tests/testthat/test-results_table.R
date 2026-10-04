# two_sets_fixture() joins 2 ug/L as criteria_95, which every detect
# exceeds, and 5000 ug/L as criteria_99, which only MW02's 8 mg/L does. Its
# non-detects, at 0.5 mg/L, sit above criteria_95 but below criteria_99.

crosstab <- function(
  data = two_sets_fixture(),
  sets = c("criteria_95", "criteria_99"),
  id_cols = "date",
  id_named = TRUE,
  highlight_lor = FALSE,
  criteria_colours = NULL,
  include_zone = FALSE
) {
  criteria_col <- if (is.null(sets)) {
    rlang::quo(NULL)
  } else {
    rlang::quo(dplyr::all_of(!!sets))
  }
  results_crosstab(
    data,
    criteria_col = criteria_col,
    criteria_named = TRUE,
    id_cols = id_cols,
    id_named = id_named,
    highlight_lor = highlight_lor,
    criteria_colours = criteria_colours,
    include_zone = include_zone,
    zone_name = "monitoring_zone",
    zone_label = "Monitoring Zone",
    location_label = "Monitoring Well"
  )
}

write_results <- function(data = two_sets_fixture(), ...) {
  path <- withr::local_tempfile(
    fileext = ".xlsx",
    .local_envir = parent.frame()
  )
  suppressMessages(results_table_to_excel(
    data,
    save_path = path,
    criteria_col = c(criteria_95, criteria_99),
    id_cols = "date",
    ...
  ))
  path
}

# The style a written cell shows: the last one laid over it.
style_at <- function(path, row, col, sheet = "Results") {
  wb <- openxlsx::loadWorkbook(path)
  found <- NULL
  for (s in wb$styleObjects) {
    if (identical(s$sheet, sheet) && any(s$rows == row & s$cols == col)) {
      found <- s$style
    }
  }
  found
}
fill_of <- function(style) {
  fill <- style$fill$fillFg
  if (is.null(fill)) NA_character_ else sub("^FF", "", unname(fill)[[1]])
}
font_of <- function(style) {
  font <- style$fontColour
  if (is.null(font)) NA_character_ else sub("^FF", "", unname(font)[[1]])
}
is_bold <- function(style) "BOLD" %in% style$fontDecoration

# A second analyte, zinc, at MW01 only and with no guideline in either set.
with_zinc <- function() {
  zinc <- chem_fixture(
    chem_name = "Zinc",
    chem_code = "7440-66-6",
    concentration = c(0.2, 0.3, 0.1, 0.2, 0.3, 0.1)
  )[1:3, ]
  zinc <- join_action_levels(
    zinc,
    action_level_fixture(criteria_name = "NEMP 95%", criteria = 2),
    value_col = "criteria_95",
    quiet = TRUE
  )
  zinc <- join_action_levels(
    zinc,
    action_level_fixture(criteria_name = "NEMP 99%", criteria = 5000),
    value_col = "criteria_99",
    quiet = TRUE
  )
  dplyr::bind_rows(two_sets_fixture(), zinc)
}


# ---------------------------------------------------------------------------
# rows, columns and cells
# ---------------------------------------------------------------------------

test_that("the crosstab has a row per sample and a column per analyte", {
  x <- crosstab()

  expect_equal(x$ids$location_code, rep(c("MW01", "MW02"), each = 3))
  expect_equal(nrow(x$analytes), 1)
  expect_equal(x$analytes$chem_name, "Copper")
  expect_equal(x$analytes$output_unit, "mg/L")
  expect_equal(x$conc[, 1], c(1.5, 2.5, 0.5, 4, 8, 0.5))
  expect_equal(x$nd[, 1], c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE))
})

test_that("analytes are sorted by group then name, a column per unit", {
  data <- dplyr::bind_rows(
    chem_fixture(chem_name = "Zinc", chem_code = "7440-66-6"),
    chem_fixture(chem_group = "Major Ions", chem_name = "Sodium"),
    chem_fixture(),
    chem_fixture(output_unit = "ug/L", concentration = 1:6 * 100)
  )
  x <- crosstab(data, sets = NULL)

  expect_equal(x$analytes$chem_group, c("Major Ions", rep("Metals", 3)))
  expect_equal(x$analytes$chem_name, c("Sodium", "Copper", "Copper", "Zinc"))
  expect_equal(x$analytes$output_unit, c("mg/L", "mg/L", "ug/L", "mg/L"))
  expect_equal(x$conc[, 3], 1:6 * 100)
})

test_that("a sample not analysed for an analyte is a gap", {
  x <- crosstab(with_zinc())

  expect_equal(x$analytes$chem_name, c("Copper", "Zinc"))
  expect_equal(x$conc[, 2], c(0.2, 0.3, 0.1, NA, NA, NA))
  expect_equal(
    result_cell_text(x$conc, x$nd)[, 2],
    c("0.2", "0.3", "<0.1", "-", "-", "-")
  )
})

test_that("a non-detect is written with its <", {
  x <- crosstab()
  expect_equal(
    result_cell_text(x$conc, x$nd)[, 1],
    c("1.5", "2.5", "<0.5", "4", "8", "<0.5")
  )
})


# ---------------------------------------------------------------------------
# shading
# ---------------------------------------------------------------------------

test_that("a result takes the colour of the highest guideline it exceeds", {
  x <- crosstab()

  # 8 mg/L exceeds both: 5 mg/L is the higher, and implies the 0.002
  expect_equal(
    x$fill[, 1],
    c("criteria_95", "criteria_95", NA, "criteria_95", "criteria_99", NA)
  )
})

test_that("the shading is by guideline value, not by the order sets are named", {
  forwards <- crosstab(sets = c("criteria_95", "criteria_99"))
  backwards <- crosstab(sets = c("criteria_99", "criteria_95"))

  expect_equal(backwards$fill, forwards$fill)
  expect_equal(backwards$set_labels, c("NEMP 99%", "NEMP 95%"))
})

test_that("two sets giving the same guideline: the first named wins", {
  data <- chem_fixture() %>%
    join_action_levels(
      action_level_fixture(criteria_name = "Set A", criteria = 2),
      value_col = "criteria_a",
      quiet = TRUE
    ) %>%
    join_action_levels(
      action_level_fixture(criteria_name = "Set B", criteria = 2),
      value_col = "criteria_b",
      quiet = TRUE
    )

  expect_equal(
    unique(stats::na.omit(crosstab(data, sets = c("criteria_a", "criteria_b"))$fill[, 1])),
    "criteria_a"
  )
  expect_equal(
    unique(stats::na.omit(crosstab(data, sets = c("criteria_b", "criteria_a"))$fill[, 1])),
    "criteria_b"
  )
})

test_that("a non-detect above a guideline is shaded only with highlight_lor", {
  expect_equal(crosstab()$fill[c(3, 6), 1], c(NA_character_, NA_character_))

  # 0.5 mg/L is above 0.002 but not 5, so it takes criteria_95
  expect_equal(
    crosstab(highlight_lor = TRUE)$fill[c(3, 6), 1],
    c("criteria_95", "criteria_95")
  )
})

test_that("highlight_lor decides, whatever lor_as_exceedance made of it", {
  data <- chem_fixture() %>%
    join_action_levels(
      action_level_fixture(criteria_name = "NEMP 95%", criteria = 2),
      value_col = "criteria_95",
      lor_as_exceedance = TRUE,
      quiet = TRUE
    )

  expect_message(
    x <- crosstab(data, sets = "criteria_95"),
    "highlight_lor = TRUE"
  )
  expect_equal(x$fill[c(3, 6), 1], c(NA_character_, NA_character_))
  expect_no_message(
    x <- crosstab(data, sets = "criteria_95", highlight_lor = TRUE)
  )
  expect_equal(x$fill[c(3, 6), 1], c("criteria_95", "criteria_95"))
})

test_that("no guideline sets: no guideline rows and nothing shaded", {
  x <- crosstab(sets = NULL)

  expect_length(x$sets, 0)
  expect_equal(nrow(x$guideline_text), 0)
  expect_true(all(is.na(x$fill)))
})


# ---------------------------------------------------------------------------
# guidelines across the top
# ---------------------------------------------------------------------------

test_that("each set's guideline is given in the result's unit", {
  x <- crosstab()

  expect_equal(x$set_labels, c("NEMP 95%", "NEMP 99%"))
  expect_equal(x$guideline_value[, 1], c(0.002, 5))
  expect_equal(x$guideline_text[, 1], c("0.002", "5"))
})

test_that("several guidelines in one column read as their range", {
  data <- two_sets_fixture()
  data$criteria_95[1] <- 0.001
  x <- crosstab(data)

  expect_equal(x$guideline_text[1, 1], "0.001 - 0.002")
  expect_true(is.na(x$guideline_value[1, 1]))
})

test_that("an analyte a set has no guideline for is left blank", {
  data <- with_zinc()
  data$criteria_95[data$chem_name == "Zinc"] <- NA
  data$criteria_99[data$chem_name == "Zinc"] <- NA
  x <- crosstab(data)

  expect_equal(x$guideline_text[, 2], c("", ""))
})

test_that("criteria_colours sets a colour by set column or by set name", {
  expect_equal(crosstab()$set_colours, c("#FBD08A", "#F0A868"))
  expect_equal(
    crosstab(criteria_colours = c(criteria_99 = "#FF0000"))$set_colours,
    c("#FBD08A", "#FF0000")
  )
  expect_equal(
    crosstab(criteria_colours = c("NEMP 95%" = "#00FF00"))$set_colours,
    c("#00FF00", "#F0A868")
  )
  expect_equal(
    crosstab(criteria_colours = c("#111111", "#222222"))$set_colours,
    c("#111111", "#222222")
  )
  expect_error(crosstab(criteria_colours = "#111111"), "must hold 2")
  expect_error(
    crosstab(criteria_colours = c(nonsense = "#111111")),
    "names no guideline set"
  )
})


# ---------------------------------------------------------------------------
# id_cols
# ---------------------------------------------------------------------------

test_that("left at the default, id columns `data` lacks are left out", {
  expect_message(
    x <- crosstab(
      id_cols = c("date", "sample_code", "lab_report_number"),
      id_named = FALSE
    ),
    "sample_code, lab_report_number"
  )
  expect_equal(x$id_labels, c("Monitoring Well", "Sample Date"))
  expect_named(x$ids, c("location_code", "date"))
})

test_that("an id column named outright that `data` lacks is an error", {
  data <- two_sets_fixture()
  data$sample_code <- paste0("S", 1:6)

  expect_error(crosstab(data, id_cols = c("date", "sample_id")), "sample_id")
  expect_error(
    crosstab(data, id_cols = c("date", "sample_id")),
    "Did you mean"
  )
})

test_that("id columns are written in the order given, under their headings", {
  data <- two_sets_fixture()
  data$sample_code <- paste0("S", 6:1)
  data$start_depth <- c(0.5, 1, 1.5, 0.5, 1, 1.5)

  x <- crosstab(
    data,
    id_cols = c("sample_code", "Sampled" = "date", "start_depth")
  )
  expect_named(x$ids, c("location_code", "sample_code", "date", "start_depth"))
  expect_equal(
    x$id_labels,
    c("Monitoring Well", "Sample ID", "Sampled", "Depth From")
  )
  # sorted by location, then the first id column
  expect_equal(x$ids$sample_code, c("S4", "S5", "S6", "S1", "S2", "S3"))
  expect_equal(x$conc[, 1], c(0.5, 2.5, 1.5, 0.5, 8, 4))
})

test_that("a column of no dictionary entry is headed from its own name", {
  data <- two_sets_fixture()
  data$bore_angle <- 1
  expect_equal(
    crosstab(data, id_cols = c("date", "bore_angle"))$id_labels,
    c("Monitoring Well", "Sample Date", "Bore angle")
  )
})

test_that("the location is written once, wherever it is named", {
  x <- crosstab(id_cols = c("location_code", "date"))
  expect_named(x$ids, c("location_code", "date"))
})

test_that("id_cols = NULL leaves one row per location, with a warning", {
  expect_warning(
    x <- crosstab(id_cols = NULL),
    "2 cell\\(s\\)"
  )
  expect_equal(x$ids$location_code, c("MW01", "MW02"))
  # the highest detect, not the 0.5 LOR
  expect_equal(x$conc[, 1], c(2.5, 8))
  expect_equal(x$nd[, 1], c(FALSE, FALSE))
})

test_that("two results in a cell keep the highest detect, else the highest LOR", {
  data <- chem_fixture(
    location_code = "MW01",
    date = as.POSIXct("2024-01-15", tz = "UTC"),
    concentration = c(3, 10, 1, 0.5, 0.2, 0.1),
    detect_flag = c("Y", "N", "Y", "N", "N", "N"),
    prefix = c(NA, "<", NA, "<", "<", "<"),
    chem_name = rep(c("Copper", "Zinc"), each = 3)
  )
  expect_warning(x <- crosstab(data, sets = NULL), "2 cell\\(s\\)")

  # copper: 3 detected beats a 10 LOR; zinc: none detected, so the 0.5 LOR
  expect_equal(x$conc[1, ], c(3, 0.5))
  expect_equal(x$nd[1, ], c(FALSE, TRUE))
})

test_that("a result the export repeats word for word is shown once, unremarked", {
  data <- two_sets_fixture()
  data <- dplyr::bind_rows(data, data[c(1, 3), ])

  expect_no_warning(x <- crosstab(data))
  expect_equal(x$conc[, 1], c(1.5, 2.5, 0.5, 4, 8, 0.5))
})


# ---------------------------------------------------------------------------
# zones and errors
# ---------------------------------------------------------------------------

test_that("include_zone puts the zone first and sorts by it", {
  data <- two_sets_fixture()
  data$monitoring_zone <- rep(c("Zone B", "Zone A"), each = 3)
  x <- crosstab(data, include_zone = TRUE)

  expect_named(x$ids, c("monitoring_zone", "location_code", "date"))
  expect_equal(x$ids$location_code, rep(c("MW02", "MW01"), each = 3))
  expect_equal(x$id_labels[1:2], c("Monitoring Zone", "Monitoring Well"))
})

test_that("a location in two zones is an error", {
  data <- two_sets_fixture()
  data$monitoring_zone <- c("A", "B", "A", "A", "A", "A")
  expect_error(crosstab(data, include_zone = TRUE), "more than one")
})

test_that("a table stacked by criteria_long() is turned away", {
  expect_error(crosstab(criteria_long(two_sets_fixture())), "criteria_long")
})

test_that("missing required columns are named", {
  data <- two_sets_fixture()
  data$location_code <- NULL
  expect_error(crosstab(data), "location_code")
})

test_that("a misnamed guideline set is an error", {
  expect_error(crosstab(sets = "criteria_50"), "criteria_50")
})


# ---------------------------------------------------------------------------
# Excel
# ---------------------------------------------------------------------------

test_that("the header block reads group, name, unit, then each set", {
  path <- write_results()
  sheet <- openxlsx::read.xlsx(path, colNames = FALSE, skipEmptyRows = FALSE)
  cells <- function(row, cols) unname(unlist(sheet[row, cols]))

  expect_equal(cells(1, 1:3), c("Monitoring Well", "Sample Date", "Metals"))
  expect_equal(cells(2, 3), "Copper")
  expect_equal(cells(3, 3), "mg/L")
  expect_equal(cells(4, c(1, 3)), c("NEMP 95%", "0.002"))
  expect_equal(cells(5, c(1, 3)), c("NEMP 99%", "5"))
  expect_equal(sheet[6:11, 1], rep(c("MW01", "MW02"), each = 3))
})

test_that("detects are numbers and non-detects are text", {
  path <- write_results()

  expect_equal(
    openxlsx::readWorkbook(path, rows = 6, cols = 3, colNames = FALSE)[[1]],
    1.5
  )
  expect_equal(
    openxlsx::readWorkbook(path, rows = 8, cols = 3, colNames = FALSE)[[1]],
    "<0.5"
  )
  # the guideline is a number too
  expect_equal(
    openxlsx::readWorkbook(path, rows = 4, cols = 3, colNames = FALSE)[[1]],
    0.002
  )
})

test_that("dates are written as dates", {
  path <- write_results()
  expect_equal(
    openxlsx::readWorkbook(
      path,
      rows = 6,
      cols = 2,
      colNames = FALSE,
      detectDates = TRUE
    )[[1]],
    as.Date("2024-01-15")
  )
  expect_equal(style_at(path, 6, 2)$numFmt$formatCode, "dd/mm/yyyy")
})

test_that("cells are bold, grey and shaded as the crosstab says", {
  path <- write_results()

  detect_95 <- style_at(path, 6, 3)
  expect_true(is_bold(detect_95))
  expect_equal(fill_of(detect_95), "FBD08A")

  detect_99 <- style_at(path, 10, 3)
  expect_true(is_bold(detect_99))
  expect_equal(fill_of(detect_99), "F0A868")

  nd <- style_at(path, 8, 3)
  expect_false(is_bold(nd))
  expect_equal(font_of(nd), "808080")
  expect_true(is.na(fill_of(nd)))
})

test_that("highlight_lor shades the non-detects in the Excel sheet", {
  nd <- style_at(write_results(highlight_lor = TRUE), 8, 3)
  expect_equal(fill_of(nd), "FBD08A")
  expect_equal(font_of(nd), "808080")
})

test_that("the headings and locations take the house colours", {
  path <- write_results()

  for (cell in list(c(1, 1), c(1, 3), c(2, 3), c(3, 3), c(6, 1))) {
    style <- style_at(path, cell[[1]], cell[[2]])
    expect_equal(fill_of(style), "008768")
    expect_equal(font_of(style), "FFFFFF")
    expect_true(is_bold(style))
  }
  expect_equal(style_at(path, 2, 3)$textRotation, "90")
  expect_equal(fill_of(style_at(path, 4, 1)), "FBD08A")
  expect_equal(fill_of(style_at(path, 5, 3)), "F0A868")
})

test_that("the identifier headings and set names are merged", {
  path <- write_results()
  merges <- openxlsx::loadWorkbook(path)$worksheets[[1]]$mergeCells

  expect_true(all(
    c("A1:A3", "B1:B3", "A4:B4", "A5:B5") %in%
      sub('.*ref="([^"]+)".*', "\\1", merges)
  ))
})

test_that("zones are merged down their rows", {
  data <- two_sets_fixture()
  data$monitoring_zone <- rep(c("Zone A", "Zone B"), each = 3)
  path <- write_results(data, include_zone = TRUE)
  merges <- sub(
    '.*ref="([^"]+)".*',
    "\\1",
    openxlsx::loadWorkbook(path)$worksheets[[1]]$mergeCells
  )

  expect_true(all(c("A6:A8", "A9:A11") %in% merges))
  expect_equal(fill_of(style_at(path, 6, 2)), "9BBEAF")
})

test_that("the legend sheet explains the formatting", {
  path <- write_results(highlight_lor = TRUE)
  legend <- openxlsx::read.xlsx(path, sheet = "Legend")

  expect_named(legend, c("Key", "Meaning"))
  expect_true(all(
    c("Detected result", "Not analysed", "Exceeds the NEMP 99% guideline") %in%
      legend$Meaning
  ))
  expect_true(any(grepl("highest guideline", legend$Meaning)))
  expect_true(any(grepl("LOR is above the guideline", legend$Meaning)))

  expect_false("Legend" %in% openxlsx::getSheetNames(write_results(legend = FALSE)))
})

test_that("`data` is returned invisibly and unchanged", {
  data <- two_sets_fixture()
  expect_invisible(
    out <- suppressMessages(results_table_to_excel(
      data,
      save_path = withr::local_tempfile(fileext = ".xlsx"),
      criteria_col = c(criteria_95, criteria_99)
    ))
  )
  expect_identical(out, data)
})

test_that("the sheet cannot take the legend's name", {
  expect_error(
    results_table_to_excel(
      two_sets_fixture(),
      save_path = withr::local_tempfile(fileext = ".xlsx"),
      sheet_name = "Legend"
    ),
    "legend sheet"
  )
})


# ---------------------------------------------------------------------------
# gt
# ---------------------------------------------------------------------------

gt_styles <- function(tbl, row, col) {
  st <- tbl[["_styles"]]
  unlist(st$styles[st$locname == "data" & st$rownum == row & st$colname == col])
}

test_that("results_table() puts the guideline rows above the results", {
  skip_if_not_installed("gt")
  tbl <- suppressMessages(
    results_table(two_sets_fixture(), criteria_col = c(criteria_95, criteria_99))
  )

  expect_s3_class(tbl, "gt_tbl")
  body <- tbl[["_data"]]
  expect_equal(body$.id_1, c("NEMP 95%", "NEMP 99%", rep(c("MW01", "MW02"), each = 3)))
  expect_equal(body$.id_2[3], "15/01/2024")
  expect_equal(body$.analyte_1, c("0.002", "5", "1.5", "2.5", "<0.5", "4", "8", "<0.5"))
  expect_equal(
    unlist(gt:::dt_spanners_get(tbl)$spanner_label),
    "Metals"
  )
})

test_that("results_table() styles the cells as the Excel sheet does", {
  skip_if_not_installed("gt")
  tbl <- suppressMessages(
    results_table(two_sets_fixture(), criteria_col = c(criteria_95, criteria_99))
  )

  # rows 1-2 are the guidelines, so MW02's 8 mg/L is row 7
  expect_true(all(c("bold", "#F0A868") %in% gt_styles(tbl, 7, ".analyte_1")))
  expect_true(all(c("bold", "#FBD08A") %in% gt_styles(tbl, 3, ".analyte_1")))
  expect_true("#808080" %in% gt_styles(tbl, 5, ".analyte_1"))
  expect_false("#FBD08A" %in% gt_styles(tbl, 5, ".analyte_1"))
  expect_true("#FBD08A" %in% gt_styles(tbl, 1, ".analyte_1"))
  expect_true(all(c("#008768", "#FFFFFF") %in% gt_styles(tbl, 3, ".id_1")))
})

test_that("results_table() carries the legend as source notes", {
  skip_if_not_installed("gt")
  tbl <- suppressMessages(results_table(
    two_sets_fixture(),
    criteria_col = c(criteria_95, criteria_99),
    highlight_lor = TRUE
  ))
  notes <- vapply(tbl[["_source_notes"]], as.character, character(1))

  expect_length(notes, nrow(results_legend(crosstab(highlight_lor = TRUE))))
  expect_true(any(grepl("Exceeds the NEMP 95% guideline", notes, fixed = TRUE)))
})

test_that("results_table() runs with no guideline set", {
  skip_if_not_installed("gt")
  tbl <- suppressMessages(results_table(chem_fixture(), criteria_col = NULL))
  body <- tbl[["_data"]]

  expect_equal(nrow(body), 6)
})

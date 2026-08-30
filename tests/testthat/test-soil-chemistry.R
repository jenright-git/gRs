# Soil exports are the awkward case: no fraction, a sampled interval, and
# every analyte reported twice under different units. These check that a soil
# table still travels the same pipeline as a liquid one.

soil_export <- function() {
  suppressMessages(data_processor(example_report("davSChem1_Chemistry.xlsx")))
}

test_that("a soil export carries the matrix and the sampled interval", {
  soil <- soil_export()

  expect_equal(unique(soil$matrix_code), "Soil")
  # The interval columns are mapped from sample_depth_from / _to / _avg. This
  # export leaves them blank, so presence is what is checked, not content.
  expect_true(all(
    c("start_depth", "end_depth", "sample_depth") %in% names(soil)
  ))
})

test_that("solid and leachate results are reported in different units", {
  path <- example_report("davSChem1_Chemistry.xlsx")
  both <- suppressMessages(data_processor(path, result_type = "all"))

  solid_units <- unique(both$output_unit[both$result_type == "REG"])
  leach_units <- unique(both$output_unit[both$result_type == "LEACHED_REG"])

  expect_length(intersect(solid_units, leach_units), 0)
})

test_that("prefix and detect_flag agree with each other", {
  soil <- soil_export()

  expect_setequal(unique(soil$detect_flag), c("Y", "N"))
  expect_true(all(soil$prefix[soil$detect_flag == "N"] == "<"))
  expect_type(soil$concentration, "double")
})

test_that("the pipeline runs end to end on soil data", {
  soil <- soil_export()

  collapsed <- select_max_concentration(soil)
  expect_lte(nrow(collapsed), nrow(soil))

  substituted <- half_lor(collapsed, lor_multiplier = 0.5)
  expect_true("lor_multiplier_applied" %in% names(substituted))

  # summary_stats() must cope with fraction present but entirely NA.
  stats <- suppressWarnings(summary_stats(substituted))
  expect_gt(nrow(stats), 0)
})

test_that("soil and liquid chemistry bind cleanly", {
  soil <- soil_export()
  liquid <- suppressMessages(data_processor(
    example_report("davLChem1_Chemistry (28).xlsx")
  ))

  combined <- dplyr::bind_rows(soil, liquid)

  expect_equal(nrow(combined), nrow(soil) + nrow(liquid))
  expect_true(all(is.na(soil$fraction)))
  expect_false(all(is.na(liquid$fraction)))
  expect_true(any(grepl("^Dissolved ", liquid$chem_name)))
})

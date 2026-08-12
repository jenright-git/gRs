library(gRs)
library(dplyr)

# Exercises the ESDAT soil/sediment export (davSChem1_Chemistry) through
# data_processor() and on into the rest of the pipeline.
#
# The soil export differs from the liquid export (davLChem1_Chemistry) in
# three ways:
#
#   * no "Total or Filtered" column          -> fraction is supplied as NA
#   * a sampled interval instead of a well   -> start_depth / end_depth /
#                                               sample_depth
#   * each analyte reported twice            -> Result Type is REG (mg/kg)
#                                               or LEACHED_REG (mg/L, ug/L)

soil_file <- "example_reports/davSChem1_Chemistry.xlsx"
liquid_file <- "example_reports/davLChem1_Chemistry (28).xlsx"

check <- function(desc, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) FALSE)
  cat(sprintf("  [%s] %s\n", if (ok) "PASS" else "FAIL", desc))
}

soil <- suppressWarnings(data_processor(soil_file))
all_types <- suppressWarnings(data_processor(soil_file, result_type = "all"))
leachate <- suppressWarnings(
  data_processor(soil_file, result_type = "LEACHED_REG")
)

cat("--- Detection ---\n")
check("soil export is detected as chemistry",
      attr(soil, "report_type") == "chemistry")
check("required columns are all present",
      all(c("sampled_date_time", "concentration", "output_unit", "site_id",
            "location_code", "prefix", "chem_name", "detect_flag",
            "chem_group") %in% names(soil)))
check("date is derived from sampled_date_time",
      "date" %in% names(soil) && !any(is.na(soil$date)))

cat("\n--- result_type filtering ---\n")
check("default keeps REG only", all(soil$result_type == "REG"))
check("'all' keeps both result types",
      setequal(unique(all_types$result_type), c("REG", "LEACHED_REG")))
check("REG + LEACHED_REG accounts for every row",
      nrow(soil) + nrow(leachate) == nrow(all_types))
check("leachate results are named explicitly",
      all(leachate$result_type == "LEACHED_REG"))
check("solid and leachate results use different units",
      length(intersect(unique(soil$output_unit),
                       unique(leachate$output_unit))) == 0)
check("naming a type not present errors rather than returning nothing",
      inherits(try(suppressWarnings(
        data_processor(soil_file, result_type = "NOT_A_TYPE")
      ), silent = TRUE), "try-error"))

cat("\n--- Soil-specific columns ---\n")
check("matrix_code identifies the export as soil",
      all(soil$matrix_code == "Soil"))
check("the sampled interval is carried through",
      all(c("start_depth", "end_depth", "sample_depth") %in% names(soil)))
check("fraction is present but empty",
      "fraction" %in% names(soil) && all(is.na(soil$fraction)))
check("no Dissolved prefix is applied without a fraction",
      !any(grepl("^Dissolved ", soil$chem_name)))

cat("\n--- Non-detects ---\n")
check("prefix is blank for detects and '<' for non-detects",
      all(soil$prefix[!is.na(soil$prefix)] == "<"))
check("detect_flag is derived from prefix",
      all(soil$detect_flag == ifelse(is.na(soil$prefix), "Y", "N")))
check("concentration is numeric",
      is.numeric(soil$concentration))

cat("\n--- Downstream pipeline ---\n")
reduced <- select_max_concentration(soil)
check("select_max_concentration collapses the field duplicates",
      nrow(reduced) < nrow(soil))
check("one row per location / date / analyte remains",
      !any(duplicated(reduced[, c("location_code", "date", "chem_name")])))

halved <- half_lor(reduced, multiplier = 0.5)
check("half_lor halves the non-detects",
      all(halved$lor_multiplier_applied[halved$prefix == "<"] == 0.5))

stats <- suppressMessages(summary_stats(halved))
check("summary_stats runs on soil data (no fraction column needed)",
      nrow(stats) > 0)

cat("\n--- Liquid export is unaffected ---\n")
liquid <- suppressWarnings(data_processor(liquid_file))
check("liquid export still reads",
      attr(liquid, "report_type") == "chemistry")
check("liquid export still carries a real fraction",
      setequal(unique(liquid$fraction), c("T", "F")))
check("dissolved results are still renamed",
      any(grepl("^Dissolved ", liquid$chem_name)))
check("soil and liquid chemistry bind cleanly",
      nrow(bind_rows(soil, liquid)) == nrow(soil) + nrow(liquid))

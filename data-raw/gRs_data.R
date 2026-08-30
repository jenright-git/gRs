# How data/gRs_data.rda is built.
#
# gRs_data is an anonymised groundwater chemistry export. It was first saved
# before data_processor() gained `detect_flag` and renamed `total_or_filtered`
# to `fraction`, which left the bundled example carrying a shape the package
# no longer produces - summary_stats() could not run on it. This brings the
# stored table up to what data_processor() returns today, and nothing else.
#
# The original export is not in the repository, so this reads and rewrites
# data/gRs_data.rda in place. It is written to be safe to run twice.

library(dplyr)

load("data/gRs_data.rda")

# data_processor() renames total_or_filtered to fraction.
if ("total_or_filtered" %in% names(gRs_data)) {
  gRs_data <- rename(gRs_data, fraction = total_or_filtered)
}

# data_processor() derives detect_flag from prefix the same way: a result with
# no "<" against it was detected.
if (!"detect_flag" %in% names(gRs_data)) {
  gRs_data <- gRs_data %>%
    mutate(detect_flag = ifelse(is.na(prefix), "Y", "N")) %>%
    relocate(detect_flag, .after = prefix)
}

# An older data_processor() prefixed "Dissolved" without checking whether the
# lab had already named the analyte that way.
gRs_data$chem_name <- sub(
  "^Dissolved Dissolved ",
  "Dissolved ",
  gRs_data$chem_name
)

usethis::use_data(gRs_data, overwrite = TRUE)

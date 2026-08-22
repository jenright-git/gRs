library(gRs)
library(dplyr)

# Exercises the esdat action level export (a guideline set such as the ANZG
# marine water toxicant DGVs) through action_level_processor() and onto a
# chemistry table with join_action_levels().
#
# The export has no column naming the guideline set, so the name is supplied
# by `name`. Its value and unit share one cell ("0.006 µg Sn/L"), and its
# Leached / Total / Filtered flags say which results the guideline may be
# compared against.

al_file <- "example_reports/ANZG Marine Water Toxicant DGVs LOSP 95_ (March 2026).xlsx"
liquid_file <- "example_reports/davLChem1_Chemistry (28).xlsx"
equis_file <- "example_reports/Analytical Results II.xlsx"
soil_file <- "example_reports/davSChem1_Chemistry.xlsx"

check <- function(label, ok) {
  cat(if (isTRUE(ok)) "  [PASS] " else "  [FAIL] ", label, "\n", sep = "")
}

# ---------------------------------------------------------------------------
cat("\n--- reading the export ---\n")

al <- action_level_processor(al_file, name = "ANZG 95% Marine")

check("one row per guideline", nrow(al) == 63)
check("the supplied name is carried", all(al$criteria_name == "ANZG 95% Marine"))
check("value split from unit", is.numeric(al$criteria) && !any(is.na(al$criteria)))
check("units left as a plain concentration", all(al$criteria_unit == "µg/L"))
check(
  "basis pulled out of 'µg Sn/L'",
  al$criteria_basis[al$chem_name == "Tributyltin"] == "Sn"
)
check("original cell kept", any(al$criteria_text == "0.006 µg Sn/L"))
check("flags read as logical", is.logical(al$total) && all(al$total))
check("report_type attribute", attr(al, "report_type") == "action_level")

cat("\n--- the name defaults to the file name ---\n")
default_named <- action_level_processor(al_file)
check(
  "name taken from the file",
  unique(default_named$criteria_name) ==
    "ANZG Marine Water Toxicant DGVs LOSP 95_ (March 2026)"
)

# ---------------------------------------------------------------------------
cat("\n--- unit conversion ---\n")

liquid <- data_processor(liquid_file)
joined <- join_action_levels(liquid, al)

check("row count is unchanged", nrow(joined) == nrow(liquid))
check("row order is unchanged", identical(joined$chem_name, liquid$chem_name))

ug <- joined %>% filter(chem_name == "Toluene", output_unit == "µg/L")
check("a µg/L guideline against a µg/L result is untouched", all(ug$criteria == 180))

mg <- joined %>% filter(chem_name == "Dissolved Lead", output_unit == "mg/L")
check(
  "a µg/L guideline is converted for an mg/L result",
  nrow(mg) > 0 && all(mg$criteria == 0.0044)
)

check(
  "the criteria unit is the result's own unit",
  all(joined$criteria_unit[!is.na(joined$criteria)] ==
        joined$output_unit[!is.na(joined$criteria)])
)

# ---------------------------------------------------------------------------
cat("\n--- matching ---\n")

check(
  "the Dissolved prefix does not stop a name match",
  any(joined$chem_name == "Dissolved Lead" & !is.na(joined$criteria))
)

# The CAS code identifies the analyte and is never trimmed to force a join:
# 91-20-3V (naphthalene by VOC) is not 91-20-3, and the guideline set's own
# 91-20-3 and 91-20-3_VOC are two different entries. Reaching one through the
# other would be a guess.
naph <- joined %>% filter(chem_code == "91-20-3V")
check(
  "a suffixed code is not collapsed onto the bare CAS guideline",
  nrow(naph) > 0 && all(is.na(naph$criteria))
)
check(
  "and it is reported as unmatched rather than quietly trimmed",
  "91-20-3V" %in% attr(joined, "unmatched")$chem_code
)
exact <- joined %>% filter(chem_code == "91-20-3")
check(
  "while the code that does match exactly still gets its guideline",
  nrow(exact) > 0 && all(exact$criteria == 70)
)

no_guideline <- joined %>% filter(chem_name == "C6-C10 fraction")
check(
  "an analyte with no guideline is left NA",
  nrow(no_guideline) > 0 && all(is.na(no_guideline$criteria))
)
check(
  "the unmatched analytes are reported back",
  "C6-C10" %in% attr(joined, "unmatched")$chem_code
)

cat("\n--- EQuIS exports match too ---\n")
equis <- suppressWarnings(data_processor(equis_file))
check("cas_rn is normalised to chem_code", "chem_code" %in% names(equis))
equis_joined <- join_action_levels(equis, al)
check(
  "an mg/l (EQuIS spelling) result is matched",
  any(!is.na(equis_joined$criteria))
)

# ---------------------------------------------------------------------------
cat("\n--- guidelines are not applied where they do not belong ---\n")

soil <- data_processor(soil_file, result_type = "all")
soil_joined <- join_action_levels(soil, al, quiet = TRUE)
check("a water guideline never reaches a soil result", all(is.na(soil_joined$criteria)))

filtered_only <- al %>% mutate(total = FALSE, filtered = TRUE)
frac <- join_action_levels(liquid, filtered_only, quiet = TRUE)
check(
  "a filtered-only guideline skips total results",
  !any(!is.na(frac$criteria) & frac$fraction == "T")
)
check(
  "a filtered-only guideline still reaches filtered results",
  any(!is.na(frac$criteria) & frac$fraction == "F")
)

leach_only <- al %>%
  mutate(matrix_code = "Soil", leached = TRUE)
leach <- join_action_levels(soil, leach_only, quiet = TRUE)
check(
  "a leachate guideline skips solid-phase results",
  !any(!is.na(leach$criteria) & leach$result_type == "REG")
)
check(
  "a leachate guideline reaches leachate results",
  any(!is.na(leach$criteria) & leach$result_type == "LEACHED_REG")
)

no_matrix <- al %>% mutate(matrix_code = NA_character_)
w <- tryCatch(
  {
    join_action_levels(liquid, no_matrix, quiet = TRUE)
    NA_character_
  },
  warning = function(w) conditionMessage(w)
)
check(
  "a guideline set with no matrix_code is warned about, not applied silently",
  grepl("no matrix_code", w)
)

# ---------------------------------------------------------------------------
cat("\n--- reading a value out of an awkward cell ---\n")

cells <- gRs:::parse_action_level(c(
  "1,000 µg/L",
  "6.5 - 8.5",
  "0.5-1.0 mg/L",
  "1E-3 mg/L",
  "<0.1 mg/L",
  "0.006 µg Sn/L"
))
check("a thousands separator is not read as a unit", cells$value[1] == 1000)
check("and the unit survives it", cells$unit[1] == "µg/L")
check(
  "a range has no single value, so it is left unread",
  is.na(cells$value[2]) && is.na(cells$value[3])
)
check("scientific notation still reads", cells$value[4] == 0.001)
check("a less-than still reads", cells$value[5] == 0.1)
check("an embedded basis still splits", cells$basis[6] == "Sn")

# A range must not survive a separate unit column either - that is the path
# where a misread value converts cleanly and gets compared.
ranged <- gRs:::process_action_levels(
  data.frame(
    ChemName = c("Widgetium", "pH"),
    ChemCode = c("1-2-3", "4-5-6"),
    MatrixType = "Water",
    ActionLevel = c("1,000", "6.5 - 8.5"),
    Units = "µg/L",
    Total = "true",
    Filtered = "true",
    Leached = "false",
    stringsAsFactors = FALSE
  ),
  name = "X"
)
check("a supplied unit does not rescue a range", nrow(ranged) == 1)
check(
  "and the comma value is read in full",
  ranged$criteria == 1000 && ranged$criteria_unit == "µg/L"
)

# ---------------------------------------------------------------------------
cat("\n--- an empty table keeps its shape ---\n")

empty <- join_action_levels(liquid[0, ], al, quiet = TRUE)
check("row count is still zero", nrow(empty) == 0)
check(
  "the guideline columns are still there",
  all(c("criteria", "criteria_unit", "exceedance", "exceedance_ratio") %in%
    names(empty))
)
check("criteria is numeric, not logical", is.numeric(empty$criteria))
check(
  "and it feeds summary_stats without erroring",
  inherits(
    tryCatch(
      summary_stats(empty, include_criteria = TRUE),
      error = function(e) e
    ),
    "data.frame"
  )
)

# ---------------------------------------------------------------------------
cat("\n--- exceedances ---\n")

hit <- joined %>% filter(chem_name == "Ethylbenzene", concentration == 5000)
check("a detected result over the guideline is an exceedance", all(hit$exceedance))
check("and its ratio is reported", all(hit$exceedance_ratio == 5000 / 80))

nd <- joined %>%
  filter(detect_flag == "N", !is.na(criteria), concentration > criteria)
check(
  "a non-detect over the guideline is flagged separately, not as an exceedance",
  nrow(nd) > 0 && !any(nd$exceedance) && all(nd$lor_above_criteria)
)

# Whether an LOR above the guideline counts is settled at the join, so that
# everything reading `exceedance` afterwards agrees with it.
lor_on <- join_action_levels(liquid, al, lor_as_exceedance = TRUE, quiet = TRUE)
nd_on <- lor_on %>%
  filter(detect_flag == "N", !is.na(criteria), concentration > criteria)
check(
  "lor_as_exceedance = TRUE counts those non-detects as exceedances",
  nrow(nd_on) > 0 && all(nd_on$exceedance)
)
check(
  "and still tells them apart from a detected exceedance",
  all(nd_on$lor_above_criteria)
)
check(
  "detected exceedances are untouched by the option",
  identical(
    which(joined$exceedance & joined$detect_flag == "Y"),
    which(lor_on$exceedance & lor_on$detect_flag == "Y")
  )
)
check(
  "the default is detects only",
  sum(joined$exceedance, na.rm = TRUE) < sum(lor_on$exceedance, na.rm = TRUE)
)

# summary_stats() must count the join's verdict, not re-derive its own.
s_off <- joined %>% filter(!is.na(criteria)) %>%
  summary_stats(include_criteria = TRUE)
s_on <- lor_on %>% filter(!is.na(criteria)) %>%
  summary_stats(include_criteria = TRUE)
check(
  "summary_stats honours the choice made at the join",
  sum(s_on$exceedance_count) > sum(s_off$exceedance_count)
)

# ---------------------------------------------------------------------------
cat("\n--- a second guideline set alongside the first ---\n")

anzg_99 <- al %>%
  mutate(criteria = criteria / 5, criteria_name = "ANZG 99% Marine")
two <- joined %>%
  join_action_levels(anzg_99, value_col = "criteria_99", quiet = TRUE)
check("both columns are present", all(c("criteria", "criteria_99") %in% names(two)))
check(
  "and hold their own values",
  isTRUE(all.equal(
    two$criteria_99[!is.na(two$criteria)],
    two$criteria[!is.na(two$criteria)] / 5
  ))
)

# The second join used to overwrite the first set's verdict.
check(
  "the second set does not overwrite the first set's exceedance columns",
  identical(two$exceedance, joined$exceedance) &&
    identical(two$exceedance_ratio, joined$exceedance_ratio)
)
check(
  "the second set gets its own, prefixed",
  all(c("criteria_99_exceedance", "criteria_99_exceedance_ratio",
        "criteria_99_lor_above_criteria") %in% names(two))
)
check(
  "which are stricter, the guideline being five times lower",
  sum(two$criteria_99_exceedance, na.rm = TRUE) >
    sum(two$exceedance, na.rm = TRUE)
)

cat("\n--- stacking the sets into one column ---\n")

long <- criteria_long(two)
check("one row per result per set", nrow(long) == nrow(two) * 2)
check(
  "both sets are named",
  setequal(unique(long$criteria_set), c("criteria", "criteria_99"))
)
check(
  "a single criteria column carries both",
  isTRUE(all.equal(
    long$criteria[long$criteria_set == "criteria"],
    two$criteria
  )) &&
    isTRUE(all.equal(
      long$criteria[long$criteria_set == "criteria_99"],
      two$criteria_99
    ))
)
check(
  "a single exceedance column carries both",
  isTRUE(all.equal(
    long$exceedance[long$criteria_set == "criteria_99"],
    two$criteria_99_exceedance
  ))
)
check(
  "the guideline set name comes across",
  "ANZG 99% Marine" %in% long$criteria_name
)
check(
  "the per-set columns are gone from the stacked table",
  !any(grepl("^criteria_99", names(long)))
)
check(
  "keep_unmatched = FALSE returns comparisons only",
  all(!is.na(criteria_long(two, keep_unmatched = FALSE)$criteria))
)
check(
  "one set can be picked out",
  setequal(unique(criteria_long(two, sets = "criteria_99")$criteria_set),
           "criteria_99")
)
check(
  "and it feeds summary_stats through the single column",
  inherits(
    long %>% filter(criteria_set == "criteria_99", !is.na(criteria)) %>%
      summary_stats(include_criteria = TRUE),
    "data.frame"
  )
)

cat("\n--- summary_stats can be pointed at a named set ---\n")

s99 <- two %>% filter(!is.na(criteria_99)) %>%
  summary_stats(include_criteria = TRUE, value_col = "criteria_99")
check(
  "the set keeps its name on the way out",
  all(c("criteria_99", "criteria_99_exceedance_count") %in% names(s99))
)
check(
  "and the default is unchanged",
  all(c("criteria", "exceedance_count") %in%
    names(summary_stats(joined %>% filter(!is.na(criteria)),
                        include_criteria = TRUE)))
)
check(
  "it counts that set's exceedances, not the default set's",
  sum(s99$criteria_99_exceedance_count) ==
    sum(two$criteria_99_exceedance[!is.na(two$criteria_99)], na.rm = TRUE)
)
check(
  "naming a column that was never joined errors clearly",
  grepl(
    "no 'criteria_999' column",
    tryCatch(
      summary_stats(two, include_criteria = TRUE, value_col = "criteria_999"),
      error = function(e) conditionMessage(e)
    )
  )
)

# ---------------------------------------------------------------------------
cat("\n--- a partly recorded fraction does not erase chem_name ---\n")

mixed <- liquid
mixed$fraction[seq(1, nrow(mixed), by = 3)] <- NA_character_
mixed <- gRs:::process_chemistry(
  mixed %>%
    rename(report_result_value = concentration, report_result_unit = output_unit,
           total_or_filtered = fraction) %>%
    mutate(chem_name = sub("^Dissolved ", "", chem_name))
)
check(
  "every row keeps its analyte name",
  !any(is.na(mixed$chem_name))
)
check(
  "the unrecorded rows are simply left unprefixed",
  !any(grepl("^Dissolved", mixed$chem_name[is.na(mixed$fraction)]))
)
check(
  "while the filtered rows are still prefixed",
  all(grepl("^Dissolved", mixed$chem_name[!is.na(mixed$fraction) &
                                            mixed$fraction == "F"]))
)

cat("\n--- expected errors ---\n")

cat("--- Expect error: two guideline sets passed at once ---\n")
res <- tryCatch(
  join_action_levels(liquid, bind_rows(al, anzg_99)),
  error = function(e) conditionMessage(e)
)
cat("Caught expected error:", res, "\n")

cat("--- Expect error: a chemistry table passed as the guidelines ---\n")
res <- tryCatch(
  join_action_levels(liquid, liquid),
  error = function(e) conditionMessage(e)
)
cat("Caught expected error:", res, "\n")

# ---------------------------------------------------------------------------
cat("\n--- a guideline with no unit is refused, never assumed ---\n")

unitless <- gRs:::process_action_levels(
  data.frame(
    ChemName = c("Toluene", "Ethylbenzene"),
    ActionLevel = c(180, 80),
    MatrixType = "Water",
    Total = "true",
    stringsAsFactors = FALSE
  ),
  name = "No Units"
)
check("the unit is left NA, not invented", all(is.na(unitless$criteria_unit)))

w <- tryCatch(
  {
    gRs:::process_action_levels(
      data.frame(ChemName = "Toluene", ActionLevel = 180,
                 stringsAsFactors = FALSE),
      name = "No Units"
    )
    NA_character_
  },
  warning = function(w) conditionMessage(w)
)
check("reading one is warned about at import", grepl("carry no unit", w))

res <- tryCatch(
  join_action_levels(liquid, unitless, quiet = TRUE),
  error = function(e) conditionMessage(e)
)
check(
  "and a set with no units at all is an error, not empty columns",
  is.character(res) && grepl("records no units", res)
)

# A set where only some rows lack a unit joins the rest and reports the gap.
partial <- al
partial$criteria_unit[partial$chem_name == "Toluene"] <- NA_character_
msgs <- utils::capture.output(
  p <- join_action_levels(liquid, partial),
  type = "message"
)
check(
  "a partly unitless set names the rows it would not join",
  any(grepl("NOT JOINED", msgs)) && any(grepl("no unit", msgs))
)
check(
  "the unitless guideline is not applied",
  all(is.na(p$criteria[p$chem_name == "Toluene"]))
)
check(
  "while the rest of the set still joins",
  any(!is.na(p$criteria))
)

cat("\n--- data_processor points at the right function ---\n")
res <- tryCatch(data_processor(al_file), warning = function(w) conditionMessage(w))
check(
  "the guideline file is recognised as one",
  grepl("action_level_processor", res)
)

# ---------------------------------------------------------------------------
cat("\n--- and on into the rest of the package ---\n")

stats <- joined %>%
  filter(!is.na(criteria)) %>%
  summary_stats(include_criteria = TRUE)
check(
  "summary_stats reads the criteria column",
  all(c("criteria", "exceedance_count") %in% names(stats))
)
check("and counts exceedances", sum(stats$exceedance_count) > 0)

plot <- joined %>%
  filter(chem_name == "Toluene", location_code == "MW04") %>%
  timeseries_plot(criteria_col = criteria)
check("timeseries_plot draws the criteria line", inherits(plot, "ggplot"))

# The line drawn must be the one summary_stats() counts against, and must not
# depend on how the rows happen to be sorted.
`%||%` <- function(a, b) if (is.null(a)) b else a
line_of <- function(p) {
  layer <- p$layers[[length(p$layers)]]
  layer$data$yintercept %||% layer$aes_params$yintercept
}

# One analyte reported in two units is what puts two numbers against a single
# guideline: join_action_levels() converts the guideline into each result's
# own unit, so the µg/L rows carry 180 and the mg/L rows 0.18.
tol_ug <- joined %>% filter(chem_name == "Toluene", !is.na(criteria))
mixed <- bind_rows(
  tol_ug,
  tol_ug %>% mutate(
    concentration = concentration / 1000,
    output_unit = "mg/L",
    criteria = criteria / 1000
  )
)

fwd <- suppressWarnings(timeseries_plot(mixed, criteria_col = criteria))
rev_ <- suppressWarnings(
  timeseries_plot(mixed[rev(seq_len(nrow(mixed))), ], criteria_col = criteria)
)
check(
  "the line does not move when the rows are re-sorted",
  isTRUE(all.equal(line_of(fwd), line_of(rev_)))
)
check(
  "and it is the lowest value, matching summary_stats",
  isTRUE(all.equal(line_of(fwd), min(mixed$criteria, na.rm = TRUE)))
)
check(
  "which is what single_criteria picks for the same group",
  isTRUE(all.equal(
    line_of(fwd),
    gRs:::single_criteria(mixed$criteria, mixed$chem_name)
  ))
)

w <- tryCatch(
  {
    timeseries_plot(mixed, criteria_col = criteria)
    NA_character_
  },
  warning = function(w) conditionMessage(w)
)
check("the ambiguity is warned about", grepl("Using the lowest", w))
check("and the units are named where they differ", grepl("more than one unit", w))
check("naming both of them", grepl("mg/L", w))

# A single unit is the ordinary case and says nothing about units.
w1 <- tryCatch(
  {
    timeseries_plot(tol_ug, criteria_col = criteria)
    NA_character_
  },
  warning = function(w) conditionMessage(w)
)
check("a single criteria value warns about nothing", is.na(w1))

cat("\nDone.\n")

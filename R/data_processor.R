#' data_processor
#'
#' Read an EQuIS or ESDAT export and normalise it to a consistent set of column
#' names. Two report families are supported:
#'
#' * **chemistry** - EQuIS Analytical Results II, the ESDAT matrix-specific
#'   exports (`LChem1_Chemistry` for liquids, `SChem1_Chemistry` for soils and
#'   sediments, and their `dav`-prefixed variants), and ESDAT `Chemistry List`
#'   exports.
#' * **water_level** - EQuIS `Water Levels II` and ESDAT gauging reports.
#'
#' Both the report family and the sheet are detected from the worksheet
#' contents rather than from sheet names, so a gauging report or a soil export
#' can be passed without changing `sheet_pattern`.
#'
#' Soil exports differ from liquid exports in three ways that this function
#' reconciles. They carry no `Total or Filtered` column, so `fraction` is
#' returned as `NA` and no `Dissolved` prefix is applied to `chem_name`. They
#' carry a sampled interval (`start_depth`, `end_depth`, `sample_depth`) that
#' liquid exports leave empty. And they report each analyte twice - once as a
#' solid-phase concentration (`REG`, mg/kg) and once as a leachate
#' concentration (`LEACHED_REG`, mg/L or ug/L) - which `result_type` resolves.
#' Soil and liquid chemistry therefore share a shape and can be combined with
#' [dplyr::bind_rows()].
#'
#' Where EQuIS and ESDAT use different names for the same field, the **EQuIS**
#' name is adopted as canonical (e.g. `reference_elev`, `water_level`,
#' `measured_depth_of_well`, `lnapl_density`, `dry_indicator_yn`, `task_code`).
#' The exceptions are the four concepts the package already had names for -
#' `site_id`, `location_code`, `sampled_date_time` and `date` - which are kept
#' so that the plotting and trend functions work unchanged across both report
#' families. ESDAT-only fields that have no EQuIS equivalent (for example
#' `product_corrected_water_level`) keep a snake_case version of the ESDAT name.
#'
#' Water level reports are returned with the full EQuIS field set present, so
#' ESDAT and EQuIS gauging data can be combined with [dplyr::bind_rows()].
#'
#' @param myfile_path file path to data
#' @param sheet_pattern pattern matching the excel sheet name for new or old
#'   esdat chemistry formats. Only used when locating a chemistry sheet.
#' @param report_type one of `"auto"` (default), `"chemistry"` or
#'   `"water_level"`. `"auto"` tries chemistry first, then water level.
#' @param result_type which result types to keep from a chemistry export that
#'   carries a result type column. `"primary"` (default) keeps the ordinary
#'   reported result - `"REG"` in ESDAT exports, `"TRG"` in EQuIS exports -
#'   and drops everything else, reporting what it dropped. `"all"` keeps every
#'   row. Otherwise give the types to keep by name, e.g.
#'   `c("REG", "LEACHED_REG")` for a soil export's solid and leachate results,
#'   or `"LEACHED_REG"` for the leachate results alone. Matching is
#'   case-insensitive, rows with no result type recorded are always kept, and
#'   exports with no result type column are unaffected.
#' @param default_depth_unit unit assumed for depths and elevations when the
#'   export carries no unit column (ESDAT gauging reports). Default `"m"`.
#'
#' @returns A tibble of normalised data, arranged by date, with a
#'   `"report_type"` attribute recording which family was detected. Returns
#'   `NULL` (with a warning) if no readable sheet is found.
#' @export
#' @examples
#' \dontrun{
#' # chemistry (unchanged behaviour)
#' data_processor('my_file_path')
#'
#' # soil / sediment - auto-detected; solid-phase results only by default
#' soil <- data_processor('davSChem1_Chemistry.xlsx')
#'
#' # the leachate (ASLP/TCLP) results the default drops
#' leachate <- data_processor('davSChem1_Chemistry.xlsx',
#'                            result_type = 'LEACHED_REG')
#'
#' # gauging reports - auto-detected, no extra arguments needed
#' esdat_gw <- data_processor('GW4_URS_Gauging_Report_esdat.xlsx')
#' equis_gw <- data_processor('Water Levels II_equis.xlsx')
#'
#' # both share a schema, so they bind cleanly
#' dplyr::bind_rows(esdat_gw, equis_gw)
#' }
#' @importFrom dplyr bind_rows filter mutate case_when rename across all_of
#'   any_of relocate
#' @importFrom readxl excel_sheets read_excel
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate rename arrange %>%
#' @importFrom lubridate floor_date parse_date_time
#' @importFrom glue glue
data_processor <- function(
  myfile_path,
  sheet_pattern = "Chem",
  report_type = c("auto", "chemistry", "water_level"),
  result_type = "primary",
  default_depth_unit = "m"
) {
  report_type <- match.arg(report_type)

  if (!file.exists(myfile_path)) {
    stop("File does not exist: ", myfile_path)
  }

  all_sheets <- readxl::excel_sheets(myfile_path)
  notes <- character(0)
  target <- NULL

  if (report_type %in% c("auto", "chemistry")) {
    found <- locate_chemistry_sheet(myfile_path, all_sheets, sheet_pattern)
    if (is.null(found$sheet)) {
      notes <- c(notes, found$note)
    } else {
      target <- found
    }
  }

  if (is.null(target) && report_type %in% c("auto", "water_level")) {
    found <- locate_water_level_sheet(myfile_path, all_sheets)
    if (is.null(found$sheet)) {
      notes <- c(notes, found$note)
    } else {
      target <- found
    }
  }

  if (is.null(target)) {
    # Action level exports look like neither family; say so rather than
    # leaving the user to work out why their guideline file read as nothing.
    hint <- if (
      !is.null(locate_action_level_sheet(myfile_path, all_sheets)$sheet)
    ) {
      "\nThis looks like an action level export - read it with action_level_processor()."
    } else {
      ""
    }
    warning(
      "No readable sheet found in '",
      basename(myfile_path),
      "'.\n",
      paste(notes, collapse = "\n"),
      "\nSheets present: ",
      paste(all_sheets, collapse = ", "),
      hint
    )
    return(NULL)
  }

  raw_data <- suppressMessages(readxl::read_excel(
    myfile_path,
    sheet = target$sheet,
    skip = target$skip
  ))

  out <- if (target$type == "water_level") {
    process_water_level(raw_data, default_depth_unit = default_depth_unit)
  } else {
    process_chemistry(raw_data, result_type = result_type)
  }

  attr(out, "report_type") <- target$type
  out
}


# ---------------------------------------------------------------------------
# Sheet location
# ---------------------------------------------------------------------------

#' Locate a chemistry sheet within a workbook
#'
#' Sheets are identified by their contents rather than by name, so the ESDAT
#' matrix-specific exports (`LChem1_Chemistry` for liquids,
#' `SChem1_Chemistry` for soils, and their `dav`-prefixed variants) are all
#' handled without listing each one. Sheets whose name matches
#' `sheet_pattern` are tried first, then every other sheet, which is what
#' finds the EQuIS Analytical Results II export on its unnamed `Sheet1`.
#' Each candidate is peeked at with a header offset of 0 and then 1, because
#' the `dav` exports carry a source/URL banner on the first row.
#'
#' @param myfile_path path to the workbook
#' @param all_sheets character vector of sheet names
#' @param sheet_pattern pattern matching esdat chemistry sheet names
#' @returns list(sheet, skip, type, note); `sheet` is NULL when nothing matched
#' @noRd
locate_chemistry_sheet <- function(myfile_path, all_sheets, sheet_pattern) {
  matching_sheets <- base::grep(sheet_pattern, all_sheets, value = TRUE)
  candidates <- unique(c(
    matching_sheets,
    setdiff(all_sheets, matching_sheets)
  ))

  for (s in candidates) {
    for (skip in c(0, 1)) {
      peek <- peek_names(myfile_path, s, skip = skip)
      if (is.null(peek)) {
        next
      }
      canonical <- canonical_names(peek, COLUMN_ALIASES)
      if (all(CHEMISTRY_SIGNATURE_COLS %in% canonical)) {
        return(list(sheet = s, skip = skip, type = "chemistry", note = NA))
      }
    }
  }

  list(
    sheet = NULL,
    skip = NULL,
    type = "chemistry",
    note = paste0(
      "- chemistry: no sheet carrying all of: ",
      paste(CHEMISTRY_SIGNATURE_COLS, collapse = ", "),
      " (or a recognised alias of each)."
    )
  )
}

#' Locate a water level (gauging) sheet within a workbook
#'
#' Each sheet is peeked at with a header offset of 0 and then 1, because ESDAT
#' gauging reports carry a source/URL banner on the first row.
#'
#' @param myfile_path path to the workbook
#' @param all_sheets character vector of sheet names
#' @returns list(sheet, skip, type, note); `sheet` is NULL when nothing matched
#' @noRd
locate_water_level_sheet <- function(myfile_path, all_sheets) {
  for (skip in c(0, 1)) {
    for (s in all_sheets) {
      peek <- peek_names(myfile_path, s, skip = skip)
      if (is.null(peek)) {
        next
      }
      canonical <- canonical_names(peek, WATER_LEVEL_ALIASES)
      has_keys <- all(
        c("location_code", "sampled_date_time") %in% canonical
      )
      has_measure <- any(WATER_LEVEL_SIGNATURE_COLS %in% canonical)
      if (has_keys && has_measure) {
        return(list(sheet = s, skip = skip, type = "water_level", note = NA))
      }
    }
  }
  list(
    sheet = NULL,
    skip = NULL,
    type = "water_level",
    note = paste0(
      "- water level: no sheet carrying a location column, a date column and ",
      "one of: ",
      paste(WATER_LEVEL_SIGNATURE_COLS, collapse = ", "),
      "."
    )
  )
}

#' Read only the header row of a worksheet
#'
#' @param myfile_path path to the workbook
#' @param sheet sheet name
#' @param skip rows to skip before the header
#' @returns character vector of cleaned column names, or NULL if unreadable
#' @noRd
peek_names <- function(myfile_path, sheet, skip = 0) {
  peek <- tryCatch(
    suppressMessages(suppressWarnings(
      readxl::read_excel(myfile_path, sheet = sheet, skip = skip, n_max = 0)
    )),
    error = function(e) NULL
  )
  if (is.null(peek) || ncol(peek) == 0) {
    return(NULL)
  }
  # AR2 detection matches on the raw uppercase names, so return both forms.
  # Peeking with an offset of 1 can turn a row of data into the header, so
  # janitor is silenced here - it has nothing useful to say about a candidate
  # that is about to be rejected anyway.
  c(names(peek), names(suppressWarnings(janitor::clean_names(peek))))
}

#' Resolve a vector of column names to their canonical equivalents
#'
#' @param nms character vector of (cleaned) column names
#' @param alias_map named list: canonical_name -> character vector of aliases
#' @returns character vector of canonical names present in `nms`
#' @noRd
canonical_names <- function(nms, alias_map) {
  resolved <- nms
  for (canonical in names(alias_map)) {
    if (canonical %in% nms) {
      next
    }
    if (any(alias_map[[canonical]] %in% nms)) {
      resolved <- c(resolved, canonical)
    }
  }
  unique(resolved)
}


# ---------------------------------------------------------------------------
# Chemistry processing
# ---------------------------------------------------------------------------

#' Normalise a raw chemistry export
#'
#' @param raw_sw_data data frame straight from readxl
#' @param result_type result types to keep; see [data_processor()]
#' @returns normalised tibble arranged by date
#' @noRd
process_chemistry <- function(raw_sw_data, result_type = "primary") {
  sw_data <- raw_sw_data %>%
    janitor::clean_names() %>%
    resolve_columns(COLUMN_ALIASES) %>%
    filter_result_type(result_type)

  if ("fraction" %in% names(sw_data) && is.logical(sw_data$fraction)) {
    sw_data <- sw_data %>%
      dplyr::mutate(fraction = ifelse(fraction, "F", "T"))
  }

  # Soil exports have no filtered/unfiltered concept and so carry no fraction
  # column. Supply it as NA rather than leaving it absent, so soil and liquid
  # chemistry bind cleanly and summary_stats() - which selects fraction by
  # name - keeps working on soil data.
  if (!"fraction" %in% names(sw_data)) {
    sw_data$fraction <- NA_character_
  }

  if (!"prefix" %in% names(sw_data) && "detect_flag" %in% names(sw_data)) {
    sw_data <- sw_data %>%
      dplyr::mutate(prefix = ifelse(detect_flag == "Y", "=", "<"))
  }

  if (!"detect_flag" %in% names(sw_data) && "prefix" %in% names(sw_data)) {
    sw_data <- sw_data %>%
      dplyr::mutate(detect_flag = ifelse(is.na(prefix), "Y", "N"))
  }

  missing <- setdiff(REQUIRED_COLUMNS, names(sw_data))
  if (length(missing) > 0) {
    stop(
      "Missing required columns after normalization: ",
      paste(missing, collapse = ", "),
      "\nColumns found: ",
      paste(names(sw_data), collapse = ", ")
    )
  }

  if (!"chem_group" %in% names(sw_data)) {
    warning(
      "Column 'chem_group' not found. ",
      "plot_by_analyte(), summary_stats(), establish_plotting_variables(), ",
      "and get_plotting_variables() require this column. ",
      "Add it manually after data_processor() returns."
    )
  }

  sw_data <- sw_data %>%
    dplyr::mutate(date = lubridate::floor_date(sampled_date_time, "day"))

  if (
    "fraction" %in%
      names(sw_data) &&
      any(!is.na(sw_data$fraction))
  ) {
    sw_data <- sw_data %>%
      dplyr::mutate(
        # Tested against NA explicitly: an export where only some rows record
        # a fraction reaches here, and a bare `fraction == "F"` returns NA for
        # the rest - which ifelse() would write into chem_name, losing the
        # analyte's name altogether rather than leaving it unprefixed.
        chem_name = ifelse(
          !is.na(fraction) & fraction == "F",
          yes = glue::glue("Dissolved {chem_name}"),
          no = chem_name
        )
      )
  }

  sw_data %>% dplyr::arrange(date)
}

#' Keep only the requested result types
#'
#' Soil exports report the same analyte twice: once as a solid-phase
#' concentration (`REG`, mg/kg) and once as a leachate concentration
#' (`LEACHED_REG`, mg/L or ug/L). Pooling the two would mix unit systems under
#' a single `chem_name`, so only the primary result types are kept by default.
#'
#' Rows with no result type recorded are always kept, and exports with no
#' result type column at all pass through untouched.
#'
#' @param sw_data chemistry data with canonical column names
#' @param result_type result types to keep; see [data_processor()]
#' @returns `sw_data` with unwanted result types removed
#' @noRd
filter_result_type <- function(sw_data, result_type) {
  if (!"result_type" %in% names(sw_data) || length(result_type) == 0) {
    return(sw_data)
  }

  wanted <- toupper(trimws(as.character(result_type)))
  if (any(wanted == "ALL")) {
    return(sw_data)
  }
  if (any(wanted == "PRIMARY")) {
    wanted <- unique(c(setdiff(wanted, "PRIMARY"), PRIMARY_RESULT_TYPES))
  }

  found <- toupper(trimws(as.character(sw_data$result_type)))
  keep <- is.na(found) | found %in% wanted

  if (!any(keep)) {
    stop(
      "No rows left after filtering on result_type. Requested: ",
      paste(wanted, collapse = ", "),
      "\nResult types present: ",
      paste(sort(unique(found[!is.na(found)])), collapse = ", "),
      "\nPass result_type = \"all\" to keep every row."
    )
  }

  dropped <- table(found[!keep])
  if (length(dropped) > 0) {
    message(
      "Dropped ",
      sum(dropped),
      " rows on result_type (",
      paste(names(dropped), unname(dropped), sep = ": ", collapse = ", "),
      "). Pass result_type = \"all\" to keep them."
    )
  }

  sw_data[keep, , drop = FALSE]
}


# ---------------------------------------------------------------------------
# Water level processing
# ---------------------------------------------------------------------------

#' Normalise a raw water level (gauging) export
#'
#' Handles the two conventions seen in the wild:
#' * EQuIS reports the water level as a *string* of significant figures and
#'   always carries `depth_unit`.
#' * ESDAT reports `Groundwater Elevation` in place of `water_level`, uses a
#'   sparse `Dry` flag rather than `dry_indicator_yn`, and carries no unit
#'   column.
#'
#' @param raw_wl_data data frame straight from readxl
#' @param default_depth_unit unit assumed when the export carries none
#' @returns normalised tibble arranged by date and location
#' @noRd
process_water_level <- function(raw_wl_data, default_depth_unit = "m") {
  wl <- raw_wl_data %>%
    janitor::clean_names() %>%
    resolve_columns(WATER_LEVEL_ALIASES)

  num_cols <- intersect(WATER_LEVEL_NUMERIC_COLS, names(wl))
  if (length(num_cols) > 0) {
    wl <- wl %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(num_cols), coerce_numeric))
  }

  if ("sampled_date_time" %in% names(wl)) {
    wl <- wl %>%
      dplyr::mutate(sampled_date_time = coerce_datetime(sampled_date_time))
  }

  missing <- setdiff(WATER_LEVEL_REQUIRED_COLUMNS, names(wl))
  if (length(missing) > 0) {
    stop(
      "Missing required columns after normalization: ",
      paste(missing, collapse = ", "),
      "\nColumns found: ",
      paste(names(wl), collapse = ", ")
    )
  }

  if (!any(WATER_LEVEL_SIGNATURE_COLS %in% names(wl))) {
    stop(
      "No water level measurement column found. Expected one of: ",
      paste(WATER_LEVEL_SIGNATURE_COLS, collapse = ", "),
      "\nColumns found: ",
      paste(names(wl), collapse = ", ")
    )
  }

  # Fill the EQuIS field set so ESDAT and EQuIS gauging data bind cleanly.
  for (nm in names(WATER_LEVEL_SCHEMA)) {
    if (!nm %in% names(wl)) {
      wl[[nm]] <- WATER_LEVEL_SCHEMA[[nm]]
    }
  }

  # water_level is an elevation; water_depth is a depth below the reference
  # point. Either can be derived from the other given a reference elevation.
  wl <- wl %>%
    dplyr::mutate(
      water_level = ifelse(
        is.na(water_level) & !is.na(reference_elev) & !is.na(water_depth),
        reference_elev - water_depth,
        water_level
      ),
      water_depth = ifelse(
        is.na(water_depth) & !is.na(reference_elev) & !is.na(water_level),
        reference_elev - water_level,
        water_depth
      ),
      water_level_depth = ifelse(
        is.na(water_level_depth),
        water_depth,
        water_level_depth
      ),
      exact_elev = ifelse(is.na(exact_elev), water_level, exact_elev)
    )

  # ESDAT flags dry wells with the word "Dry" and leaves the rest blank.
  wl <- wl %>%
    dplyr::mutate(dry_indicator_yn = normalise_yn(dry_indicator_yn))

  wl <- wl %>%
    dplyr::mutate(
      depth_unit = ifelse(
        is.na(depth_unit),
        default_depth_unit,
        as.character(depth_unit)
      ),
      date = lubridate::floor_date(sampled_date_time, "day")
    )

  wl %>%
    dplyr::relocate(dplyr::any_of(names(WATER_LEVEL_SCHEMA))) %>%
    dplyr::arrange(date, location_code)
}


# ---------------------------------------------------------------------------
# Coercion helpers
# ---------------------------------------------------------------------------

#' Coerce a column to numeric, tolerating text and all-NA logical columns
#' @param x vector to coerce
#' @noRd
coerce_numeric <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }
  if (is.logical(x)) {
    return(as.numeric(x))
  }
  suppressWarnings(as.numeric(trimws(as.character(x))))
}

#' Coerce a column to POSIXct, tolerating Excel serials and text dates
#' @param x vector to coerce
#' @noRd
coerce_datetime <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(x)
  }
  if (inherits(x, "Date")) {
    return(as.POSIXct(x, tz = "UTC"))
  }
  if (is.numeric(x)) {
    # Excel's 1900 date system, with its leap year bug baked in.
    return(as.POSIXct(x * 86400, origin = "1899-12-30", tz = "UTC"))
  }
  suppressWarnings(lubridate::parse_date_time(
    as.character(x),
    orders = c("Ymd HMS", "Ymd HM", "Ymd", "dmY HMS", "dmY HM", "dmY"),
    tz = "UTC",
    quiet = TRUE
  ))
}

#' Normalise assorted yes/no conventions to "Y"/"N"
#'
#' Blank is treated as "N" because ESDAT populates its `Dry` column only for
#' the rows that were dry.
#'
#' @param x vector to normalise
#' @noRd
normalise_yn <- function(x) {
  if (is.logical(x)) {
    return(ifelse(!is.na(x) & x, "Y", "N"))
  }
  if (is.numeric(x)) {
    return(ifelse(!is.na(x) & x > 0, "Y", "N"))
  }
  flag <- toupper(trimws(as.character(x)))
  ifelse(flag %in% c("Y", "YES", "TRUE", "T", "DRY", "1"), "Y", "N")
}


# ---------------------------------------------------------------------------
# Column dictionaries
# ---------------------------------------------------------------------------

# All of these, in canonical form, identify a chemistry export. Every
# supported format carries the four, so they discriminate chemistry sheets
# from gauging sheets and from the banner rows the ESDAT exports start with.
CHEMISTRY_SIGNATURE_COLS <- c(
  "location_code",
  "sampled_date_time",
  "chem_name",
  "concentration"
)

# Result types kept by result_type = "primary": the ordinary reported result
# in ESDAT exports ("REG") and in EQuIS exports ("TRG", target analyte).
# Anything else - leachate results ("LEACHED_REG"), surrogates, internal
# standards - is dropped unless asked for by name.
PRIMARY_RESULT_TYPES <- c("REG", "TRG")

# Any one of these, plus a location and a date, identifies a gauging report.
WATER_LEVEL_SIGNATURE_COLS <- c(
  "water_level",
  "water_depth",
  "water_level_depth",
  "reference_elev"
)

# Concepts shared by chemistry and water level exports. These keep the names
# the package already used so downstream functions work for both.
SHARED_ALIASES <- list(
  sampled_date_time = c(
    "measurement_date",
    "sample_date_time",
    "date_time",
    "datetime",
    "sample_date",
    "collected_date_time",
    "sampled_date",
    "gauging_date"
  ),
  site_id = c("facility_code", "site", "site_code"),
  location_code = c(
    "sys_loc_code",
    "loc_code",
    "location",
    "monitoring_location",
    "location_id"
  )
)

# Maps canonical column names to known aliases from different export formats.
# Add new aliases here when a new export format is encountered.
# Within each vector, earlier entries take priority over later ones.
COLUMN_ALIASES <- c(
  SHARED_ALIASES,
  list(
    concentration = c(
      "report_result_value",
      "reported_value",
      "result_numeric",
      "result"
    ),
    output_unit = c(
      "report_result_unit",
      "reported_unit",
      "result_unit",
      "unit",
      "units"
    ),
    prefix = c("qualifier", "result_qualifier", "result_prefix"),
    chem_name = c(
      "chemical_name",
      "analyte",
      "analyte_name",
      "chemical"
    ),
    # The analyte's code. ESDAT chemistry and ESDAT action level exports
    # share this code set - including the disambiguating suffixes such as
    # `91-20-3_VOC` - so it is what join_action_levels() matches on.
    chem_code = c(
      "cas_rn",
      "cas_number",
      "casrn",
      "cas",
      "chemical_code",
      "analyte_code"
    ),
    chem_group = c(
      "chemical_group",
      "analyte_group",
      "param_group",
      "mth_anl_group_member",
      "method_analyte_group_member",
      "method_analyte_group"
    ),
    fraction = c("total_or_filtered", "filtered", "sample_fraction"),
    sample_type = c(
      "samp_type",
      "field_sample_type",
      "sample_type_code",
      "type"
    ),
    detect_flag = c("detect"),
    # Distinguishes an ordinary result from a leachate result in the ESDAT
    # soil exports; see PRIMARY_RESULT_TYPES.
    result_type = c("result_type_code"),
    # Soil exports carry the matrix and the sampled interval; water exports
    # leave the depth columns empty. EQuIS names are canonical, as elsewhere.
    matrix_code = c("matrix_type", "matrix"),
    start_depth = c("sample_depth_from", "depth_from", "top_depth"),
    end_depth = c("sample_depth_to", "depth_to", "bottom_depth"),
    sample_depth = c("sample_depth_avg", "average_depth", "depth")
  )
)

REQUIRED_COLUMNS <- c(
  "sampled_date_time",
  "concentration",
  "output_unit",
  "site_id",
  "location_code",
  "prefix",
  "chem_name",
  "detect_flag"
)

# Water level aliases. Canonical names follow the EQuIS "Water Levels II"
# export; the aliases are the ESDAT gauging report equivalents plus common
# variants. ESDAT-only fields (lnapl_elevation, product_corrected_water_level,
# top_screen_depth, bottom_screen_depth, interpolated_toc, monitoring_zone,
# monitoring_unit, project) have no EQuIS counterpart and keep their own names.
WATER_LEVEL_ALIASES <- c(
  SHARED_ALIASES,
  list(
    loc_name = c("alternative_name", "location_name", "location_alt_name"),
    reference_elev = c(
      "reference_elevation",
      "top_casing_elev",
      "toc_elevation",
      "reference_point_elevation"
    ),
    water_level = c(
      "groundwater_elevation",
      "gw_elevation",
      "water_elevation",
      "water_level_elev",
      "standing_water_level"
    ),
    exact_elev = c("exact_elevation"),
    water_depth = c(
      "depth_to_water",
      "dtw",
      "water_depth_m",
      "depth_to_groundwater"
    ),
    water_level_depth = c("water_level_depth_corrected"),
    measured_depth_of_well = c(
      "well_depth",
      "total_well_depth",
      "measured_depth",
      "depth_of_well"
    ),
    depth_unit = c("depth_units", "elevation_unit", "unit", "units"),
    technician = c("measured_by", "gauged_by", "field_technician", "sampler"),
    dry_indicator_yn = c("dry", "dry_yn", "dry_indicator", "well_dry"),
    measurement_method = c("method", "gauging_method"),
    dip_or_elevation = c("dip_or_elev", "measurement_basis"),
    remark = c("comments", "comment", "remarks", "notes", "note"),
    equipment_code = c("equipment", "instrument_code"),
    lnapl_depth = c("product_depth", "napl_depth", "depth_to_product"),
    lnapl_thickness = c("product_thickness", "napl_thickness"),
    lnapl_density = c(
      "lnapl_rel_density",
      "lnapl_relative_density",
      "product_density",
      "napl_density",
      "specific_gravity"
    ),
    lnapl_elevation = c("napl_elevation", "product_elevation"),
    product_corrected_water_level = c(
      "corrected_water_level",
      "product_corrected_water_level_elevation"
    ),
    dnapl_depth = c("depth_to_dnapl"),
    dnapl_thickness = c("dnapl_thick"),
    task_code = c("monitoring_round", "round", "monitoring_event", "event"),
    batch_number = c("batch", "ebatch"),
    approval_code = c("approval"),
    x_coord = c("x_coordinate", "easting"),
    y_coord = c("y_coordinate", "northing"),
    top_screen_depth = c("screen_top_depth", "top_of_screen"),
    bottom_screen_depth = c("screen_bottom_depth", "bottom_of_screen"),
    monitoring_zone = c("zone"),
    monitoring_unit = c("hydrogeological_unit"),
    project = c("project_id", "project_code")
  )
)

WATER_LEVEL_REQUIRED_COLUMNS <- c(
  "sampled_date_time",
  "site_id",
  "location_code"
)

# Columns coerced to numeric before any derivation. EQuIS returns water_level
# as text (significant figures applied by the report), and ESDAT returns
# all-blank columns as logical.
WATER_LEVEL_NUMERIC_COLS <- c(
  "reference_elev",
  "water_level",
  "exact_elev",
  "water_depth",
  "water_level_depth",
  "measured_depth_of_well",
  "lnapl_depth",
  "lnapl_thickness",
  "lnapl_density",
  "lnapl_elevation",
  "product_corrected_water_level",
  "dnapl_depth",
  "dnapl_thickness",
  "top_screen_depth",
  "bottom_screen_depth",
  "x_coord",
  "y_coord",
  "longitude",
  "latitude"
)

# The EQuIS "Water Levels II" field set, used as the default output schema so
# that ESDAT and EQuIS gauging data share a shape. The final two entries are
# ESDAT-only fields carried in the schema for the same reason.
WATER_LEVEL_SCHEMA <- list(
  site_id = NA_character_,
  location_code = NA_character_,
  loc_name = NA_character_,
  sampled_date_time = as.POSIXct(NA),
  reference_elev = NA_real_,
  water_level = NA_real_,
  exact_elev = NA_real_,
  water_depth = NA_real_,
  water_level_depth = NA_real_,
  measured_depth_of_well = NA_real_,
  depth_unit = NA_character_,
  technician = NA_character_,
  dry_indicator_yn = NA_character_,
  measurement_method = NA_character_,
  dip_or_elevation = NA_character_,
  remark = NA_character_,
  equipment_code = NA_character_,
  lnapl_depth = NA_real_,
  lnapl_thickness = NA_real_,
  lnapl_density = NA_real_,
  dnapl_depth = NA_real_,
  dnapl_thickness = NA_real_,
  task_code = NA_character_,
  approval_code = NA_character_,
  x_coord = NA_real_,
  y_coord = NA_real_,
  lnapl_elevation = NA_real_,
  product_corrected_water_level = NA_real_
)

#' Rename columns to canonical names based on an alias map
#'
#' @param df data frame to normalise
#' @param alias_map named list: canonical_name -> character vector of known aliases
#' @returns df with columns renamed to canonical names where a match is found
resolve_columns <- function(df, alias_map) {
  current_names <- names(df)
  for (canonical in names(alias_map)) {
    if (canonical %in% current_names) {
      next
    }
    matched <- intersect(alias_map[[canonical]], current_names)
    if (length(matched) > 1) {
      warning(
        "Multiple columns match canonical '",
        canonical,
        "': ",
        paste(matched, collapse = ", "),
        ". Using '",
        matched[[1]],
        "'."
      )
    }
    if (length(matched) >= 1) {
      df <- dplyr::rename(df, !!canonical := !!matched[[1]])
    }
  }
  df
}

# a table with no detect_flag is an error naming data_processor()

    Code
      check_detect_flag(dplyr::tibble(prefix = "<"), "half_lor")
    Condition
      Error:
      ! `data` has no `detect_flag` column, so a detected result cannot be told from a non-detect. Read the results in with data_processor(), which adds it, before calling half_lor().


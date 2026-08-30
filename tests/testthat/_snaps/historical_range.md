# keep = exceedance needs a guideline

    Code
      historical_range(history_fixture(), keep = "exceedance", quiet = TRUE)
    Condition
      Error in `historical_range()`:
      ! `keep = "exceedance"` needs a guideline to compare against. Join one with join_action_levels() first.

# historical_range names the columns it cannot do without

    Code
      historical_range(data)
    Condition
      Error in `historical_range()`:
      ! `data` is missing required columns: detect_flag. Pass a table from data_processor().

# spike_factor must be a positive number

    Code
      historical_range(history_fixture(), spike_factor = -1, quiet = TRUE)
    Condition
      Error in `historical_range()`:
      ! `spike_factor` must be a positive number, or NULL to switch the check off.


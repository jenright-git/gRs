# exceedance_summary needs a guideline to have been joined

    Code
      exceedance_summary(exceedance_data())
    Condition
      Error in `exceedance_summary()`:
      ! `data` has no 'criteria' column, so nothing can be said to have exceeded anything. Join a guideline set onto it with join_action_levels(), or name the column it was joined into with `criteria_col`.

# exceedance_summary names the columns it cannot do without

    Code
      exceedance_summary(data)
    Condition
      Error in `exceedance_summary()`:
      ! `data` is missing required columns: detect_flag. Pass a table from data_processor().


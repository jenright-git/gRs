# lor_multiplier must be a single number, 0 or more

    Code
      summary_stats(chem_fixture(), lor_multiplier = -1)
    Condition
      Error:
      ! `lor_multiplier` must be a single number, 0 or more.

# group_vars names the columns `data` lacks

    Code
      summary_stats(chem_fixture(), group_vars = "zone")
    Condition
      Error in `summary_stats()`:
      ! `group_vars` names columns `data` does not have: zone.

# summary_stats names the columns it cannot do without

    Code
      summary_stats(data)
    Condition
      Error in `summary_stats()`:
      ! `data` is missing required columns: concentration. Pass a table from data_processor().

# include_criteria says which column to name when it finds none

    Code
      summary_stats(chem_fixture(), include_criteria = TRUE)
    Condition
      Error in `summary_stats()`:
      ! `data` has no 'criteria' column. Join a guideline set onto it with join_action_levels(), or name the column it was joined into with `criteria_col`.


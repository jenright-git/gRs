# a unitless guideline set is an error, not empty columns

    Code
      join_action_levels(chem_fixture(), action_level_fixture(criteria_unit = NA_character_))
    Condition
      Error in `join_action_levels()`:
      ! `action_levels` records no units, so no guideline can be compared against a result. Add the unit to the guideline file - either in the action level cell ("80 µg/L") or in a units column - and read it again with action_level_processor().

# two guideline sets at once is an error naming both

    Code
      join_action_levels(chem_fixture(), levels)
    Condition
      Error in `join_action_levels()`:
      ! `action_levels` holds more than one guideline set (Set A, Set B). Join one at a time, using a different `value_col` for each.

# criteria_long names the sets present when asked for one that is not

    Code
      criteria_long(compared, sets = "criteria_99")
    Condition
      Error in `criteria_long()`:
      ! No guideline set joined under: criteria_99. Sets present: criteria.


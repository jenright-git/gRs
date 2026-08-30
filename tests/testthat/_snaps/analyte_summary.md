# a round that matches nothing says which rounds are present

    Code
      analyte_summary(two_analytes(), round = "2099 Q4")
    Condition
      Error in `resolve_round()`:
      ! No result in `data` belongs to monitoring_round = 2099 Q4. Rounds present: 2024 Q1.

# analyte_summary names the columns it cannot do without

    Code
      analyte_summary(data)
    Condition
      Error in `analyte_summary()`:
      ! `data` is missing required columns: detect_flag. Pass a table from data_processor().

# group_vars must name real columns

    Code
      analyte_summary(two_analytes(), group_vars = "zone", quiet = TRUE)
    Condition
      Error in `analyte_summary()`:
      ! `group_vars` names columns `data` does not have: zone.


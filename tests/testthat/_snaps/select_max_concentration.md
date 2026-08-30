# a table with no sample_type column is returned, with a warning

    Code
      out <- select_max_concentration(data)
    Condition
      Warning in `select_max_concentration()`:
      No 'sample_type' column, so no duplicate samples can be identified and `data` is returned unchanged. Name the column holding sample types with `sample_type_col`.


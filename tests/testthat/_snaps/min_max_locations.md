# the round found and the rows reported are named

    Code
      min_max_locations(ranked_fixture(), n_min = 2)
    Message
      Reporting on the latest round in `data`: monitoring_round = 2024 Q1.
      6 results reported across 2 analytes in monitoring_round = 2024 Q1: the highest 1 and the lowest 2 of each.
    Output
      # A tibble: 6 x 18
        chem_name extreme  rank location_code date                prefix concentration
        <chr>     <chr>   <int> <chr>         <dttm>              <chr>          <dbl>
      1 Copper    Maximum     1 MW02          2024-05-14 00:00:00 <NA>             8  
      2 Copper    Minimum     1 MW01          2024-03-15 00:00:00 <                0.5
      3 Copper    Minimum     1 MW02          2024-06-13 00:00:00 <                0.5
      4 Zinc      Maximum     1 MW02          2024-05-14 00:00:00 <NA>             8  
      5 Zinc      Minimum     1 MW01          2024-03-15 00:00:00 <                0.5
      6 Zinc      Minimum     1 MW02          2024-06-13 00:00:00 <                0.5
      # i 11 more variables: output_unit <chr>, sampled_date_time <dttm>,
      #   site_id <chr>, monitoring_zone <chr>, chem_group <chr>, chem_code <chr>,
      #   fraction <chr>, detect_flag <chr>, sample_type <chr>, matrix_code <chr>,
      #   monitoring_round <chr>

# bad input is refused

    Code
      min_max_locations(ranked_fixture()["chem_name"], quiet = TRUE)
    Condition
      Error in `min_max_locations()`:
      ! `data` is missing required columns: location_code, concentration, detect_flag. Pass a table from data_processor().
    Code
      min_max_locations(ranked_fixture(), n_max = 0, n_min = 0, quiet = TRUE)
    Condition
      Error in `min_max_locations()`:
      ! `n_max` and `n_min` are both 0, so there is nothing to report. Ask for at least one result from one end of the range.
    Code
      min_max_locations(ranked_fixture(), n_max = -1, quiet = TRUE)
    Condition
      Error in `min_max_locations()`:
      ! `n_max` must be a single non-negative whole number.
    Code
      min_max_locations(ranked_fixture(), n_min = 1.5, quiet = TRUE)
    Condition
      Error in `min_max_locations()`:
      ! `n_min` must be a single non-negative whole number.
    Code
      min_max_locations(ranked_fixture(), group_vars = "zone", quiet = TRUE)
    Condition
      Error in `min_max_locations()`:
      ! `group_vars` names columns `data` does not have: zone.


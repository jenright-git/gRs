# nd_threshold and min_detects cannot both be given

    Code
      mann_kendall_test(trend_fixture(), nd_threshold = 0.5, min_detects = 2)
    Condition
      Error in `mann_kendall_test()`:
      ! Only one of `nd_threshold` or `min_detects` may be specified, not both.

# zero substitution beforehand leaves no LOR to flag

    Code
      out <- mann_kendall_test(half_lor(data, lor_multiplier = 0))
    Condition
      Warning in `mann_kendall_test()`:
      `data` has had its non-detects substituted at zero by half_lor(), so the LORs they were reported at are lost and `lor_changed`, `lor_min` and `lor_max` are NA for the series that hold them. Pass the results as reported and set `lor_multiplier = 0` here instead.


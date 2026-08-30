# nd_threshold and min_detects cannot both be given

    Code
      mann_kendall_test(trend_fixture(), nd_threshold = 0.5, min_detects = 2)
    Condition
      Error in `mann_kendall_test()`:
      ! Only one of `nd_threshold` or `min_detects` may be specified, not both.


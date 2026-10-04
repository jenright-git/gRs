# include_zone without the zone says how to group by it

    Code
      write_summary_stats(include_zone = TRUE)
    Condition
      Error in `summary_stats_to_excel()`:
      ! `data` has no column monitoring_zone. Group by it in summary_stats(), with group_vars = "monitoring_zone", or name the column with `zone_col`.

# results are turned away, with the summary_stats() call to make

    Code
      write_summary_stats(chem_fixture())
    Condition
      Error in `summary_stats_to_excel()`:
      ! `data` holds results, not a summary_stats() table. Summarise it first: data %>% summary_stats() %>% summary_stats_to_excel().

---

    Code
      write_summary_stats(two_sets_fixture())
    Condition
      Error in `summary_stats_to_excel()`:
      ! `data` holds results, not a summary_stats() table. Summarise it first: data %>% summary_stats(include_criteria = TRUE, criteria_col = c(criteria_95, criteria_99)) %>% summary_stats_to_excel().

# a table stacked by criteria_long() points at criteria_col

    Code
      write_summary_stats(summary_stats(criteria_long(two_sets_fixture()),
      include_criteria = TRUE))
    Condition
      Error in `summary_stats_to_excel()`:
      ! `data` comes from a table stacked by criteria_long(). Run summary_stats() on the table from join_action_levels() instead, and name the sets to write side by side in `criteria_col`, e.g. criteria_col = c(criteria_95, criteria_99).


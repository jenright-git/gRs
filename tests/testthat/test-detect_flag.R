test_that("only a < prefix makes a non-detect flag", {
  expect_equal(
    detect_flag_from_prefix(c("<", NA, "=", ">", "")),
    c("N", "Y", "Y", "Y", "Y")
  )
})

test_that("only a Y flag is a detect", {
  expect_equal(is_detect(c("Y", "N", NA, "y")), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(is_detect(factor(c("Y", "N"))), c(TRUE, FALSE))
})

test_that("the prefix shown is read from the flag", {
  expect_equal(display_prefix(c("Y", "N", NA)), c(NA, "<", "<"))
})

test_that("a detect above the lab's range keeps its >", {
  # only a detect's ">" is kept; the "<" still comes from the flag alone
  expect_equal(
    display_prefix(c("Y", "Y", "Y", "N", "N"), c(">", "=", NA, ">", NA)),
    c(">", NA, NA, "<", "<")
  )
})

test_that("a table with no detect_flag is an error naming data_processor()", {
  expect_snapshot(
    check_detect_flag(dplyr::tibble(prefix = "<"), "half_lor"),
    error = TRUE
  )
  expect_no_error(check_detect_flag(dplyr::tibble(detect_flag = "Y"), "x"))
})

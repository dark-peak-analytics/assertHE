test_that("get_foo_coverage works for example project", {
  # suppress messages because the covr package function
  # call inside of get_foo_coverage sources all files without stripping
  # out the non-function definitions.
  df <-
    #suppressMessages({
      get_foo_coverage(
        foo_folder = testthat::test_path("example_project", "R"),
        test_folder = testthat::test_path("tests", "testthat")
      )
    #})
  expect_true(nrow(df) > 0)
  expect_true("data.frame" %in% class(df))
  expect_true(all(between(
    x = df$coverage,
    left = 0,
    right = 1
  )))
  expect_true("character" %in% class(df$foo_string))

})

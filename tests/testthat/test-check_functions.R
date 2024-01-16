test_that("test no error when running folder function", {
  testthat::expect_silent(
    tmp <<- tabulate_functions_in_folder(
      path = ".",
      collapse = T,
      packages_to_exclude = c("base", "stats", "utils")
    )
  )

  testthat::expect_s3_class(
    tmp,
    "data.frame"
  )

  testthat::expect_type(
    tabulate_functions_in_folder(
      path = ".",
      collapse = F,
      packages_to_exclude = c("base", "stats", "utils")
    ),
    "list"
  )


  testthat::expect_type(
    tabulate_functions_in_folder(
      path = ".",
      collapse = F,
      packages_to_exclude = NULL
    ),
    "list"
  )

})




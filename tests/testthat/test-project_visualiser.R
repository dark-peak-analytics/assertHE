test_that("test that nothing blows up with visualiser run ", {
  # ignore if in coverage checks
  # breaks because we are doing coverage checks within the function itself.
  testthat::skip_if(covr::in_covr())

  # sicksicker project
  project_path <- testthat::test_path("example_project")

  expect_no_error({
    visualise_project(
      project_path = project_path,
      foo_path = "R",
      test_path = "tests/testthat",
      run_coverage = T
    )

  })

})

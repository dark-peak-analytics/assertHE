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
      run_coverage = FALSE,
      show_in_shiny = TRUE)

  })

})



test_that("Visualiser cx2cea model without coverage", {

  cdx2cea_project_path <- testthat::test_path("cdx2cea_master")

  expect_no_error({

    visualise_project(project_path = cdx2cea_project_path,
                      foo_path = "R",
                      test_path = "tests/testthat",
                      run_coverage = FALSE)

  })

})


# NOTE: CANNOT TEST COVERAGE ON VISUALISER BECAUSE OUR TEST TESTS THE COVERAGE
# WHICH THEN RECALLS TEST. IM SO CONFUSED AND GAVE UP!
# Need to manually run the code below to check a visual and error message appear.

# test_that("Visualiser cx2cea model with coverage", {
#   cdx2cea_project_path <- testthat::test_path("cdx2cea_master")
#
#   sink("nul")
#   vis_object <- visualise_project(
#     project_path = cdx2cea_project_path,
#     foo_path = "R",
#     test_path = "tests/testthat",
#     run_coverage = TRUE
#   )
#
#   sink()
#
#   testthat::expect_true("visNetwork" %in% class(vis_object))
#
# })



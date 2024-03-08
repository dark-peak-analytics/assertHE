test_that("Visualiser Sick Sicker ", {

  sicksicker_project_path <- testthat::test_path("example_project")

  expect_no_error({

  visualise_project(project_path = sicksicker_project_path,
                    foo_path = "R",
                    test_path = "tests/testthat",
                    run_coverage = T)

  })

})



test_that("Visualiser cx2cea model without coverage", {

  cdx2cea_project_path <- testthat::test_path("cdx2cea_master")

  expect_no_error({

    visualise_project(project_path = cdx2cea_project_path,
                      foo_path = "R",
                      test_path = "tests/testthat",
                      run_coverage = F)

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
#     run_coverage = T
#   )
#
#   sink()
#
#   testthat::expect_true("visNetwork" %in% class(vis_object))
#
# })



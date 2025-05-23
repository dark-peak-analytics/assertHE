# test_that("test no error when running folder function", {
#
#   #if (testthat::testing_package() != ""){
#   #  path <- dirname(dirname(getwd()))
#   #}else{
#   #  path <- getwd()
#   #}
#
#     testthat::expect_silent(
#       assertHE::tabulate_functions_in_folder(
#         path = testthat::test_path(),
#         path_exclude = NULL,
#         collapse = TRUE,
#         packages_to_exclude = c("base", "stats", "utils")
#       )
#     )
#
#     tmp <- assertHE::tabulate_functions_in_folder(
#       path = testthat::test_path("."),
#       path_exclude = NULL,
#       collapse = TRUE,
#       packages_to_exclude = c("base", "stats", "utils")
#     )
#
#     testthat::expect_s3_class(tmp,
#                               "data.frame")
#
#     testthat::expect_type(
#       assertHE::tabulate_functions_in_folder(
#         path = testthat::test_path("."),
#         path_exclude = NULL,
#         collapse = FALSE,
#         packages_to_exclude = c("base", "stats", "utils")
#       ),
#       "list"
#     )
#
#
#     testthat::expect_type(
#       assertHE::tabulate_functions_in_folder(
#         path = testthat::test_path("."),
#         path_exclude = NULL,
#         collapse = FALSE,
#         packages_to_exclude = NULL
#       ),
#       "list"
#     )
#
# })
#
#
#
#
# test_that("find_test can identify a test where it exists", {
#
#   #if (testthat::testing_package() != ""){
#   #  path <- dirname(dirname(getwd()))
#   #}else{
#   #  path <- getwd()
#   #}
#
#   #if(testthat::testing_package() == ""){
#     path_to_test1 <-
#       assertHE:::find_test(v_functions = "check_markov_trace",
#                            path = testthat::test_path("example_project"),
#                            test_path = "tests/testthat")
#
#     testthat::expect_length(object = path_to_test1, n = 1)
#
#     path_to_test2 <-
#       assertHE:::find_test(
#         v_functions = c("check_trans_prob_array", "mean"),
#         path = testthat::test_path("example_project"),
#         test_path = "tests/testthat"
#       )
#
#     testthat::expect_length(object = path_to_test2 , n = 2)
#   #}
# })
#
#
#
#
#
#
#
#
#
#
#
# test_that(
#   "tabulate_functions_in_folder_with_tests can identify functions, packages and test locations",
#   {
#     # if (testthat::testing_package() != "") {
#     #   path <- dirname(dirname(getwd()))
#     # } else{
#     #   path <- getwd()
#     # }
#
#     #if(testthat::testing_package() == ""){
#     testthat::expect_silent(
#       assertHE:::tabulate_functions_in_folder_with_tests(
#         path = testthat::test_path("example_project"),
#         path_exclude = NULL,
#         packages_to_exclude = c("base", "stats", "utils"),
#         test_path = "tests/testthat"
#       )
#     )
#
#     testthat::expect_silent(
#       assertHE:::tabulate_functions_in_folder_with_tests(
#         path = testthat::test_path("example_project"),
#         path_exclude = NULL,
#         packages_to_exclude = NULL,
#         test_path = "tests/testthat"
#       )
#     )
#
#
#     df_tests <-
#       assertHE:::tabulate_functions_in_folder_with_tests(
#         path = testthat::test_path("example_project"),
#         path_exclude = NULL,
#         packages_to_exclude = c("base", "stats", "utils"),
#         test_path = "tests/testthat"
#       )
#
#     df_tests2 <-
#       assertHE:::tabulate_functions_in_folder_with_tests(
#         path = testthat::test_path("example_project"),
#         path_exclude = NULL,
#         packages_to_exclude = NULL,
#         test_path = "tests/testthat"
#       )
#
#     testthat::expect_s3_class(df_tests,
#                               "data.frame")
#
#     testthat::expect_s3_class(df_tests2,
#                               "data.frame")
#
#     testthat::expect_equal(object = ncol(df_tests2),
#                            expected = ncol(df_tests))
#     testthat::expect_gt(object = nrow(df_tests2),
#                         expected = nrow(df_tests))
#
#     testthat::expect_false(object = {
#       "base" %in% df_tests$package
#     })
#     testthat::expect_true(object = {
#       "base" %in% df_tests2$package
#     })
#
#     #}
#   }
# )

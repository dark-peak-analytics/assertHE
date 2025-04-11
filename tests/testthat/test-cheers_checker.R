test_that("find_function_definitions works as intended",
    {
      expected = c(
        "do_something_random",
        "calculate_something",
        "find_matches",
        "perform_task",
        "combine_strings",
        "process_data",
        "transform_data",
        "sort_values",
        "generate_output",
        "do_everything",
        "lots_of_comments_foo",
        "foo_outside",
        "foo_inside"
      )


      object = find_function_definitions(
                    filename = testthat::test_path("example_scripts", "example_tricky_functions.R"))
      object <- object$text
      expect_equal(object, expected)
    })

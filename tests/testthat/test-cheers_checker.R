test_that("Extracting function names works as intended",
          {

  example1 <- " .function.name. <-

  function
  (a, b, c){


  }"

  example2 <- "function_name <- function(a, b, c){}"

  example3 <-"__function.name__ =



                      function(a, b, c){}"

  example4 <- "extract_function_name = function(string) {
  # does the string 'function' exist in the string
"

  example5 <- "function_name <-function(a){}"
  example6 <- "function_name =function(a){}"
  example7 <- "function_name<-function(a){}"
  example8 <- "function_name=function(a){}"

  exampleHORRID <- " .function.name. =  #comment
    function       # more comment
       (a)   # yet more comment
          {}"

  exampleComplicated <- "# Here are some comments for foo
    #' @family test

      foo <-

    #a comment here - function indented by whitespace too !

      function  ( a )

  {

            a <- some_function(x = a)
            a = another_function(x = 'hello')

          }"

  simple <- "   test_4  <-
  #' adding text here

        function(   x)   {y <- x/2
  return(y)

  }"


    function_commented <- "#' test_my_foo <-  function(){}
      myfoo <- function(){

    }"


  double_function <- "#' double function
  # two functions here, damn.

  myfoo1 <-


  function(){

  hello_function(a)
  }


  myfoo2 <- functon(){}"

  expect_equal(extract_function_name(example1), ".function.name.")
  expect_equal(extract_function_name(example2), "function_name")
  expect_equal(extract_function_name(example3), "__function.name__")
  expect_equal(extract_function_name(example4), "extract_function_name")
  expect_equal(extract_function_name(example5), "function_name")
  expect_equal(extract_function_name(example6), "function_name")
  expect_equal(extract_function_name(example7), "function_name")
  expect_equal(extract_function_name(example8), "function_name")
  expect_equal(extract_function_name(exampleHORRID), ".function.name.")
  expect_equal(extract_function_name(simple), "test_4")
  expect_equal(extract_function_name(exampleComplicated), "foo")
  expect_equal(extract_function_name(function_commented), "myfoo")   # NOT test_my_foo
  expect_equal(extract_function_name(double_function), "myfoo1")

  })





test_that("Next element after integer in vector works as intended",
{
  expect_equal(find_next_vector_element(10, 1:12), 11)
  expect_equal(find_next_vector_element(4, seq(1, 120, 5)), 6)
  expect_equal(find_next_vector_element(120, seq(1, 120, 5)), 116)
})


test_that("Previous element before integer in vector works as intended",
{
  expect_equal(find_previous_vector_element(10, 1:12), 9)
  expect_equal(find_previous_vector_element(4, seq(1, 120, 5)), 1)
  expect_equal(find_previous_vector_element(1, seq(1, 120, 5)), 1)
})









 test_that("get_file_cheers_classifications works for a few example scripts",
           {
             expect_silent({

               expect_equal(
                get_file_cheers_classifications(filename = testthat::test_path("example_scripts/create_markov_trace.R"),
                                                cheers_pattern = "@family"),
                 stats::setNames(object = "create_Markov_trace", nm =  "simulation")
               )

               expect_equal(
                 get_file_cheers_classifications(filename = testthat::test_path("example_scripts/define_transition_matrix.R"),
                                                 cheers_pattern = "@family"),
                 stats::setNames(object = "define_transition_matrix", nm =  "transitions")
               )

               expect_equal(
                get_file_cheers_classifications(filename = testthat::test_path("example_scripts/example_script.R"),
                                                cheers_pattern = "@family"),
                NA
               )

             })
           })



test_that("get_folder_cheers_classifications works for a few example folders",
          {
            expect_silent({
              get_folder_cheers_classifications(path = testthat::test_path("example_scripts"),
                                                cheers_pattern =  "@family")
            })
          })




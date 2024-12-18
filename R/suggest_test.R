#' @title Suggest a test for a function given its arguments and body
#'
#' @description This function suggests a test for a function given its arguments and body.
#'
#' @param foo_name The name of the function.
#' @param foo_arguments The arguments of the function.
#' @param foo_body The body of the function.
#' @param foo_title The title of the function.
#' @param foo_desc The description of the function.
#' @param model_name The name of the model to use. Default is "gpt-3.5-turbo-0125".
#' @param llm_api_url The URL of the LLM API. Default is the value of the environment variable "LLM_API_URL".
#' @param llm_api_key The API key for the LLM API. Default is the value of the environment variable "LLM_API_KEY".
#'
#' @return The response from the LLM API.
#'
#' @examples
#' \dontrun{
#' response <- suggest_test_from_arguments_and_body(
#'  foo_name = "my_function",
#'  foo_arguments = c("x", "y"),
#'  foo_body = c(
#'  "z <- x + y",
#'  "z"
#'  ),
#'  foo_title = "Add two numbers",
#'  foo_desc = "This function adds two numbers."
#'  )
#'  httr::content(response)
#'  }
suggest_test_from_arguments_and_body <- function(foo_name,
                                                 foo_arguments,
                                                 foo_body,
                                                 foo_title,
                                                 foo_desc,
                                                 model_name = "gpt-3.5-turbo-0125",
                                                 llm_api_url = Sys.getenv("LLM_API_URL"),
                                                 llm_api_key = Sys.getenv("LLM_API_KEY")) {

  context <- readLines(system.file("system_prompt_test.md", package = "assertHE")) |>
    paste0(collapse = "\n")

  func_json_data <-
    jsonlite::toJSON(
      list(
        "name" = paste0(as.character(foo_name), collapse = "\n"),
        "title" = paste0(as.character(foo_title), collapse = "\n"),
        "description" = paste0(as.character(foo_desc), collapse = "\n"),
        "arguments" = paste0(as.character(foo_arguments), collapse = "\n"),
        "body" = paste0(as.character(foo_body), collapse = "\n")
      )
    )

  # POST BODY
  body <- list(model = model_name,
               temperature = 0,
               messages = list(list(role = "system",
                                    content = context),
                               list(role = "user",
                                    content = func_json_data)))

  # Run the
  response <- httr::POST(
    # use chatGPT website (you can copy paste)
    url = llm_api_url,
    # Authorize
    config = httr::add_headers(Authorization = paste("Bearer", llm_api_key)),
    # Output type: use JSON
    httr::content_type_json(),
    # encode the value to json format
    encode = "json",
    # Controlling what to show as the output, it's going to be a list of following things
    body = body
  )
  # extracts the code from the LLM response & returns
  return(httr::content(response)$choices[[1]]$message$content)

}


write_test_to_file <- function(response, file_path = "tests/testthat/test-TEMP-AI.R") {
  # writes the code to a file
  writeLines(response, file_path)
  rstudioapi::navigateToFile(file_path)
  source(file_path)
}




function_to_check <- "create_Markov_trace"

source("tests/testthat/example_scripts/create_markov_trace.R")

dat <- get_function_data(function_to_check)

func_json_data <-
  jsonlite::toJSON(
    list(
      "name" = function_to_check,
      "title" = paste0(as.character(dat$title), collapse = "\n"),
      "description" = paste0(as.character(dat$desc), collapse = "\n"),
      "arguments" = paste0(as.character(dat$arguments), collapse = "\n"),
      "body" = paste0(as.character(dat$body), collapse = "\n")
    )
  )

response <- suggest_test_from_arguments_and_body(
  foo_name = function_to_check,
  foo_arguments = dat$arguments,
  foo_body = dat$body,
  foo_title = dat$title,
  foo_desc = dat$desc,
  model_name = "gpt-3.5-turbo-0125",
  llm_api_url = Sys.getenv("LLM_API_URL"),
  llm_api_key = Sys.getenv("LLM_API_KEY")
)

write_test_to_file(response = response,
                   file_path = "tests/testthat/test-TEMP-AI.R")

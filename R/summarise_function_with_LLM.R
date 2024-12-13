#' Summarize a function using a Large Language Model
#'
#' This function summarizes another function using a Language Model.
#'
#' @inheritParams create_prompt
#' @inheritParams summarise_function_from_arguments_and_body
#' @param envir The environment in which to look for the function.
#'
#' @return A character string with a summary of the function based on its arguments and body.
#'
#' @export
#' @examples
#' \dontrun{
#' summarise_function_with_LLM(foo_name = "get_active_functions",
#'                             envir = rlang::ns_env("assertHE"))
#' }
#'
summarise_function_with_LLM <- function(foo_name,
                                        llm_api_url = Sys.getenv("LLM_API_URL"),
                                        llm_api_key = Sys.getenv("LLM_API_KEY"),
                                        envir = environment()){

  # get the function data list (function arguments and body)
  l_foo_data <- get_function_data(foo_name, envir = envir)

  # API request created and sent
  response <-
    summarise_function_from_arguments_and_body(
      foo_name = foo_name,
      foo_arguments = l_foo_data[["arguments"]],
      foo_body = l_foo_data[["body"]],
      foo_title = l_foo_data[["title"]],
      foo_desc = l_foo_data[["desc"]],
      model_name = "gpt-3.5-turbo-0125",
      llm_api_url = llm_api_url,
      llm_api_key = llm_api_key
    )

  # message cleaned and returned
  response <- return_message(API_response = response)

  # wrap the string to a more reasonable line length
  response_wrapped <- wrap_string(input_string = response, width = 50)

  return(response_wrapped)

}


#' Retrieve Function data to a list
#'
#' This function retrieves data about the arguments and body of a specified function.
#'
#' @param foo_name The name of the function to retrieve data from.
#' @param envir The environment in which to look for the function.
#' @return A list with elements for 'arguments' and 'body' of the specified function.
#' @export
#' @importFrom methods formalArgs
#' @examples
#' \dontrun{
#' get_function_data(foo_name = "create_Markov_trace")
#' }
get_function_data <- function(foo_name, envir = environment()) {

  # get data on arguments and body
  foo_arguments <- names(formals(foo_name, envir = envir))
  #foo_body      <- base::body(foo_name)
  fun <- get(foo_name, mode = "function", envir = envir)
  foo_body <- body(fun)


  descriptors   <- tryCatch({
    get_roxygen_description_from_foo(foo_name = foo_name)
  }, error = function(e)
    NULL)

  # create list containing data
  output <- list(name = foo_name,
                 arguments = foo_arguments,
                 body      = foo_body,
                 title     = descriptors$title,
                 desc      = descriptors$description)

  return(output)
}


#' Summarise a function from its arguments and body
#'
#' Summarise a function using a LLM via API and retrieve the result
#'
#' @param foo_arguments vector of arguments
#' @param foo_body single character containing the unparsed body
#' @param model_name name of the LLM to use (default gpt-3.5-turbo-0125)
#' @param llm_api_url url to the API for the LLM
#' @param llm_api_key key for the API for the LLM
#' @inheritParams create_prompt
#'
#' @return response from LLM containing all pertinant information & tokens used
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tmp <- summarise_function_from_arguments_and_body(
#'   foo_arguments = LETTERS[1:3],
#'   foo_body = "D <- A+B+C; return(D)",
#'   model_name = "gpt-3.5-turbo-0125",
#'   llm_api_url = Sys.getenv("LLM_API_URL"),
#'   llm_api_key = Sys.getenv("LLM_API_KEY"),
#'   foo_desc = "add three numbers, these numbers relate to the number of apples on three trees",
#'   foo_title = "apple adder",
#'   foo_name = "apple_add"
#' )
#' httr::content(tmp)
#' }
#'
summarise_function_from_arguments_and_body <- function(foo_name,
                                                       foo_arguments,
                                                       foo_body,
                                                       foo_title,
                                                       foo_desc,
                                                       model_name = "gpt-3.5-turbo-0125",
                                                       llm_api_url = Sys.getenv("LLM_API_URL"),
                                                       llm_api_key = Sys.getenv("LLM_API_KEY")) {

  context <- readLines(system.file("system_prompt.md", package = "assertHE")) |>
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

  return(response)

}

#' Create a prompt for a LLM
#'
#' Uses the function arguments and function body as inputs to create a prompt
#' for the LLM.
#'
#' @param foo_arguments the arguments to the function
#' @param foo_body      the body of the function
#' @param foo_name      function name
#' @param foo_title     function title
#' @param foo_desc      function description
#'
#' @return a single prompt in the form of a character string
#' @export
#'
#' @examples
#' \dontrun{
#' create_prompt(
#' foo_arguments = LETTERS[1:3],
#' foo_body = "D <- A+B+C; return(D)",
#' foo_name = "calculate_QALYs",
#' foo_desc = "This function calcs QALYs",
#' foo_title = "Calculate the QALYs")
#' }
create_prompt <- function(foo_arguments,
                          foo_body,
                          foo_name,
                          foo_desc,
                          foo_title){

  prompt <- paste0(
    collapse = " ",
    c(
      "I am reviewing some R code. I have a single function called '",
      paste0(foo_name),
      paste0("' with a title, description, arguments and a body. Summarise what the function does in two paragraphs,"),
      paste0("returning your output as HTML code. The Roxygen title and description is as follows"),
      paste0("title: '", foo_title, "';"),
      paste0("description: '", foo_desc, "';"),
      paste0("The function arguments are: "),
      paste(collapse = ", ",  foo_arguments),
      paste0("; The function body is:"),
      foo_body
    )
  )

  return(prompt)
}




#' Extract the content from the output of the LLM
#'
#' Extracts content and prints the number of tokens used as a message.
#'
#' @param API_response response from the LLM API
#' @param verbose whether to include the message for the number of token's used
#'
#' @return A single string summary of the content of the LLM response
return_message <- function(API_response,
                           verbose = T){

  if (verbose) message(paste("Tokens used:", httr::content(API_response)$usage$total_tokens))

  output <- httr::content(API_response)

  out <- output[["choices"]][[1]][["message"]][["content"]]

  return(out)
}


#' Get roxygen title and description from function
#'
#' @param foo_name function for which want descriotion
#'
#' @return text containing description
#' @importFrom roxygen2 parse_file
#' @examples
#' \dontrun{
#' source_files(path = "tests/testthat/example_project/R", keep_source = TRUE)
#' get_roxygen_description_from_foo("calculate_costs")
#' }
get_roxygen_description_from_foo <- function(foo_name) {
  # get the name of the file containing the function
  filename <- utils::getSrcFilename(x = eval(parse(text = foo_name)),
                             full.names = TRUE)
  # parse the entire file
  parsed <- roxygen2::parse_file(file = filename)

  # get the block with the function in it
  thisfn <- which(sapply(parsed,
                         function(block) {
                           block$line == utils::getSrcLocation(eval(parse(text = foo_name)), "line")
                         }))

  # extract a list of tags
  parsed_list <- parsed[[thisfn]]$tags

  # get the roxygen description
  desc_text <- get_roxygen_description(parsed_list = parsed_list)

  return(desc_text)

}




#' Get Title and Description from Parsed List
#'
#' This function extracts the title and description from a parsed list.
#'
#' @param parsed_list A list containing parsed elements.
#' @return A list containing the title and description.
#' @export
#' @examples
#' \dontrun{
#' parsed_list <- list(list(tag = "title", val = "Sample Title"),
#'                      list(tag = "description", val = "This is a sample description."))
#' get_roxygen_description(parsed_list)
#' }
get_roxygen_description <- function(parsed_list) {
  desc_index <- sapply(
    X = parsed_list,
    FUN = function(x) {
      x$tag == "description"
    }
  ) |> which()

  description <- parsed_list[[desc_index]][["val"]]

  title_index <- sapply(
    X = parsed_list,
    FUN = function(x) {
      x$tag == "title"
    }
  ) |> which()

  title <- parsed_list[[title_index]][["val"]]

  return(list("title" = title,
              "description" = sub(x =  description, pattern = "\n", replacement = " ")))
}

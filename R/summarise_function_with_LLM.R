#' Summarize a function using a Large Language Model
#'
#' This function summarizes another function using a Language Model.
#'
#' @inheritParams create_prompt
#' @inheritParams summarise_function_from_arguments_and_body
#'
#' @return A character string with a summary of the function based on its arguments and body.
#'
#' @export
#' @examples
#' \dontrun{
#' summarise_function_with_LLM("check_init")
#' }
#'
summarise_function_with_LLM <- function(foo_name,
                                        text_language = "English",
                                        llm_api_url = Sys.getenv("LLM_API_URL"),
                                        llm_api_key = Sys.getenv("LLM_API_KEY")){
  # get the function data list (function arguments and body)
  l_foo_data <- get_function_data(foo_name)

  # API request created and sent
  response <-
    summarise_function_from_arguments_and_body(
      foo_name = foo_name,
      foo_arguments = l_foo_data[["arguments"]],
      foo_body = l_foo_data[["body"]],
      foo_title = l_foo_data[["title"]],
      foo_desc = l_foo_data[["desc"]],
      text_language = text_language,
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
#' @return A list with elements for 'arguments' and 'body' of the specified function.
#' @export
#' @importFrom methods formalArgs
#' @examples
#' \dontrun{
#' get_function_data(foo_name = "create_Markov_trace")
#' }
get_function_data <- function(foo_name) {
  # get data on arguments and body
  foo_arguments <- methods::formalArgs(def = foo_name)
  foo_body      <- base::body(foo_name)
  descriptors   <- tryCatch({
    get_roxygen_description_from_foo(foo_name = foo_name)
  }, error = function(e)
    NULL)

  # create list containing data
  output <- list(arguments = foo_arguments,
                 body      = foo_body,
                 title     = descriptors$title,
                 desc   = descriptors$description)

  #if(!is.null(descriptors)){
  #  output$title  = descriptors$title
  #  output$desc   = descriptors$description
  #}

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
                                                       text_language = "English",
                                                       model_name = "gpt-3.5-turbo-0125",
                                                       llm_api_url = Sys.getenv("LLM_API_URL"),
                                                       llm_api_key = Sys.getenv("LLM_API_KEY")) {
  # create the prompt:
  prompt <- create_prompt(foo_arguments = foo_arguments,
                          foo_body =  foo_body,
                          foo_name = foo_name,
                          foo_title = foo_title,
                          foo_desc = foo_desc,
                          text_language = text_language)

  # POST BODY
  body <- list(model = model_name,
               messages = list(list(role = "user",
                                    content = prompt)))

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
#' @param text_language the language to return the summary in
#'
#' @return a single prompt in the form of a character string
#' @export
#'
#' @examples
#' \dontrun{
#' create_prompt(
#' text_language = "Spanish",
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
                          foo_title,
                          text_language = "English"){

  prompt <- paste0(
    collapse = " ",
    c(
      paste0("I am reviewing some R code. I have a single function called"),
      paste0("'", foo_name, "'"),
      paste0("with a title, description, arguments and a body. Summarise what the function does in two paragraphs,"),
      paste0("in the language '", text_language, "' using the fewest tokens possible."),
      paste0("The Roxygen title and description is as follows."),
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

  #string <- paste0(paste0("Title: ", title), paste0("; Description: ", description))

  #out <- sub(x =  string, pattern = "\n", replacement = " ")

  return(list("title" = title,
              "description" = sub(x =  description, pattern = "\n", replacement = " ")))
}


#get_function_data("define_transition_matrix")
#

#get_roxygen_description_from_foo("define_transition_matrix")




# foo_name <- "check_markov_trace"
# l_function_data <- get_function_data(foo_name)
# create_prompt(l_function_data$arguments, l_function_data$body)

# call function from R
# response <- summarise_function_with_LLM(foo_name = "define_transition_matrix")


# convert_to_html <- function(input_string) {
#   # Replace inline code snippets within `` with <code> tags
#   html_code <- gsub("`([^`]+)`", "<code>\\1</code>", input_string)
#   # Wrap the whole string within <p> tags
#   html_code <- paste("<p>", html_code, "</p>")
#   return(html_code)
# }

# Test the function
#html_output <- convert_to_html(response)


# writeLines(html_output, con = here::here("example.html"))
# (code to write some content to the file)
#rstudioapi::viewer("example.html")

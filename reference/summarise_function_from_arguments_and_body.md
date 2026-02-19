# Summarise a function from its arguments and body

Summarise a function using a LLM via API and retrieve the result

## Usage

``` r
summarise_function_from_arguments_and_body(
  foo_name,
  foo_arguments,
  foo_body,
  foo_title,
  foo_desc,
  model_name = "gpt-3.5-turbo-0125",
  llm_api_url = Sys.getenv("LLM_API_URL"),
  llm_api_key = Sys.getenv("LLM_API_KEY")
)
```

## Arguments

- foo_name:

  function name

- foo_arguments:

  vector of arguments

- foo_body:

  single character containing the unparsed body

- foo_title:

  function title

- foo_desc:

  function description

- model_name:

  name of the LLM to use (default gpt-3.5-turbo-0125)

- llm_api_url:

  url to the API for the LLM

- llm_api_key:

  key for the API for the LLM

## Value

response from LLM containing all pertinant information & tokens used

## Examples

``` r
if (FALSE) { # \dontrun{
tmp <- summarise_function_from_arguments_and_body(
  foo_arguments = LETTERS[1:3],
  foo_body = "D <- A+B+C; return(D)",
  model_name = "gpt-3.5-turbo-0125",
  llm_api_url = Sys.getenv("LLM_API_URL"),
  llm_api_key = Sys.getenv("LLM_API_KEY"),
  foo_desc = "add three numbers, these numbers relate to the number of apples on three trees",
  foo_title = "apple adder",
  foo_name = "apple_add"
)
httr::content(tmp)
} # }
```

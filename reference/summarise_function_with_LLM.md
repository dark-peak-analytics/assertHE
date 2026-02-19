# Summarize a function using a Large Language Model

This function summarizes another function using a Language Model.

## Usage

``` r
summarise_function_with_LLM(
  foo_name,
  llm_api_url = Sys.getenv("LLM_API_URL"),
  llm_api_key = Sys.getenv("LLM_API_KEY"),
  envir = environment()
)
```

## Arguments

- foo_name:

  function name

- llm_api_url:

  url to the API for the LLM

- llm_api_key:

  key for the API for the LLM

- envir:

  The environment in which to look for the function.

## Value

A character string with a summary of the function based on its arguments
and body.

## Examples

``` r
if (FALSE) { # \dontrun{
summarise_function_with_LLM(foo_name = "get_active_functions",
                            llm_api_url = Sys.getenv("LLM_API_URL"),
                            llm_api_key = Sys.getenv("LLM_API_KEY"),
                            envir = rlang::ns_env("assertHE"))
} # }
```

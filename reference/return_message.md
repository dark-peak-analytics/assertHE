# Extract the content from the output of the LLM

Extracts content and prints the number of tokens used as a message.

## Usage

``` r
return_message(API_response, verbose = TRUE)
```

## Arguments

- API_response:

  response from the LLM API

- verbose:

  whether to include the message for the number of token's used

## Value

A single string summary of the content of the LLM response

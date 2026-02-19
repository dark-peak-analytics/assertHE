# Create a prompt for a LLM

Uses the function arguments and function body as inputs to create a
prompt for the LLM.

## Usage

``` r
create_prompt(foo_arguments, foo_body, foo_name, foo_desc, foo_title)
```

## Arguments

- foo_arguments:

  the arguments to the function

- foo_body:

  the body of the function

- foo_name:

  function name

- foo_desc:

  function description

- foo_title:

  function title

## Value

a single prompt in the form of a character string

## Examples

``` r
create_prompt(
foo_arguments = LETTERS[1:3],
foo_body = "D <- A+B+C; return(D)",
foo_name = "calculate_QALYs",
foo_desc = "This function calcs QALYs",
foo_title = "Calculate the QALYs")
#> [1] "I am reviewing some R code. I have a single function called ' calculate_QALYs ' with a title, description, arguments and a body. Summarise what the function does in two paragraphs, returning your output as HTML code. The Roxygen title and description is as follows title: 'Calculate the QALYs'; description: 'This function calcs QALYs'; The function arguments are:  A, B, C ; The function body is: D <- A+B+C; return(D)"
```

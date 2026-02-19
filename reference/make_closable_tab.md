# Create closable shiny tab

Create closable shiny tab

## Usage

``` r
make_closable_tab(tab_name, content_output_Id, output_type = "text")
```

## Arguments

- tab_name:

  Character scalar representing the name or title of the shiny tab.

- content_output_Id:

  Character scalar representing the id of the shiny tab.

- output_type:

  Character scalar specifying the type of rendered output. Default is
  `"text"` and can also accept `"HTML"`.

## Value

A tab that can be passed to
[`shiny::tabsetPanel()`](https://rdrr.io/pkg/shiny/man/tabsetPanel.html)

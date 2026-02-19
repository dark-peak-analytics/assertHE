# Run a Shiny app to host a network visualization

Run a Shiny app to host a network visualization

## Usage

``` r
run_shiny_app(
  uiFunction = define_app_ui,
  serverFunction = define_app_server,
  network_object,
  network_title = "Function Network",
  project_path,
  foo_path
)
```

## Arguments

- uiFunction:

  Function defining shiny user-interface

- serverFunction:

  Function defining shiny server logic

- network_object:

  visNetwork object to be displayed in the shiny app

- network_title:

  Title to be displayed in hte app above the title

- project_path:

  Path to the project directory

- foo_path:

  Path to the function folder

## Value

A shiny app

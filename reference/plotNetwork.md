# Plot Network

Visualize a network plot using the visNetwork package.

## Usage

``` r
plotNetwork(
  df_edges,
  from_col = "from",
  to_col = "to",
  df_summary,
  df_coverage,
  color_no_test = c(background = "#fad1d0", border = "#9c0000", highlight = "#9c0000"),
  color_with_test = c(background = "#e6ffe6", border = "#65a765", highlight = "#65a765"),
  color_mod_coverage = c(background = "#FFD580", border = "#E49B0F", highlight =
    "#E49B0F"),
  moderate_coverage_range = c(0.2, 0.8),
  show_in_shiny = FALSE,
  network_title = NULL,
  scale_node_size_by_degree = FALSE
)
```

## Arguments

- df_edges:

  A data frame containing columns "from" and "to" representing the edges
  of the network.

- from_col:

  Name of the column in df_edges representing the source nodes.

- to_col:

  Name of the column in df_edges representing the target nodes.

- df_summary:

  A summary dataframe containing the information about each function.

- df_coverage:

  a summary dataframe with function names and test coverages

- color_no_test:

  named vector with hexcodes for background, border and highlight

- color_with_test:

  named vector with hexcodes for background, border and highlight

- color_mod_coverage:

  named vector with hexcodes for background, border and highlight where
  coverage moderate

- moderate_coverage_range:

  vector of two values giving range defined as moderate coverage.

- show_in_shiny:

  logical scalar indicating whether to prepare/deploy the network using
  a built in shiny app. Default is `FALSE`.

- network_title:

  title of the network plot.

- scale_node_size_by_degree:

  Scale the node size by the degree centrality of the node.

## Value

A visNetwork object representing the network plot.

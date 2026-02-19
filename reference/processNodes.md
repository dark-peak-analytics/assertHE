# Process Nodes

Process unique nodes from a dataframe of edges.

## Usage

``` r
processNodes(df_edges, from_col = "from", to_col = "to")
```

## Arguments

- df_edges:

  A data frame containing columns "from" and "to" representing the edges
  of the network.

- from_col:

  Name of the column in df_edges representing the source nodes.

- to_col:

  Name of the column in df_edges representing the target nodes.

## Value

A data frame of unique nodes with labels.

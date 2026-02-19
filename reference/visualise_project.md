# Visualize Project

Visualize the dependencies between functions in a project using a
network plot.

## Usage

``` r
visualise_project(
  project_path,
  foo_path = "R",
  test_path = NULL,
  exclude_files = NULL,
  exclude_dirs = NULL,
  run_coverage = FALSE,
  color_no_test = c(background = "#fad1d0", border = "#9c0000", highlight = "#9c0000"),
  color_with_test = c(background = "#e6ffe6", border = "#65a765", highlight = "#65a765"),
  color_mod_coverage = c(background = "#FFD580", border = "#E49B0F", highlight =
    "#E49B0F"),
  moderate_coverage_range = c(0.2, 0.8),
  print_isolated_foo = FALSE,
  show_in_shiny = FALSE,
  network_title = "Function Network",
  scale_node_size_by_degree = TRUE
)
```

## Arguments

- project_path:

  Path to the project folder.

- foo_path:

  Path to the folder containing foo functions.

- test_path:

  Path to the folder containing test functions.

- exclude_files:

  A regular expression for files to NOT process (basename)

- exclude_dirs:

  A regular expression for directories to NOT process (dirname)

- run_coverage:

  Boolean determining whether to run coverage assessment

- color_no_test:

  named vector with hexcodes for background, border and highlight

- color_with_test:

  named vector with hexcodes for background, border and highlight

- color_mod_coverage:

  named vector with hexcodes for background, border and highlight where
  coverage moderate

- moderate_coverage_range:

  vector of two values giving range defined as moderate coverage.

- print_isolated_foo:

  Print the isolated functions to the console (default false)

- show_in_shiny:

  logical scalar indicating whether to prepare/deploy the network using
  a built in shiny app. Default is `FALSE`.

- network_title:

  title of the network plot.

- scale_node_size_by_degree:

  Scale the node size by the degree centrality of the node.

## Value

A visNetwork object representing the network plot of function
dependencies.

## Examples

``` r
# \donttest{
# Example takes more than 5 seconds to run
# Visualize project dependencies in HTML
if(require(testthat)) {
  folder_path <- assertHE_example("example_project")
  visualise_project(
    project_path = folder_path,
    foo_path = "R",
    test_path = "tests/testthat",
    run_coverage = TRUE
  )
}
#> Test passed with 2 successes 🎉.
#> Test passed with 2 successes 😸.
#> Test passed with 6 successes 🎊.
#> Test passed with 2 successes 🥳.
#> Test passed with 3 successes 🥇.
#> Test passed with 2 successes 😀.
#> Test passed with 1 success 🎊.

{"x":{"nodes":{"id":["calculate_QALYs","calculate_costs","calculate_discounting_weights","create_Markov_trace","define_transition_matrix","mat_mult","run_sickSicker_model","utility_example","utility_example2"],"label":["calculate_QALYs","calculate_costs","calculate_discounting_weights","create_Markov_trace","define_transition_matrix","mat_mult","run_sickSicker_model","utility_example","utility_example2"],"title":["<b>Foo Name<\/b>: calculate_QALYs<br><b>Foo Location<\/b>: R/calculate_QALYs.R#L40<br><b>Test location<\/b>: tests/testthat/test-calculate_QALYs.R#L26<br><b>Coverage<\/b>: 100%","<b>Foo Name<\/b>: calculate_costs<br><b>Foo Location<\/b>: R/calculate_costs.R#L39<br><b>Test location<\/b>: tests/testthat/test-calculate_costs.R#L33<br><b>Coverage<\/b>: 100%","<b>Foo Name<\/b>: calculate_discounting_weights<br><b>Foo Location<\/b>: R/calculate_discounting_weights.R#L29<br><b>Test location<\/b>: tests/testthat/test-calculate_discounting_weights.R#L8<br><b>Coverage<\/b>: 100%","<b>Foo Name<\/b>: create_Markov_trace<br><b>Foo Location<\/b>: R/create_markov_trace.R#L44<br><b>Test location<\/b>: tests/testthat/test-create_Markov_trace.R#L30<br><b>Coverage<\/b>: 100%","<b>Foo Name<\/b>: define_transition_matrix<br><b>Foo Location<\/b>: R/define_transition_matrix.R#L29<br><b>Test location<\/b>: tests/testthat/test-define_transition_matrix.R#L17<br><b>Coverage<\/b>: 100%","<b>Foo Name<\/b>: mat_mult<br><b>Foo Location<\/b>: R/calculate_QALYs.R#L107<br><b>Test location<\/b>: NA<br><b>Coverage<\/b>: 0%","<b>Foo Name<\/b>: run_sickSicker_model<br><b>Foo Location<\/b>: R/run_sickSicker_model.R#L36<br><b>Test location<\/b>: tests/testthat/test-run_sickSicker_model.R#L32<br><b>Coverage<\/b>: 100%","<b>Foo Name<\/b>: utility_example<br><b>Foo Location<\/b>: R/utils.R#L3<br><b>Test location<\/b>: tests/testthat/test-utility_functions_with_different_name.R#L5<br><b>Coverage<\/b>: 60%","<b>Foo Name<\/b>: utility_example2<br><b>Foo Location<\/b>: R/utils.R#L14<br><b>Test location<\/b>: NA<br><b>Coverage<\/b>: 0%"],"color.background":["#e6ffe6","#e6ffe6","#e6ffe6","#e6ffe6","#e6ffe6",null,"#e6ffe6","#FFD580","#fad1d0"],"color.border":["#65a765","#65a765","#65a765","#65a765","#65a765",null,"#65a765","#E49B0F","#9c0000"],"color.highlight":["#65a765","#65a765","#65a765","#65a765","#65a765",null,"#65a765","#E49B0F","#9c0000"],"value":[0.2,0.2,0.2,0.2,0.2,0.2,1,0.2,0.2]},"edges":{"from":["run_sickSicker_model","run_sickSicker_model","run_sickSicker_model","run_sickSicker_model","run_sickSicker_model"],"to":["calculate_discounting_weights","define_transition_matrix","create_Markov_trace","calculate_costs","calculate_QALYs"]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"nodes":{"shape":"dot"},"manipulation":{"enabled":true},"edges":{"arrows":"from"},"height":"600px","width":"1000px"},"groups":null,"width":"1000px","height":"600px","idselection":{"enabled":true,"style":"width: 150px; height: 26px","useLabels":true,"main":"Select by id"},"byselection":{"enabled":false,"style":"width: 150px; height: 26px","multiple":false,"hideColor":"rgba(200,200,200,0.5)","highlight":false},"main":{"text":"Function Network","style":"font-family:Georgia, Times New Roman, Times, serif;font-weight:bold;font-size:20px;text-align:center;"},"submain":{"text":"Functions without a test are <a style=\"color:#9c0000;\">red<\/a> and those with a test are <a style=\"color:#65a765;\">green<\/a>. Hover over nodes for more information.","style":"font-family:Calibri; font-size:15px; text-align:center;"},"footer":{"text":"<a href=\"https://github.com/dark-peak-analytics/assertHE/\">Created with assertHE<\/a>","style":"font-family:Georgia, Times New Roman, Times, serif;font-size:12px;text-align:center;"},"background":"rgba(0, 0, 0, 0)","opts_manipulation":{"datacss":"table.legend_table {\n  font-size: 11px;\n  border-width:1px;\n  border-color:#d3d3d3;\n  border-style:solid;\n}\ntable.legend_table td {\n  border-width:1px;\n  border-color:#d3d3d3;\n  border-style:solid;\n  padding: 2px;\n}\ndiv.table_content {\n  width:80px;\n  text-align:center;\n}\ndiv.table_description {\n  width:100px;\n}\n\n.operation {\n  font-size:20px;\n}\n\n.network-popUp {\n  display:none;\n  z-index:299;\n  width:250px;\n  /*height:150px;*/\n  background-color: #f9f9f9;\n  border-style:solid;\n  border-width:1px;\n  border-color: #0d0d0d;\n  padding:10px;\n  text-align: center;\n  position:fixed;\n  top:50%;  \n  left:50%;  \n  margin:-100px 0 0 -100px;  \n\n}","addNodeCols":["id","label"],"editNodeCols":["id","label"],"tab_add_node":"<span id=\"addnode-operation\" class = \"operation\">node<\/span> <br><table style=\"margin:auto;\"><tr><td>id<\/td><td><input id=\"addnode-id\"  type= \"text\" value=\"new value\"><\/td><\/tr><tr><td>label<\/td><td><input id=\"addnode-label\"  type= \"text\" value=\"new value\"><\/td><\/tr><\/table><input type=\"button\" value=\"save\" id=\"addnode-saveButton\"><\/button><input type=\"button\" value=\"cancel\" id=\"addnode-cancelButton\"><\/button>","tab_edit_node":"<span id=\"editnode-operation\" class = \"operation\">node<\/span> <br><table style=\"margin:auto;\"><tr><td>id<\/td><td><input id=\"editnode-id\"  type= \"text\" value=\"new value\"><\/td><\/tr><tr><td>label<\/td><td><input id=\"editnode-label\"  type= \"text\" value=\"new value\"><\/td><\/tr><\/table><input type=\"button\" value=\"save\" id=\"editnode-saveButton\"><\/button><input type=\"button\" value=\"cancel\" id=\"editnode-cancelButton\"><\/button>"},"highlight":{"enabled":true,"hoverNearest":false,"degree":{"from":9,"to":9},"algorithm":"hierarchical","hideColor":"rgba(200,200,200,0.5)","labelOnly":true},"collapse":{"enabled":true,"fit":false,"resetHighlight":true,"clusterOptions":null,"keepCoord":true,"labelSuffix":"(cluster)"}},"evals":[],"jsHooks":[]}
# Visualize project dependencies in shiny
if(interactive()) {
  visualise_project(
    project_path = folder_path,
    foo_path = "R",
    test_path = "tests/testthat",
    run_coverage = TRUE,
    show_in_shiny = TRUE
  )
}
# }
```

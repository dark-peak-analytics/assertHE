#' Visualize Project
#'
#' Visualize the dependencies between functions in a project using a network plot.
#'
#' @inheritParams plotNetwork
#' @param project_path Path to the project folder.
#' @param foo_path Path to the folder containing foo functions.
#' @param test_path Path to the folder containing test functions.
#' @param exclude_files A regular expression for files to NOT process (basename)
#' @param exclude_dirs A regular expression for directories to NOT process (dirname)
#' @param run_coverage Boolean determining whether to run coverage assessment
#' @param print_isolated_foo Print the isolated functions to the console (default false)
#'
#' @return A visNetwork object representing the network plot of function dependencies.
#'
#' @examples
#' \dontrun{
#' # Visualize project dependencies in HTML
#' visualise_project(
#'   project_path = "tests/testthat/example_project",
#'   foo_path = "R",
#'   test_path = "tests/testthat",
#'   run_coverage = TRUE
#' )
#'
#' # Visualize project dependencies in shiny
#' visualise_project(
#'   project_path = "tests/testthat/example_project",
#'   foo_path = "R",
#'   test_path = "tests/testthat",
#'   run_coverage = TRUE,
#'   show_in_shiny = TRUE
#' )
#' }
#' @export
visualise_project <- function(project_path,
                              foo_path = "R",
                              test_path = NULL,
                              exclude_files = NULL,
                              exclude_dirs = NULL,
                              run_coverage = F,
                              color_no_test = c("background" = "#fad1d0", "border" = "#9c0000", "highlight" = "#9c0000"),
                              color_with_test = c("background" = "#e6ffe6", "border" = "#65a765", "highlight" = "#65a765"),
                              color_mod_coverage = c("background" = "#FFD580", "border" = "#E49B0F", "highlight" = "#E49B0F"),
                              moderate_coverage_range = c(0.2, 0.8),
                              print_isolated_foo = FALSE,
                              show_in_shiny = FALSE,
                              network_title = "Function Network",
                              scale_node_size_by_degree = TRUE) {

  # Check folder existence
  stopifnot(dir.exists(project_path),
            dir.exists(paste0(project_path,"/", foo_path)),
            dir.exists(paste0(project_path,"/", test_path)))

  # Load and summarize the model
  df_summary <- summarise_model(project_path = project_path,
                                foo_folder = foo_path,
                                exclude_files = exclude_files,
                                exclude_dirs = exclude_dirs,
                                test_folder = test_path)

  # if test path is null then don't include them in summary...
  test_path <- if (is.null(test_path)) {
    NULL
  } else{
    paste0(project_path, "/", test_path)
  }

  # get the full function paths including project path and use from here on wards
  foo_path <- paste0(project_path, "/", foo_path)

  # get test coverage
  if (run_coverage) {
    df_coverage <-
      get_foo_coverage(foo_folder  = foo_path,
                       test_folder = test_path)
  }else{
    df_coverage <- NULL
  }

  # Create a new environment to avoid sourcing scripts into the namespace
  pkg_env <- new.env(parent = baseenv())

  # Load all functions into this environment
  load_functions_into_env(foo_path, pkg_env)

  # Identify function dependencies and create a network plot
  df_edges <- identify_dependencies(v_unique_foo = df_summary$foo_string,
                                    pkg_env = pkg_env)

  # identify which functions are isolated (not child or parent of any other function)
  # this means that there is no to (or its NA), and no from (or its NA)
  # print isolated functions in a nice sentence
  # add a new line for each function name for readability
  if(shiny::isTruthy(print_isolated_foo)) {
    v_isolated <- get_isolated_foo(df_edges = df_edges)
    if (length(v_isolated) > 0) {
      cat("The following functions are isolated (no parent or child): ",
          v_isolated,
          sep = "\n * ")
    }
  }

  # Plot the network of edges and nodes
  p <- plotNetwork(
    network_title = network_title,
    df_edges = df_edges,
    to_col = "to",
    from_col = "from",
    df_summary = df_summary,
    df_coverage = df_coverage,
    color_no_test = color_no_test,
    color_with_test = color_with_test,
    color_mod_coverage = color_mod_coverage,
    moderate_coverage_range = moderate_coverage_range,
    show_in_shiny = show_in_shiny,
    scale_node_size_by_degree = scale_node_size_by_degree
  )



  # whether to display in shiny or not
  if(!isTRUE(show_in_shiny)) {
    return(p)
  } else {
    run_shiny_app(network_object = p,
                  network_title = network_title,
                  project_path = project_path,
                  foo_path = foo_path)
  }

}


#' Get Isolated Functions
#'
#' @param df_edges A data.table with two columns ("from" and "to") representing the dependencies.
#'
#' @return A vector of isolated function names.
#'
#' @examples
#' \dontrun{
#' get_isolated_foo(df_edges = data.frame(from = c("a", "b", "c", "d"), to = c("b", "c", NA, NA)))
#' }
get_isolated_foo <- function(df_edges){
  # get all unique functions
  v_functions <- unique(c(df_edges$from, df_edges$to))
  # remove NAs
  v_functions <- v_functions[!is.na(v_functions)]
  # from edges
  has_parent <- df_edges$from[df_edges$from %in% df_edges$to]
  has_child  <- unique(df_edges$from[!is.na(df_edges$to)])
  # return functions with no parent or child
  v_isolated <- setdiff(v_functions, c(has_parent, has_child))
  # return this vector of isolated function names
  return(v_isolated)
}

#' Load Functions into an Environment
#'
#' @param path Path to the folder containing the R scripts.
#' @param env The environment into which the functions will be loaded.
#'
#' @returns None
#'
#' @noRd
load_functions_into_env <- function(path, env) {
  files <- find_files(path = path, file_regx = ".R")
  for (file in files) {
    source_funcs(file, env = env)
  }
}





#' Identify Dependencies
#'
#' Identify dependencies between functions.
#'
#' @param v_unique_foo Vector of unique function strings.
#' @param pkg_env The package environment where the functions are defined
#'   (e.g. global).
#'
#' @return A data.table with two columns ("from" and "to") representing the dependencies.
#'
#' @importFrom data.table rbindlist
#'
#' @export
identify_dependencies <- function(v_unique_foo, pkg_env = environment()) {
  lapply(
    X = v_unique_foo,
    FUN = .called_by,
    all_functions = v_unique_foo,
    pkg_env = pkg_env
  ) |>
    data.table::rbindlist(fill = TRUE) |>
    unique() |>
    as.data.frame()
}

#' Called By
#'
#' Identify functions called by a given function within a specified project folder
#'
#' @param fname The name of the target function.
#' @param all_functions A character vector of all function names in the project.
#' @param pkg_env The package environment where the functions are defined (e.g. global).
#'
#' @return A data.table with two columns ("from" and "to") representing the dependencies
#'         of the target function. Returns NA if no dependencies are found.
#'
#' @details
#' The function identifies functions called by the target function `fname` within the specified
#' package environment `pkg_env`. It searches for dependencies within the literal code of the
#' function body and returns a data.table with two columns ("from" and "to") representing the
#' dependencies. If no dependencies are found, it returns a data.table with "from" as the target
#' function and "to" as NA.
#'
#' Note: This function may potentially miss calls if they are in attributes of the closure. For example
#' when function is defined within another function, capturing the environment of the outer function.
#'
#' @examples
#' \dontrun{
#' # Identify functions called by a specific function
#' .called_by(
#' fname = "my_function",
#' all_functions = c("function1", "function2", "function3"),
#' pkg_env = environment())
#' }
#'
#'
.called_by <- function(fname,
                       all_functions,
                       pkg_env){

  assertthat::assert_that(
    is.environment(pkg_env)
    , is.character(all_functions)
    , assertthat::is.string(fname)
  )

  # Get only the body of the function
  # We will potentially miss calls if they are in attributes of the closure,
  # e.g., the way the decorators package implements decorators
  tryCatch(
    expr = {
      f <- body(get(fname, envir = pkg_env))
      # get the literal code of the function
      f_vec <- .parse_function(f)

      # Figure out which ones mix
      matches <- match(
        x = f_vec,
        table = all_functions,
        nomatch = 0
      )
      matches <- matches[matches > 0]

      if (length(matches) == 0){
        #return(invisible(NULL))
        return(
          data.table::data.table(
            from = fname,
            to = NA)
        )
      }

      # Convention: If A depends on B, then A is the SOURCE
      # and B is the TARGET so that it looks like A -> B
      # This is consistent with the UML dependency convention
      # fname calls <matches>. So fname depends on <matches>.
      # So fname is SOURCE and <matches> are TARGETs
      edgeDT <- data.frame(
        from = fname,
        to = unique(all_functions[matches])
      )

      return(edgeDT)

    },
    error = function(e) {
      return(
        data.frame(
          from = fname,
          to = NA)
      )
    }
  )

}

#' Parse Function
#'
#' This function parses an R expression, breaking it down into its components.
#'
#' @param x An R expression to be parsed.
#'
#' @return A character string or a list of parsed components, depending on the input expression.
#'
#' @details
#' If the input expression `x` is not an atomic value, symbol, or an environment pointer,
#' the function breaks it up into a list of components. It also handles expressions of the
#' form `foo$bar` by splitting them up, keeping only the relevant parts for parsing.
#'
#' If `x` is a list of expressions, the function recursively parses each expression until
#' they can no longer be listed, filtering out atomic values in the process.
#'
#' If `x` is not listable (e.g. a function), it is deparsed into a character string.
#'
#' @examples
#' \dontrun{
#' # Parse a simple expression
#' tmp <- dplyr::across
#' .parse_function(tmp)
#' }
.parse_function <- function (x) {
  # If expression x is not an atomic value or symbol (i.e., name of object) or
  # an environment pointer then we can break x up into list of components
  listable <- (!is.atomic(x) && !is.symbol(x) && !is.environment(x))

  if (!is.list(x) && listable) {
    x <- as.list(x)

    # Check for expression of the form foo$bar
    # We still want to split it up because foo might be a function
    # but we want to get rid of bar, because it's a symbol in foo's namespace
    # and not a symbol that could be reliably matched to the package namespace
    if (identical(x[[1]], quote(`$`))) {
      x <- x[1:2]
    }
  }



  if (listable){
    # Filter out atomic values because we don't care about them
    x <- Filter(f = Negate(is.atomic), x = x)

    # Parse each listed expression recursively until
    # they can't be listed anymore
    out <- unlist(lapply(x, .parse_function), use.names = FALSE)
  } else {

    # If not listable, deparse into a character string
    out <- paste(deparse(x), collapse = "\n")
  }
  return(out)
}

#' Plot Network
#'
#' Visualize a network plot using the visNetwork package.
#'
#' @param df_edges A data frame containing columns "from" and "to" representing the edges of the network.
#' @param from_col Name of the column in df_edges representing the source nodes.
#' @param to_col Name of the column in df_edges representing the target nodes.
#' @param df_summary A summary dataframe containing the information about each function.
#' @param df_coverage a summary dataframe with function names and test coverages
#' @param color_no_test named vector with hexcodes for background, border and highlight
#' @param color_with_test named vector with hexcodes for background, border and highlight
#' @param color_mod_coverage named vector with hexcodes for background, border and highlight where coverage moderate
#' @param moderate_coverage_range vector of two values giving range defined as moderate coverage.
#' @param show_in_shiny logical scalar indicating whether to prepare/deploy the
#' network using a built in shiny app. Default is `FALSE`.
#' @param network_title title of the network plot.
#' @param scale_node_size_by_degree Scale the node size by the degree centrality of the node.
#'
#' @return A visNetwork object representing the network plot.
#'
#' @examples
#' \dontrun{
#' # Plot a network from a data frame of edges
#' plotNetwork(df_edges)
#' }
#'
#' @export
#' @importFrom visNetwork visNetwork visEdges visOptions
#' @importFrom dplyr rename
#' @importFrom htmltools a
#' @importFrom igraph graph_from_data_frame degree V
plotNetwork <- function(df_edges,
                        from_col = "from",
                        to_col = "to",
                        df_summary,
                        df_coverage,
                        color_no_test = c("background" = "#fad1d0", "border" = "#9c0000", "highlight" = "#9c0000"),
                        color_with_test = c("background" = "#e6ffe6", "border" = "#65a765", "highlight" = "#65a765"),
                        color_mod_coverage = c("background" = "#FFD580", "border" = "#E49B0F", "highlight" = "#E49B0F"),
                        moderate_coverage_range = c(0.2, 0.8),
                        show_in_shiny = FALSE,
                        network_title = NULL,
                        scale_node_size_by_degree = FALSE) {
  # Check input validity
  assertthat::assert_that(is.data.frame(df_edges),
                          from_col %in% colnames(df_edges),
                          to_col %in% colnames(df_edges))

  # Extract unique nodes from the dataframe
  df_nodes <- processNodes(df_edges = df_edges,
                           from_col = from_col,
                           to_col = to_col)

  df_node_info <- df_summary[, c("foo_string", "foo_location", "test_location")]

  # add in foo locations and test location to function strings in the edge dataframe
  df_node_info <-
    df_node_info[!duplicated(df_node_info[ , "foo_string"]), ] |>
    merge(y = df_nodes,
          by.x = "foo_string",
          by.y =  "id",
          all.y = T)

  # add in coverage
  foo_string_rename <- c("id" = "foo_string")
  if (!is.null(df_coverage)) {
    df_node_info <- df_node_info |>
      merge(y = df_coverage,
            by = "foo_string",
            all.x = T) |>
      dplyr::rename(dplyr::all_of(foo_string_rename))
  } else{
    df_node_info <- df_node_info |>
      dplyr::mutate(coverage = NA) |>
      dplyr::rename(dplyr::all_of(foo_string_rename))
  }

  # create the html for the toggle...
  foo_paths <- df_node_info$foo_location
  test_paths <- df_node_info$test_location

  df_nodes$title <- NULL
  if(isTRUE(show_in_shiny)) {
    for (index in 1:length(foo_paths)) {
      df_nodes$title[index] <- paste0(
        "<b>Foo Name</b>: ", df_node_info$label[index], " ",
        "<i class='fa-solid fa-robot' ",
        "title='Request AI summary' ",
        "aria-hidden='true' style='cursor:pointer; color: #337ab7;' id='",
        paste0("aiAssist_", df_node_info$label[index]),
        "' onclick=\"aiAssist('", df_node_info$label[index],
        "');\"></i>",
        "<br><b>Foo Location</b>: ", foo_paths[index],
        " ",
        "<i class='fa-solid fa-eye' ",
        "title='Open function definition in browser' ",
        "aria-hidden='true' style='cursor:pointer; color: #337ab7;' id='",
        paste0("openFileInShiny_", df_node_info$label[index]),
        "' onclick=\"openInShiny('", foo_paths[index],
        "');\"></i>",
        " ",
        "<i class='fa-solid fa-up-right-from-square' ",
        "title='Open function definition in RStudio' ",
        "aria-hidden='true' style='cursor:pointer; color: #75AADB;' id='",
        paste0("openFileInRStudio_", df_node_info$label[index]),
        "' onclick=\"openInRStudio('",  foo_paths[index],
        "');\"></i>",
        "<br><b>Test location</b>:",
        # skip "Test location" if coverage is 0%, cleaned_test_path == "".
        ifelse(
          test = test_paths[index] == "",
          yes = " ",
          no = paste0(
            " ",
            test_paths[index],
            " ",
            "<i class='fa-solid fa-eye' ",
            "title='Open function test(s) in browser' ",
            "aria-hidden='true' style='cursor:pointer; color: #337ab7;' id='",
            paste0("openFileInShiny_", df_node_info$label[index], "_test"),
            "' onclick=\"openInShiny('",
             test_paths[index],
            "');\"></i>",
            " ",
            "<i class='fa-solid fa-up-right-from-square' ",
            "title='Open function test(s) in RStudio' ",
            "aria-hidden='true' style='cursor:pointer; color: #75AADB;' id='",
            paste0("openFileInRStudio_", df_node_info$label[index], "_test"),
            "' onclick=\"openInRStudio('",
            test_paths[index],
            "');\"></i>"
          )
        ),
        "<br><b>Coverage</b>: ",
        paste0(df_node_info$coverage[index] * 100, "%")
      )
    }
  } else {
    df_nodes$title <- paste0(
      "<b>Foo Name</b>: ",
      df_node_info$label,
      "<br><b>Foo Location</b>: ",
      df_node_info$foo_location,
      "<br><b>Test location</b>: ",
      # skip "Test location" if coverage is 0%, cleaned_test_path == "".
      ifelse(
        test = test_paths == "",
        yes = NULL,
        no = df_node_info$test_location
        ),
      "<br><b>Coverage</b>: ",
      paste0(df_node_info$coverage * 100, "%")
    )
  }

  # define the colors based upon tests
  df_nodes$color.background <- ifelse(
    test = is.na(df_node_info$test_location),
    yes = color_no_test["background"],
    no  = color_with_test["background"]
  )

  df_nodes$color.border <- ifelse(
    test = is.na(df_node_info$test_location),
    yes = color_no_test["border"],
    no  = color_with_test["border"]
  )

  df_nodes$color.highlight <- ifelse(
    test = is.na(df_node_info$test_location),
    yes = color_no_test["highlight"],
    no  = color_with_test["highlight"]
  )

  if (shiny::isTruthy(scale_node_size_by_degree)) {

    # don't let igraph warn about replacing missing values
    df_edges <- replace(df_edges, is.na(df_edges), "NA")

    # network
    graph_of_edges <- igraph::graph_from_data_frame(d = df_edges,
                                                    directed = TRUE)
    degree_centrality <-
      igraph::degree(graph_of_edges, mode = "out")  # In-degree centrality for directed graphs
    # remove NA
    degree_centrality <-
      degree_centrality[names(degree_centrality) != "NA"]
    # Scale Degree centrality values
    scaled_centrality <- degree_centrality / max(degree_centrality)

    # Add scaled Degree centrality values to the dataframe
    df_nodes$value <-
      scaled_centrality[match(df_nodes$id, igraph::V(graph_of_edges)$name)]
  }

  # if code coverage is not all nulls
  if(any(!is.na(df_node_info$coverage))){

    df_nodes$color.background <- ifelse(
      test = between(x = df_node_info$coverage,
                     left = moderate_coverage_range[1],
                     right = moderate_coverage_range[2]),
      yes = color_mod_coverage["background"],
      no  = df_nodes$color.background
    )


    df_nodes$color.border <- ifelse(
      test = between(x = df_node_info$coverage,
                     left = moderate_coverage_range[1],
                     right = moderate_coverage_range[2]),
      yes = color_mod_coverage["border"],
      no  = df_nodes$color.border
    )

    df_nodes$color.highlight <- ifelse(
      test = between(x = df_node_info$coverage,
                     left = moderate_coverage_range[1],
                     right = moderate_coverage_range[2]),
      yes = color_mod_coverage["highlight"],
      no  = df_nodes$color.highlight
    )

  }

  # Create the network plot
  g <- if(isTRUE(show_in_shiny)){
    visNetwork::visNetwork(
      nodes = df_nodes,
      edges = df_edges,
      width = "1000px",
      height = "600px"
    )
  } else {
    visNetwork::visNetwork(
      nodes = df_nodes,
      edges = df_edges,
      main = network_title,
      submain = list(
        text = paste0(
          'Functions without a test are <a style="color:#9c0000;">red</a> and ',
          'those with a test are <a style="color:#65a765;">green</a>. Hover ',
          'over nodes for more information.'
        ),
        style = "font-family:Calibri; font-size:15px; text-align:center;"
      ),
      footer = paste0(
        '<a href="https://github.com/dark-peak-analytics/assertHE/"',
        '>Created with assertHE</a>'),
      width = "1000px",
      height = "600px"
    )
  }
  g <- g |>
    visNetwork::visEdges(arrows = 'from') |>
    visNetwork::visOptions(
      manipulation = TRUE,
      highlightNearest = list(
        enabled = TRUE,
        degree = nrow(df_nodes),
        algorithm = "hierarchical"
      ),
      collapse = list(enabled = TRUE),
      height = "600px",
      width = "1000px",
      nodesIdSelection = TRUE
    )

  return(g)
}

#' Process Nodes
#'
#' Process unique nodes from a dataframe of edges.
#'
#' @param df_edges A data frame containing columns "from" and "to" representing the edges of the network.
#' @param from_col Name of the column in df_edges representing the source nodes.
#' @param to_col Name of the column in df_edges representing the target nodes.
#'
#' @return A data frame of unique nodes with labels.
#' @importFrom stats na.omit
#' @importFrom dplyr mutate
processNodes <- function(df_edges,
                         from_col = "from",
                         to_col = "to") {

  assertthat::assert_that(msg = "Number of rows of df_edges in 'processNodes' does not exceed 1",
                          nrow(df_edges) > 1)

  df_nodes <- data.frame(
    id = df_edges[, c(from_col, to_col)] |>
      unlist(use.names = FALSE) |>
      unique() |>
      stats::na.omit()
  ) |>
    dplyr::mutate(label = id)

  return(df_nodes)
}

#' Run a Shiny app to host a network visualization
#'
#' @param uiFunction Function defining shiny user-interface
#' @param serverFunction Function defining shiny server logic
#' @param network_object visNetwork object to be displayed in the shiny app
#' @param network_title Title to be displayed in hte app above the title
#' @param project_path Path to the project directory
#' @param foo_path Path to the function folder
#'
#' @return A shiny app
#'
#' @importFrom rstudioapi navigateToFile
#'
#' @examples
#' \dontrun{
#' network_object <- visualise_project(
#'     project_path = "tests/testthat/example_project",
#'     foo_path = "R",
#'     test_path = "tests/testthat",
#'     run_coverage = TRUE
#'  )
#'
#'  run_shiny_app(network_object = network_object)
#' }
run_shiny_app <- function(
    uiFunction = app_ui,
    serverFunction = app_server,
    network_object,
    network_title = "Function Network",
    project_path,
    foo_path) {

  shiny::shinyApp(
    ui = uiFunction(network_title),
    server = serverFunction(network_object = network_object,
                            project_path = project_path,
                            foo_path = foo_path)
  )
}

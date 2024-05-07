#' Visualize Project
#'
#' Visualize the dependencies between functions in a project using a network plot.
#'
#' @inheritParams plotNetwork
#' @param project_path Path to the project folder.
#' @param foo_path Path to the folder containing foo functions.
#' @param test_path Path to the folder containing test functions.
#' @param run_coverage Boolean determining whether to run coverage assessment
#' @param print_isolated_foo Print the isolated functions to the console.
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
                              run_coverage = F,
                              color_no_test = c("background" = "#fad1d0", "border" = "#9c0000", "highlight" = "#9c0000"),
                              color_with_test = c("background" = "#e6ffe6", "border" = "#65a765", "highlight" = "#65a765"),
                              color_mod_coverage = c("background" = "#FFD580", "border" = "#E49B0F", "highlight" = "#E49B0F"),
                              moderate_coverage_range = c(0.2, 0.8),
                              print_isolated_foo = TRUE,
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

  # the scripts must be loaded in the namespace...
  # so we have to source them all before we can run the code.
  # ideally we would not need to do this, although its hardly the end of the world
  source_files(path = foo_path,
               verbose = F,
               keep_source = FALSE)

  # Identify function dependencies and create a network plot
  df_edges <- identify_dependencies(v_unique_foo = df_summary$foo_string)

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
                  project_path = project_path)
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



#' Identify Dependencies
#'
#' Identify dependencies between functions.
#'
#' @param v_unique_foo Vector of unique function strings.
#'
#' @return A data.table with two columns ("from" and "to") representing the dependencies.
#'
#' @importFrom data.table rbindlist
#'
#' @export
identify_dependencies <- function(v_unique_foo) {
  lapply(
    X = v_unique_foo,
    FUN = .called_by,
    all_functions = v_unique_foo,
    pkg_env = environment()
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
    print("scale by degree")
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

#' Remove artefacts from file path
#'
#' @param file_location Character scalar specifying the path of a file.
#' @param project_path Character scalar specifying the path of the project.
#'
#' @return A character scalar
#'
#' @importFrom here here
#'
#' @examples
#' \dontrun{
#' cleaned_file_path <- get_function_path(
#'  file_location = "tests/testthat/example_project/R/calculate_QALYs.R#L41"
#' )
#' cleaned_file_path <- get_function_path(
#'  file_location = c(
#'    "tests/testthat/example_project/R/calculate_QALYs.R#L41",
#'    "tests/testthat/example_project/R/calculate_QALYs.R#L49"
#'  )
#' )
#' }
get_function_path <- function(file_location, project_path) {

  get_function_path <- gsub("#.*", "", file_location)

  full_file_path <- ifelse(
    test = is.na(get_function_path),
    yes =  "",
    no = paste0(project_path, "/", get_function_path)
  )

  return(full_file_path)
}

#' Extract function line in file path
#'
#' @param file_location Character scalar specifying the path of a file.
#'
#' @return A numeric scalar
#'
#' @examples
#' \dontrun{
#' cleaned_function_line <- get_function_line(
#'  file_location = "tests/testthat/example_project/R/calculate_QALYs.R:L41"
#' )
#' cleaned_function_line <- get_function_line(
#'  file_location = c(
#'    "tests/testthat/example_project/R/calculate_QALYs.R#L41",
#'    "tests/testthat/example_project/R/calculate_QALYs.R#L49"
#'  )
#' )
#' }
get_function_line <- function(file_location) {

  function_line <- gsub(".*#L", "", file_location)

  function_line <- ifelse(
    test = is.na(function_line),
    yes =  -1L,
    no = as.numeric(function_line)
  )

  return(function_line)
}


#' Create closable shiny tab
#'
#' @param tab_name Character scalar representing the name or title of the shiny
#' tab.
#' @param content_output_Id Character scalar representing the id of the shiny
#' tab.
#' @param output_type Character scalar specifying the type of rendered output.
#' Default is `"text"` and can also accept `"HTML"`.
#'
#' @return A tab that can be passed to `shiny::tabsetPanel()`
make_closable_tab <- function(
    tab_name,
    content_output_Id,
    output_type = "text") {
  shiny::tabPanel(
    title = shiny::div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      shiny::span(
        shiny::HTML(
          paste0("<b>", tab_name, "</b>")
        ),
        style = "flex-grow: 1;"
      ),
      shiny::actionButton(
        inputId = "close",
        label = shiny::HTML(
          '<i class="fa fa-window-close" aria-hidden="true"></i>'
        ),
        class = "close-tab",
        onclick = sprintf(
          "Shiny.setInputValue('close_tab', '%s');",
          content_output_Id
        )
      )
    ),
    shiny::div(
      class = "custom-tab-content",
      if(output_type != "HTML") {
        shiny::verbatimTextOutput(outputId = content_output_Id)
      } else {
        shiny::htmlOutput(outputId = content_output_Id)
      }
    ),
    value = content_output_Id
  )
}

#' Create Shiny app UI
#'
#' @param network_title Character string representing the title of the network to be displayed above the network.
#'
#' @return Shiny app user interface
#'
#' @importFrom shinyWidgets pickerInput
define_app_ui <- function(network_title) {

  languages <- c("English", "Spanish", "Mandarin", "Hindi", "Arabic",
                 "Bengali", "Portuguese", "Russian", "Japanese", "German",
                 "Javanese", "Punjabi", "Wu", "Telugu", "Marathi",
                 "Vietnamese", "Korean", "French", "Turkish", "Tamil",
                 "Bulgarian", "Croatian", "Czech", "Danish", "Dutch",
                 "Estonian", "Finnish", "Greek", "Hungarian", "Irish",
                 "Italian", "Latvian", "Lithuanian", "Maltese", "Polish",
                 "Romanian", "Slovak", "Slovenian", "Swedish")

  flags <- c("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/gb.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/es.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/cn.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/in.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/sa.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/bd.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/pt.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/ru.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/jp.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/de.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/id.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/in.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/us.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/in.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/in.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/vn.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/kr.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/fr.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/tr.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/in.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/bg.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/hr.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/cz.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/dk.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/nl.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/ee.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/fi.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/gr.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/hu.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/ie.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/it.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/lv.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/lt.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/mt.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/pl.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/ro.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/sk.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/si.svg",
             "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/se.svg")


  shiny::fluidPage(
    # Initialize shinyjs and waiter
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    # Define javaScript functions
    shiny::tags$head(
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"
      ),
      shiny::tags$script("
      // Initialize a variable to mirror the values of 'aiAssist',
      // 'openInShiny' and 'openInRStudio'
      var currentAiAssistValue = null;
      var currentOpenInShinyValue = null;
      var currentOpenInRstudioValue = null;

      function aiAssist(function_name) {
        console.log('Executing JavaScript function aiAssist');
        console.log('Function name: ' + function_name);

        // Check if the current value is the same as 'function_name'
        if (currentAiAssistValue === function_name) {

          // Temporarily set 'aiAssist' and 'close_tab' to null
          // to ensure the change is detected
          Shiny.setInputValue('aiAssist', null, {priority: 'event'});
          Shiny.setInputValue('close_tab', null, {priority: 'event'});

          // Use setTimeout to ensure the null value is processed before setting
          // the new value
          setTimeout(function() {
            Shiny.setInputValue('aiAssist', function_name, {priority: 'event'});
          }, 10);

        } else {

          // Directly set 'aiAssist' to 'function_name'
          Shiny.setInputValue('aiAssist', function_name);
        }

       // Update the mirror variable to reflect the new value
        currentAiAssistValue = function_name;
      }

      function openInShiny(file_location) {
        console.log('Executing JavaScript function openInShiny');
        console.log('File location: ' + file_location);

        // Check if the current value is the same as 'file_location'
        if (currentOpenInShinyValue === file_location) {

          // Temporarily set 'openInShiny' and 'close_tab' to null
          // to ensure the change is detected
          Shiny.setInputValue('openInShiny', null, {priority: 'event'});
          Shiny.setInputValue('close_tab', null, {priority: 'event'});

          // Use setTimeout to ensure the null value is processed before setting
          // the new value
          setTimeout(function() {
            Shiny.setInputValue('openInShiny', file_location, {priority: 'event'});
          }, 10);
        } else {

          // Directly set 'openInShiny' to 'file_location' if the values are
          // different
          Shiny.setInputValue('openInShiny', file_location, {priority: 'event'});
        }

        // Update the mirror variable to reflect the new value
        currentOpenInShinyValue = file_location;
      }

      function openInRStudio(file_location) {
        console.log('Executing JavaScript function openInRStudio');
        console.log('File location: ' + file_location);

        // Check if the current value is the same as 'file_location'
        if (currentOpenInRstudioValue === file_location) {

          // Temporarily set 'openInRStudio' to null to ensure the change is
          // detected
          Shiny.setInputValue('openInRStudio', null, {priority: 'event'});

          // Use setTimeout to ensure the null value is processed before setting
          // the new value
          setTimeout(function() {
            Shiny.setInputValue('openInRStudio', file_location, {priority: 'event'});
          }, 10);

        } else {

          // Directly set 'openInRStudio' to 'file_location'
          Shiny.setInputValue('openInRStudio', file_location);
        }

       // Update the mirror variable to reflect the new value
        currentOpenInRstudioValue = file_location;
      }

      $(document).on('shiny:connected', function(event) {
        function adjustTabHeight() {
          var windowHeight = $(window).height();
          // Distance from the top of the viewport
          var offsetTop = $('#fileTabs').offset().top;
          // Subtract any additional margin or padding
          var tabHeight = windowHeight - offsetTop - 20;
          $('.tab-content').css('height', tabHeight + 'px');
        }

        // Adjust the height on window resize
        $(window).resize(adjustTabHeight);

        // Initial adjustment
        adjustTabHeight();
      });
    "),
      # Define CSS
      shiny::tags$style("
      /* Make tab content scrollable */
      .tab-content {
        /* Enable vertical scrolling */
        overflow-y: auto;
      }

      /* Style for custom tab content background */
      .custom-tab-content {
        background-color: #f5f5f5;
        padding-bottom: 5px;
        border-radius: 4px;
        margin-bottom: 20px;
      }

      /* Reduce padding around close actionButton */
      .close-tab {
        border: none;
        padding: 5px;
        margin: 5px;
        color: red;
      }

      /* Waiter */
      .waiter-overlay  {
        position: fixed;
        height: 919px;
        width: 1375px;
        top: 0px;
        left: 0px;
        background-color: rgba(51, 62, 72, 0.5) !important;
      }
    ")
    ),
    # Define network plot title/subtitle divs
    shiny::fluidRow(align = 'center',
                    shinyWidgets::pickerInput(inputId = "language",
                                label = "Language:",
                                choices = languages,
                                selected = "English",
                                width = "fit",
                                options = list(
                                  `live-search` = TRUE),
                                choicesOpt = list(content =
                                       mapply(languages, flags,
                                              FUN = function(language, flagUrl) {
                                                shiny::HTML(paste(
                                                  shiny::tags$img(src=flagUrl,
                                                           width=20,
                                                           height=15),
                                                  language
                                                ))
                                              },
                                              SIMPLIFY = FALSE,
                                              USE.NAMES = FALSE)))
    ),
    shiny::HTML(
      paste0(
        '<div id="titlehtmlwidget-b9361e0bc6cd12c5d6d9" style="font-family: ',
        'Georgia, &quot;Times New Roman&quot;, Times, serif; font-weight: ',
        'bold; font-size: 20px; text-align: center; background-color: ',
        'inherit; display: block;">',
        network_title,
        '</div>'
      )
    ),
    shiny::HTML(
      paste0(
        '<div id="subtitlenetworkPlot" ',
        'style="font-family:Calibri; font-size:15px; text-align:center; ',
        'background-color: inherit;">',
        'Functions without a test are <a style="color:#9c0000;">red</a> ',
        'and those with a test are <a style="color:#65a765;">green</a>. ',
        'Hover over nodes for more information. Click on ',
        "<i class='fa-solid fa-robot' aria-hidden='true' ",
        "style='color: #337ab7;'></i>",' to request an AI generated summary ',
        "of the corresponding function, ",
        "<i class='fa-solid fa-up-right-from-square' aria-hidden='true' ",
        "style='color: #75AADB;'></i>",' to open the file in RStudio, or on ',
        "<i class='fa-solid fa-eye' aria-hidden='true' style='color: #337ab7;'></i>",
        ' to load its contents into a new browser tab.</div>'
      )
    ),
    shiny::br(),
    # Define main panel
    shiny::fluidRow(
      shiny::column(
        # Prevent another tabs from covering network tooltip/popup
        style = "z-index: 10000;",
        width = 11,
        visNetwork::visNetworkOutput(
          outputId = "networkPlot"
        ),
        id = "mainColumn"
      ),
      shiny::column(
        width = 1,
        # Define a tabsetPanel to contain the dynamic tabs showing user code
        shiny::tabsetPanel(
          id = "fileTabs"
        ),
        id = "tabColumn"
      )
    ),
    shiny::HTML(
      paste0(
        '<div id="footernetworkPlot" style="font-family: Georgia, &quot;Times ',
        'New Roman&quot;, Times, serif; font-size: 12px; text-align: center; ',
        'background-color: inherit; display: block;">',
        '<a href="https://github.com/dark-peak-analytics/assertHE/">',
        'Created with assertHE</a></div>'
      )
    )
  )
}

#' Create Shiny app server logic
#'
#' @inheritParams run_shiny_app
#'
#' @return Shiny app server logic
define_app_server <- function(network_object, project_path) {

  function(input, output, session) {

    # Keep track of the current tab's output ID
    currentTabId <- shiny::reactiveVal(NULL)

    # Keep track of the number of AI-generated summary requests
    aiAssit_calls <- shiny::reactiveVal(0)

    # Render the network visual
    output$networkPlot <- visNetwork::renderVisNetwork(network_object)

    # Observer to handle AI response in a new tab within the shiny app
    shiny::observeEvent(
      ignoreNULL = TRUE,
      ignoreInit = TRUE,
      eventExpr = input$aiAssist,
      handlerExpr = {
        # If there's a current tab open, remove it
        if (!is.null(currentTabId())) {
          shiny::removeTab(
            inputId = "fileTabs",
            target = currentTabId()
          )
        }

        if(input$aiAssist != "") {
          function_name <- input$aiAssist
          tab_name <- paste(function_name)

          # Check if the function name refers to an existing function
          if (is.function(get(x = function_name))) {
            # Check if the number of calls did not exceed a maximum:
            if(aiAssit_calls() < 5) {
              # Waiter
              waiter <- waiter::Waiter$new(
                html = shiny::div(
                  style = "display: flex;
                          flex-direction: column;
                          align-items: center;
                          justify-content:center;
                          color: white;
                          opacity: 1 !important;",
                  shiny::h4("Please wait ..."),
                  shiny::h6(paste(
                    "generating", function_name, "function summary ..."
                  )),
                  shiny::br(),
                  shiny::br(),
                  waiter::spin_wandering_cubes()
                ),
                hide_on_render  = FALSE
              )
              waiter$show()
              on.exit(waiter$hide())

              # Set the ID of the new tab to be the current one
              currentTabId(tab_name)

              # Query AI:
              print(input$language)
              ai_response <- summarise_function_with_LLM(
                text_language = input$language,
                foo_name = function_name,
                llm_api_url = Sys.getenv("LLM_API_URL"),
                llm_api_key = Sys.getenv("LLM_API_KEY")
              )

              # Create the new tab and output its content
              output[[tab_name]] <- shiny::renderUI({
                shiny::HTML(ai_response)
              })

              # Update the number of calls
              aiAssit_calls(aiAssit_calls() + 1)

              # Dynamically adjust column widths
              shinyjs::runjs(
                '$("#mainColumn").removeClass("col-sm-11").addClass("col-sm-6");'
              )
              shinyjs::runjs(
                '$("#tabColumn").removeClass("col-sm-1").addClass("col-sm-6");'
              )

              # Insert the new tab and set it to the current
              shiny::insertTab(
                inputId = "fileTabs",
                make_closable_tab(
                  tab_name = paste("AI summary -", tab_name),
                  content_output_Id = tab_name,
                  output_type = "HTML"
                ),
                select = TRUE
              )

            } else {
              # Reset the current tab ID
              currentTabId(NULL)
              # Reset columns width
              shinyjs::runjs(
                '$("#mainColumn").removeClass("col-sm-6").addClass("col-sm-11");'
              )
              shinyjs::runjs(
                '$("#tabColumn").removeClass("col-sm-6").addClass("col-sm-1");'
              )
              # Notify the user if the file is not found
              shiny::showNotification(
                ui = "You have exceeded the allotted AI queries.",
                type = "warning"
              )
            }
          } else {
            # Notify the user if the file is not found
            shiny::showNotification(
              ui = paste("Function not found:", function_name),
              type = "error"
            )
          }
        }
      }
    )

    # Observer to handle opening files in RStudio
    shiny::observeEvent(
      ignoreNULL = TRUE,
      ignoreInit = TRUE,
      eventExpr = input$openInRStudio,
      handlerExpr = {
        # Open the file in RStudio
        file_location <- input$openInRStudio
        file_path <- get_function_path(
          file_location = file_location,
          project_path =  project_path
        )
        function_line <- get_function_line(
          file_location = file_location
        )

        if (file.exists(file_path)) {
          rstudioapi::navigateToFile(
            file = file_path,
            line = function_line
          )
        } else {
          shiny::showNotification(
            paste("File not found:", file_path),
            type = "error"
          )
        }
      }
    )

    # Observer to handle opening files in a new tab within the shiny app
    shiny::observeEvent(
      ignoreNULL = TRUE,
      ignoreInit = TRUE,
      eventExpr = input$openInShiny,
      handlerExpr = {
        if(input$openInShiny != "") {
          file_location <- input$openInShiny
          file_location <- get_function_path(
            file_location = file_location,
            project_path =  project_path
          )
          tab_name <- basename(file_location)

          # If there's a current tab open, remove it
          if (!is.null(currentTabId())) {
            shiny::removeTab(
              inputId = "fileTabs",
              target = currentTabId()
            )
          }

          # Set the ID of the new tab to be the current one
          currentTabId(tab_name)

          # Check if the file path is valid
          if (file.exists(file_location)) {
            # Read the file content
            file_content <- readLines(file_location)

            # Create the new tab and output its content
            output[[tab_name]] <- shiny::renderPrint({
              cat(paste(file_content, collapse = "\n"))
            })

            # Dynamically adjust column widths
            shinyjs::runjs(
              '$("#mainColumn").removeClass("col-sm-11").addClass("col-sm-6");'
            )
            shinyjs::runjs(
              '$("#tabColumn").removeClass("col-sm-1").addClass("col-sm-6");'
            )

            # Insert the new tab and set it to the current
            shiny::insertTab(
              inputId = "fileTabs",
              make_closable_tab(
                tab_name = tab_name,
                content_output_Id = tab_name
              ),
              select = TRUE
            )
          } else {
            # Notify the user if the file is not found
            shiny::showNotification(
              ui = paste("File not found:", file_location),
              type = "error"
            )
          }
        }
      }
    )

    # Observer to remove the tab opened in the shiny app
    shiny::observeEvent(
      ignoreNULL = TRUE,
      ignoreInit = TRUE,
      eventExpr = input$close_tab,
      handlerExpr = {
        if (!is.null(currentTabId())) {
          shiny::removeTab(
            inputId = "fileTabs",
            target = currentTabId()
          )
          # Reset the current tab ID
          currentTabId(NULL)
          # Reset columns width
          shinyjs::runjs(
            '$("#mainColumn").removeClass("col-sm-6").addClass("col-sm-11");'
          )
          shinyjs::runjs(
            '$("#tabColumn").removeClass("col-sm-6").addClass("col-sm-1");'
          )
        }
      }
    )
  }
}

#' Run a Shiny app to host a network visualization
#'
#' @param uiFunction Function defining shiny user-interface
#' @param serverFunction Function defining shiny server logic
#' @param network_object visNetwork object to be displayed in the shiny app
#' @param network_title Title to be displayed in hte app above the title
#' @param project_path Path to the project directory
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
    uiFunction = define_app_ui,
    serverFunction = define_app_server,
    network_object,
    network_title = "Function Network",
    project_path) {

  shiny::shinyApp(
    ui = uiFunction(network_title),
    server = serverFunction(network_object = network_object,
                            project_path = project_path)
  )
}

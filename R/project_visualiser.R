#' Visualize Project
#'
#' Visualize the dependencies between functions in a project using a network plot.
#'
#' @inheritParams plotNetwork
#' @param project_path Path to the project folder.
#' @param foo_path Path to the folder containing foo functions.
#' @param test_path Path to the folder containing test functions.
#' @param run_coverage Boolean determining whether to run coverage assessment
#'
#' @return A visNetwork object representing the network plot of function dependencies.
#'
#' @importFrom miceadds source.all
#'
#' @examples
#' \dontrun{
#' # Visualize project dependencies
#' visualise_project(
#' project_path = "tests/testthat/example_project",
#' foo_path = "R",
#' test_path = "tests/testthat",
#' run_coverage = T)
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
                              show_in_shiny = FALSE) {

  # Check folder existence
  stopifnot(dir.exists(project_path),
            dir.exists(paste0(project_path,"/", foo_path)),
            dir.exists(paste0(project_path,"/", test_path)))

  # if test path is null then don't include them in summary...
  test_path <- if (is.null(test_path)) {
    NULL
  } else{
    paste0(project_path, "/", test_path)
  }

  # Load and summarize the model
  df_summary <- summarise_model(foo_folder = paste0(project_path, "/", foo_path),
                                test_folder = test_path)

  df_coverage <- NULL
  # get test coverage
  if (run_coverage) {
    df_coverage <-
      get_foo_coverage(foo_folder  = paste0(project_path, "/", foo_path),
                       test_folder = test_path)
  }

  # the scripts must be loaded in the namespace...
  # so we have to source them all before we can run the code.
  # ideally we would not need to do this, although its hardly the end of the world
  miceadds::source.all(path = paste0(project_path,"/", foo_path),
                       print.source = F)

  # Identify function dependencies and create a network plot
  df_edges <- identify_dependencies(v_unique_foo = df_summary$foo_string)

  # Plot the network of edges and nodes
  p <- plotNetwork(
    df_edges = df_edges,
    to_col = "to",
    from_col = "from",
    df_summary = df_summary,
    df_coverage = df_coverage,
    color_no_test = color_no_test,
    color_with_test = color_with_test,
    color_mod_coverage = color_mod_coverage,
    moderate_coverage_range = moderate_coverage_range,
    show_in_shiny = show_in_shiny
  )

  if(!isTRUE(show_in_shiny)) {

    return(p)
  } else {

    run_shiny_app(network_object = p)
  }
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
#'
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
#' netowrk using a built in shiny app. Default is `FALSE`.
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
plotNetwork <- function(df_edges,
                        from_col = "from",
                        to_col = "to",
                        df_summary,
                        df_coverage,
                        color_no_test = c("background" = "#fad1d0", "border" = "#9c0000", "highlight" = "#9c0000"),
                        color_with_test = c("background" = "#e6ffe6", "border" = "#65a765", "highlight" = "#65a765"),
                        color_mod_coverage = c("background" = "#FFD580", "border" = "#E49B0F", "highlight" = "#E49B0F"),
                        moderate_coverage_range = c(0.2, 0.8),
                        show_in_shiny = FALSE) {
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
        "<b>Foo Name</b>: ",
        df_node_info$label[index],
        "<br><b>Foo Location</b>: ", foo_paths[index], " ",
        "<i class='fa fa-eye' ",
        "title='Open function definition in browser' ",
        "aria-hidden='true' style='cursor:pointer; color: #337ab7;' id='",
        paste0("openFileInShiny_", df_node_info$label[index]),
        "' onclick=\"openInShiny('", foo_paths[index], "');\"></i>", " ",
        "<i class='fa fa-external-link' ",
        "title='Open function definition in RStudio' ",
        "aria-hidden='true' style='cursor:pointer; color: #75AADB;' id='",
        paste0("openFileInShiny_", df_node_info$label[index]),
        "' onclick=\"openInRStudio('", foo_paths[index], "');\"></i>",
        # skip "Test location" if coverage is 0%, cleaned_test_path == "".
        ifelse(
          test_paths[index] == "",
          "",
          paste0(
            "<br><b>Test location</b>: ", test_paths[index], " ",
            "<i class='fa fa-eye' ",
            "title='Open function test(s) in browser' ",
            "aria-hidden='true' style='cursor:pointer; color: #337ab7;' id='",
            paste0("openFileInShiny_", df_node_info$label[index], "_test"),
            "' onclick=\"openInShiny('", test_paths[index], "');\"></i>", " ",
            "<i class='fa fa-external-link' ",
            "title='Open function test(s) in RStudio' ",
            "aria-hidden='true' style='cursor:pointer; color: #75AADB;' id='",
            paste0("openFileInShiny_", df_node_info$label[index], "_test"),
            "' onclick=\"openInRStudio('", test_paths[index], "');\"></i>"
          )
        ),
        "<br><b>Coverage</b>: ",
        paste0(df_node_info$coverage[index] * 100, "%")
      )
    }
  } else {
    df_nodes$title <- paste0(
      "Foo Name: ",
      df_node_info$label,
      "<br>Foo Location: ", df_node_info$foo_location,
      # skip "Test location" if coverage is 0%, cleaned_test_path == "".
      ifelse(
        test = test_paths == "",
        yes = NULL,
        no = paste0(
          "<br>Test location: ",
          df_node_info$test_location
        )
      ),
      "<br>Coverage: ",
      paste0(df_node_info$coverage * 100, "%")
    )
  }

  # define the colors based upon tests
  df_nodes$color.background <- ifelse(test = is.na(df_node_info$test_location),
                                      yes = color_no_test["background"],
                                      no  = color_with_test["background"])

  df_nodes$color.border <- ifelse(test = is.na(df_node_info$test_location),
                                  yes = color_no_test["border"],
                                  no  = color_with_test["border"])

  df_nodes$color.highlight <- ifelse(test = is.na(df_node_info$test_location),
                                     yes = color_no_test["highlight"],
                                     no  = color_with_test["highlight"])


  # if code coverage is not all nulls
  if(any(!is.na(df_node_info$coverage))){

    df_nodes$color.background <- ifelse(test = between(x = df_node_info$coverage,
                                                       left = moderate_coverage_range[1],
                                                       right = moderate_coverage_range[2]),
                                        yes = color_mod_coverage["background"],
                                        no  = df_nodes$color.background)


    df_nodes$color.border <- ifelse(test = between(x = df_node_info$coverage,
                                                   left = moderate_coverage_range[1],
                                                   right = moderate_coverage_range[2]),
                                    yes = color_mod_coverage["border"],
                                    no  = df_nodes$color.border)

    df_nodes$color.highlight <- ifelse(test = between(x = df_node_info$coverage,
                                                      left = moderate_coverage_range[1],
                                                      right = moderate_coverage_range[2]),
                                       yes = color_mod_coverage["highlight"],
                                       no  = df_nodes$color.highlight)

  }


  # Create the network plot
  g <- visNetwork::visNetwork(
    nodes = df_nodes,
    edges = df_edges,
    main = "Function Network",
    submain = list(text = 'Functions without a test are <a style="color:#9c0000;">red</a> and those with a test are <a style="color:#65a765;">green</a>. Hover over nodes for more information.',
                   style = "font-family:Calibri; font-size:15px; text-align:center;"),
    footer = '<a href="https://github.com/dark-peak-analytics/assertHE/">Created with assertHE</a>',
    width = "100%"
  ) |>
    visNetwork::visEdges(arrows = 'from') |>
    visNetwork::visOptions(
      manipulation = TRUE,
      highlightNearest = list(
        enabled = TRUE,
        degree = nrow(df_nodes),
        algorithm = "hierarchical"
      ),
      collapse = list(enabled = TRUE),
      height = "500px",
      width = "100%",
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
#'
#' @return A character scalar
#' @export
#'
#' @examples
#' \dontrun{
#' cleaned_file_path <- get_function_path(
#'  file_location = "tests/testthat/example_project/R/calculate_QALYs.R:L41"
#' )
#' cleaned_file_path <- get_function_path(
#'  file_location = c(
#'    "tests/testthat/example_project/R/calculate_QALYs.R:L41",
#'    "tests/testthat/example_project/R/calculate_QALYs.R:L49"
#'  )
#' )
#' }
get_function_path <- function(file_location) {

  get_function_path <- gsub(":.*", "", file_location)

  full_file_path <- ifelse(
    test = is.na(get_function_path),
    yes =  "",
    no = paste0(here::here(), "/", get_function_path)
  )

  return(full_file_path)
}

#' Extract function line in file path
#'
#' @param file_location Character scalar specifying the path of a file.
#'
#' @return A numeric scalar
#' @export
#'
#' @examples
#' \dontrun{
#' cleaned_function_line <- get_function_line(
#'  file_location = "tests/testthat/example_project/R/calculate_QALYs.R:L41"
#' )
#' cleaned_function_line <- get_function_line(
#'  file_location = c(
#'    "tests/testthat/example_project/R/calculate_QALYs.R:L41",
#'    "tests/testthat/example_project/R/calculate_QALYs.R:L49"
#'  )
#' )
#' }
get_function_line <- function(file_location) {

  function_line <- gsub(".*:L", "", file_location)

  function_line <- ifelse(
    test = is.na(function_line),
    yes =  -1L,
    no = as.numeric(function_line)
  )

  return(function_line)
}

#' Create Shiny app UI
#'
#' @return Shiny app user interface
define_app_ui <- function() {

  shiny::fluidPage(
    # Initialize shinyjs
    shinyjs::useShinyjs(),
    # Define javaScript functions
    shiny::tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
      ),
      shiny::tags$script("
      // Initialize a variable to mirror the value of 'openInShiny'
      var currentOpenInShinyValue = null;
      var currentOpenInRstudioValue = null;

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
      /* CSS to make tab content scrollable */
      .tab-content {
        overflow-y: auto; /* Enable vertical scrolling */
      }

      /* Style for custom tab content background */
      .custom-tab-content {
        background-color: #f5f5f5;
        padding-bottom: 5px;
        border-radius: 4px;
        margin-bottom: 20px;
      }
    ")
    ),
    # Define main panel
    shiny::fluidRow(
      shiny::column(
        # Prevent another tabs from covering network tooltip
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
    )
  )
}

#' Create Shiny app server logic
#'
#' @inheritParams run_shiny_app
#'
#' @return Shiny app server logic
define_app_server <- function(network_object) {
  function(input, output, session) {
    # Function to create a closable tab
    makeClosableTab <- function(tabName, contentOutputId) {
      shiny::tabPanel(
        title = div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          shiny::span(tabName, style = "flex-grow: 1;"),
          shiny::actionButton(
            inputId = "close",
            label = HTML("&times;"),
            class = "close-tab",
            onclick = sprintf(
              "Shiny.setInputValue('close_tab', '%s');",
              contentOutputId
            )
          )
        ),
        shiny::div(
          class = "custom-tab-content",
          shiny::verbatimTextOutput(outputId = contentOutputId)
        ),
        value = contentOutputId  # Assign the contentOutputId as the tab's value for easy identification
      )
    }

    # Keep track of the current tab's output ID
    currentTabId <- shiny::reactiveVal(NULL)

    # Render the network visual
    output$networkPlot <- visNetwork::renderVisNetwork(network_object)

    # Observer to handle opening files in RStudio
    shiny::observeEvent(
      ignoreNULL = TRUE,
      ignoreInit = TRUE,
      eventExpr = input$openInRStudio,
      handlerExpr = {
        # Open the file in RStudio
        file_location <- input$openInRStudio
        file_path <- get_function_path(
          file_location = file_location
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
      })

    # Observer to handle opening files in a new tab within the shiny app
    shiny::observeEvent(
      ignoreNULL = TRUE,
      ignoreInit = TRUE,
      eventExpr = input$openInShiny,
      handlerExpr = {
        if(input$openInShiny != "") {
          file_location <- input$openInShiny
          file_location <- get_function_path(
            file_location = file_location
          )
          tab_name <- basename(file_location)

          # If there's a current tab open, remove it
          if (!is.null(currentTabId())) {
            print("I found a tab")
            print(currentTabId())
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
              makeClosableTab(
                tabName = tab_name,
                contentOutputId = tab_name
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
      })

    # Observer to remove the tab opened in the shiny app
    observeEvent(
      ignoreNULL = TRUE,
      ignoreInit = TRUE,
      eventExpr = input$close_tab,
      handlerExpr = {
        print("detecting closer")
        print(input$close_tab)
        print("compare")
        print(currentTabId())
        if (!is.null(currentTabId())) {
          print("Print closing!")
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
      })
  }
}

#' Run a Shiny app to host a network visualization
#'
#' @param uiFunction Function defining shiny user-interface
#' @param serverFunction Function defining shiny server logic
#' @param network_object visNetwork object to be displayed in the shiny app
#'
#' @return A shiny app
#' @export
#'
#' @examples
#' \dontrun {
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
    network_object) {

  shiny::shinyApp(
    ui = uiFunction(),
    server = serverFunction(network_object = network_object)
  )
}









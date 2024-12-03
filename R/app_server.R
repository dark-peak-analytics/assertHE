#' Help text
#' @importFrom shiny div icon span br
#' @return A shiny HTML element
help_text <- div(
  class = "subtitle-container",
  span(
    class = "subtitle-text",
    "Hover over nodes for more information.", br(),
    "Functions without a test are ", span(class="text-danger fw-bolder", "red"),
    "and those with a test are ", span(class="text-success fw-bolder", "green"), ". ",
    br(),
    "Click on", icon(
      name = "robot",
      style = "color: #337ab7; margin-right: 5px; margin-left: 5px;"
    ),
    " to request an AI generated summary of the corresponding function.", br(),
    icon(
      name = "up-right-from-square",
      style = "color: #75AADB; margin-right: 5px; margin-left: 5px;"
    ),
    " to open the file in RStudio, or", br(),
    icon(
      name = "eye",
      style = "color: #337ab7; margin-right: 5px; margin-left: 5px;"
    ),
    " to request a visualisation of the function's call graph."
  )
)

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

#' Create Shiny app server logic
#'
#' @inheritParams run_shiny_app
#' @param foo_path path to the function folder
#' @importFrom shinyWidgets show_toast
#' @importFrom shinyjs onclick
#'
#' @return Shiny app server logic
app_server <- function(network_object, project_path, foo_path) {
  function(input, output, session) {

    # Create a new environment to avoid sourcing scripts into the namespace
    pkg_env <- new.env(parent = baseenv())

    # Load all functions into this environment
    load_functions_into_env(path = foo_path, env =  pkg_env)

    # Keep track of the current tab's output ID
    currentTabId <- shiny::reactiveVal(NULL)

    # Keep track of the number of AI-generated summary requests
    aiAssit_calls <- shiny::reactiveVal(0)

    # Render the network visual
    output$networkPlot <- visNetwork::renderVisNetwork(network_object)

    onclick("question-icon-click", {
      show_toast(
        title = "Help",
        text = help_text,
        type = "info",
        timer = 0,
        width = "500px"
      )
    })

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
          if (is.function(get(x = function_name, envir = pkg_env))) {
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
              ai_response <- summarise_function_with_LLM(
                foo_name = function_name,
                llm_api_url = Sys.getenv("LLM_API_URL"),
                llm_api_key = Sys.getenv("LLM_API_KEY"),
                envir = pkg_env
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

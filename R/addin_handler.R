app_env <- new.env()

app_env$p_project_path = NULL
app_env$p_foo_path = NULL
app_env$p_test_path = NULL
app_env$p_exclude_files = NULL
app_env$p_exclude_dirs = NULL
app_env$p_run_coverage = NULL
app_env$p_color_no_test = NULL
app_env$p_color_with_test = NULL
app_env$p_color_mod_coverage = NULL
app_env$p_moderate_coverage_range = NULL
app_env$p_print_isolated_foo = NULL
app_env$p_show_in_shiny = NULL
app_env$p_network_title = NULL
app_env$p_scale_node_size_by_degree = NULL

# Environment to track app state

app_env$closed <- FALSE

# Addin handler
visualise_project_addin <- function() {

  essentials_panel <- shiny::tabPanel(
    "Essential",
    htmltools::br(),
    shiny::fluidRow(
      shiny::column(align="center",
                    width = 12,
                    shiny::textInput(
                      inputId = "network_title",
                      label = "Network Title",
                      value = "Function Network"
                    ),
                    shiny::textInput(
                      inputId = "project_path",
                      label = "Project Path",
                      value = "."
                    ),
                    shiny::textInput(
                      inputId = "foo_path",
                      label = "Function folder Path",
                      value = "R"
                    ),
                    shiny::textInput(
                      inputId = "test_path",
                      label = "Test folder Path",
                      value = NULL,
                      placeholder = "tests/testthat"
                    ),
                    shiny::textInput(
                      inputId = "exclude_files",
                      label = "Exclude Files (comma-separated)",
                      value = NULL
                    ),
                    shiny::textInput(
                      inputId = "exclude_dirs",
                      label = "Exclude Directories (comma-separated)",
                      value = NULL
                    ),
                    shiny::checkboxInput(
                      inputId = "run_coverage",
                      label = "Run Coverage",
                      value = FALSE
                    ),
                    shiny::checkboxInput("show_in_shiny", "Show in Shiny", value = FALSE)
      )
    )
  )

  color_panel <- shiny::tabPanel(
    "Node colours",
    htmltools::br(),
    shiny::fluidRow(
      shiny::column(
        align = "center",
        width = 4,
        offset = 0,
        colourpicker::colourInput(width = "80%",
                                  inputId = "color_no_test_bg",
                                  label =  "No Test Background",
                                  value = "#fad1d0",
                                  showColour = "both",
                                  closeOnClick = T
        ),
        colourpicker::colourInput(width = "80%",
                                  inputId = "color_no_test_border",
                                  label =  "No Test Background",
                                  value = "#9c0000",
                                  showColour = "both",
                                  closeOnClick = T
        ),
        colourpicker::colourInput(width = "80%",
                                  inputId = "color_no_test_highlight",
                                  label =  "No Test Highlight",
                                  value = "#9c0000",
                                  showColour = "both",
                                  closeOnClick = T
        )
      ),
      shiny::column(
        width = 4,
        offset = 0,
        align = "center",
        colourpicker::colourInput(width = "80%",
                                  inputId = "color_with_test_bg",
                                  label =  "With Test Background",
                                  value = "#e6ffe6",
                                  showColour = "both",
                                  closeOnClick = T
        ),
        colourpicker::colourInput(width = "80%",
                                  inputId = "color_with_test_border",
                                  label =  "With Test Border",
                                  value = "#65a765",
                                  showColour = "both",
                                  closeOnClick = T
        ),
        colourpicker::colourInput(width = "80%",
                                  inputId = "color_with_test_highlight",
                                  label =  "With Test Highlight",
                                  value = "#65a765",
                                  showColour = "both",
                                  closeOnClick = T
        )
      ),
      shiny::column(
        width = 4,
        offset = 0,
        align="center",
        colourpicker::colourInput(width = "80%",
                                  inputId = "color_mod_coverage_bg",
                                  label =  "Moderate Coverage Background",
                                  value = "#FFD580",
                                  showColour = "both",
                                  closeOnClick = T
        ),
        colourpicker::colourInput(width = "80%",
                                  inputId = "color_mod_coverage_border",
                                  label =  "Moderate Coverage Border",
                                  value = "#E49B0F",
                                  showColour = "both",
                                  closeOnClick = T
        ),
        colourpicker::colourInput(width = "80%",
                                  inputId = "color_mod_coverage_highlight",
                                  label =  "Moderate Coverage Highlight",
                                  value = "#E49B0F",
                                  showColour = "both",
                                  closeOnClick = T
        )
      )
    ) # close row,
  )



  extra_panel <- shiny::tabPanel(
    "Other options",
    htmltools::br(),
    shiny::column(
      align = "center",
      width = 12,
      offset = 0,
      shiny::sliderInput(
        "moderate_coverage_range",
        "Moderate Coverage Range",
        min = 0,
        max = 1,
        value = c(0.2, 0.8)
      ),
      shiny::checkboxInput("print_isolated_foo", "Print Isolated Functions", value = TRUE),
      shiny::checkboxInput("scale_node_size_by_degree", "Scale Node Size by Degree", value = TRUE)
    )
  )



  # App UI design
  ui <- shiny::fluidPage(
    htmltools::tags$head(# Note the wrapping of the string in HTML()
      htmltools::tags$style(
        htmltools::HTML(
          ".nav-tabs {
          display: flex !important;
          justify-content: center !important;
          width: 100%;
          }
          .col-sm-8 {
          float:none;
          }
          .tab-panel > div {
          width: 100%;
          }
          .nav-tabs>li>a{
          color: black;
          }"
        )
      )),

    # title section at top
    shiny::titlePanel(htmltools::h1("Visualise Project",
                                    align = 'center')),
    # short description
    htmltools::p(
      "Select from the options below and click the 'Visualize' button at the bottom to generate the network graph.",
      align = 'center'
    ),

    # three tabs here created in objects above
    shiny::tabsetPanel(essentials_panel,
                       color_panel,
                       extra_panel),



    # Action button
    htmltools::hr(),
    shiny::column(
      align = "center",
      offset = 0,
      width = 12,
      shiny::actionButton("visualize", "Visualize",
                          style = "background-color: #1f883d; color: white; font-weight: bold")
    )

  ) # close page

  server <- function(input, output, session) {

    shiny::observeEvent(input$visualize, {


      exclude_files <- ifelse(is.null(input$exclude_files), NULL, unlist(strsplit(input$exclude_files, ",")))
      exclude_dirs <- ifelse(is.null(input$exclude_dirs), NULL, unlist(strsplit(input$exclude_dirs, ",")))

      color_no_test <- c("background" = input$color_no_test_bg, "border" = input$color_no_test_border, "highlight" = input$color_no_test_highlight)
      color_with_test <- c("background" = input$color_with_test_bg, "border" = input$color_with_test_border, "highlight" = input$color_with_test_highlight)
      color_mod_coverage <- c("background" = input$color_mod_coverage_bg, "border" = input$color_mod_coverage_border, "highlight" = input$color_mod_coverage_highlight)

      app_env$p_project_path = input$project_path
      app_env$p_foo_path = input$foo_path
      app_env$p_test_path = input$test_path
      app_env$p_exclude_files = exclude_files
      app_env$p_exclude_dirs = exclude_dirs
      app_env$p_run_coverage = input$run_coverage
      app_env$p_color_no_test = color_no_test
      app_env$p_color_with_test = color_with_test
      app_env$p_color_mod_coverage = color_mod_coverage
      app_env$p_moderate_coverage_range = input$moderate_coverage_range
      app_env$p_print_isolated_foo = input$print_isolated_foo
      app_env$p_show_in_shiny = input$show_in_shiny
      app_env$p_network_title = input$network_title
      app_env$p_scale_node_size_by_degree = input$scale_node_size_by_degree

      app_env$closed <- TRUE
      shiny::stopApp()

    })
  }

  # Run the application and capture the reactive value
  inputApp <- shiny::shinyApp(ui = ui, server = server)
  shiny::runApp(inputApp)

  # Wait for the button to be clicked
  while (!app_env$closed) {
    Sys.sleep(0.1)
  }

  assertHE::visualise_project(
    project_path = app_env$p_project_path,
    foo_path = app_env$p_foo_path,
    test_path = app_env$p_test_path,
    exclude_files = app_env$p_exclude_files,
    exclude_dirs = app_env$p_exclude_dirs,
    run_coverage = app_env$p_run_coverage,
    color_no_test = app_env$p_color_no_test,
    color_with_test = app_env$p_color_with_test,
    color_mod_coverage = app_env$p_color_mod_coverage,
    moderate_coverage_range = app_env$p_moderate_coverage_range,
    print_isolated_foo = app_env$p_print_isolated_foo,
    show_in_shiny = app_env$p_show_in_shiny,
    network_title = app_env$p_network_title,
    scale_node_size_by_degree = app_env$p_scale_node_size_by_degree)

}

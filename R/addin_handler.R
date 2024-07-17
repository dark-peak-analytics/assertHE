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

  ui <- shiny::fluidPage(
    shiny::titlePanel("Visualise Project"),
    shiny::actionButton("visualize", "Visualize",
                        style = "background-color: yellow; color: black; font-weight: bold;"),
    shiny::tabsetPanel(
      shiny::tabPanel("Project",
          shiny::textInput("project_path", "Project Path", value = "."),
          shiny::textInput("foo_path", "Foo Path", value = "R"),
          shiny::textInput("test_path", "Test Path", value = NULL),
          shiny::textInput("exclude_files", "Exclude Files (comma-separated)", value = NULL),
          shiny::textInput("exclude_dirs", "Exclude Dirs (comma-separated)", value = NULL),
          shiny::checkboxInput("run_coverage", "Run Coverage", value = FALSE),
      ),
      shiny::tabPanel( "Display options",
        shiny::textInput("color_no_test_bg", "No Test Background Color", value = "#fad1d0"),
        shiny::textInput("color_no_test_border", "No Test Border Color", value = "#9c0000"),
        shiny::textInput("color_no_test_highlight", "No Test Highlight Color", value = "#9c0000"),
        shiny::textInput("color_with_test_bg", "With Test Background Color", value = "#e6ffe6"),
        shiny::textInput("color_with_test_border", "With Test Border Color", value = "#65a765"),
        shiny::textInput("color_with_test_highlight", "With Test Highlight Color", value = "#65a765"),
        shiny::textInput("color_mod_coverage_bg", "Moderate Coverage Background Color", value = "#FFD580"),
        shiny::textInput("color_mod_coverage_border", "Moderate Coverage Border Color", value = "#E49B0F"),
        shiny::textInput("color_mod_coverage_highlight", "Moderate Coverage Highlight Color", value = "#E49B0F")
      ),
      shiny::tabPanel( "Other options",
        shiny::sliderInput("moderate_coverage_range", "Moderate Coverage Range", min = 0, max = 1, value = c(0.2, 0.8)),
        shiny::checkboxInput("print_isolated_foo", "Print Isolated Foo", value = TRUE),
        shiny::checkboxInput("show_in_shiny", "Show in Shiny", value = FALSE),
        shiny::textInput("network_title", "Network Title", value = "Function Network"),
        shiny::checkboxInput("scale_node_size_by_degree", "Scale Node Size by Degree", value = TRUE)
      )
    ),
      shiny::mainPanel(
        shiny::plotOutput("network_plot")
      )
    )

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
  rv <- shiny::runApp(inputApp)

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

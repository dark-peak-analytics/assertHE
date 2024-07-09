

# Addin handler
visualise_project_addin <- function() {
  library(shiny)
  ui <- fluidPage(
    titlePanel("Visualise Project"),
    sidebarLayout(
      sidebarPanel(
        textInput("project_path", "Project Path", value = ""),
        textInput("foo_path", "Foo Path", value = "R"),
        textInput("test_path", "Test Path", value = NULL),
        textInput("exclude_files", "Exclude Files (comma-separated)", value = NULL),
        textInput("exclude_dirs", "Exclude Dirs (comma-separated)", value = NULL),
        checkboxInput("run_coverage", "Run Coverage", value = FALSE),
        textInput("color_no_test_bg", "No Test Background Color", value = "#fad1d0"),
        textInput("color_no_test_border", "No Test Border Color", value = "#9c0000"),
        textInput("color_no_test_highlight", "No Test Highlight Color", value = "#9c0000"),
        textInput("color_with_test_bg", "With Test Background Color", value = "#e6ffe6"),
        textInput("color_with_test_border", "With Test Border Color", value = "#65a765"),
        textInput("color_with_test_highlight", "With Test Highlight Color", value = "#65a765"),
        textInput("color_mod_coverage_bg", "Moderate Coverage Background Color", value = "#FFD580"),
        textInput("color_mod_coverage_border", "Moderate Coverage Border Color", value = "#E49B0F"),
        textInput("color_mod_coverage_highlight", "Moderate Coverage Highlight Color", value = "#E49B0F"),
        sliderInput("moderate_coverage_range", "Moderate Coverage Range", min = 0, max = 1, value = c(0.2, 0.8)),
        checkboxInput("print_isolated_foo", "Print Isolated Foo", value = TRUE),
        checkboxInput("show_in_shiny", "Show in Shiny", value = FALSE),
        textInput("network_title", "Network Title", value = "Function Network"),
        checkboxInput("scale_node_size_by_degree", "Scale Node Size by Degree", value = TRUE),
        actionButton("visualize", "Visualize")
      ),
      mainPanel(
        plotOutput("network_plot")
      )
    )
  )

  server <- function(input, output, session) {
    observeEvent(input$visualize, {
      exclude_files <- ifelse(is.null(input$exclude_files), NULL, unlist(strsplit(input$exclude_files, ",")))
      exclude_dirs <- ifelse(is.null(input$exclude_dirs), NULL, unlist(strsplit(input$exclude_dirs, ",")))

      color_no_test <- c("background" = input$color_no_test_bg, "border" = input$color_no_test_border, "highlight" = input$color_no_test_highlight)
      color_with_test <- c("background" = input$color_with_test_bg, "border" = input$color_with_test_border, "highlight" = input$color_with_test_highlight)
      color_mod_coverage <- c("background" = input$color_mod_coverage_bg, "border" = input$color_mod_coverage_border, "highlight" = input$color_mod_coverage_highlight)

      visualise_project(
        project_path = input$project_path,
        foo_path = input$foo_path,
        test_path = input$test_path,
        exclude_files = exclude_files,
        exclude_dirs = exclude_dirs,
        run_coverage = input$run_coverage,
        color_no_test = color_no_test,
        color_with_test = color_with_test,
        color_mod_coverage = color_mod_coverage,
        moderate_coverage_range = input$moderate_coverage_range,
        print_isolated_foo = input$print_isolated_foo,
        show_in_shiny = input$show_in_shiny,
        network_title = input$network_title,
        scale_node_size_by_degree = input$scale_node_size_by_degree
      )
    })
  }

  shinyApp(ui = ui, server = server)
}

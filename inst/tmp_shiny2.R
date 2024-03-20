ui <- shiny::fluidPage(
  # Enable shinyJs
  shinyjs::useShinyjs(),
  shiny::tags$head(
    shiny::tags$script("
      // Initialize a variable to mirror the value of 'openInShiny'
      var currentOpenInShinyValue = null;

      function openInShiny(file_location) {
        console.log('Executing JavaScript function openInShiny');
        console.log('File location: ' + file_location);

        // Check if the current value is the same as 'file_location'
        if (currentOpenInShinyValue === file_location) {

          // Temporarily set 'openInShiny' and 'close_tab' to null
          // to ensure the change is detected
          Shiny.setInputValue('openInShiny', null, {priority: 'event'});
          Shiny.setInputValue('close_tab', null, {priority: 'event'});

          // Use setTimeout to ensure the null value is processed before setting the new value
          setTimeout(function() {
            Shiny.setInputValue('openInShiny', file_location, {priority: 'event'});
          }, 10);
        } else {

          // Directly set 'openInShiny' to 'file_location' if the values are different
          Shiny.setInputValue('openInShiny', file_location, {priority: 'event'});
        }

        // Update the mirror variable to reflect the new value
        currentOpenInShinyValue = file_location;
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
    shiny::tags$style("
      /* CSS to make tab content scrollable */
      .tab-content {
        overflow-y: auto; /* Enable vertical scrolling */
      }
    ")
  ),
  shiny::fluidRow(
    shiny::column(
      width = 6,
      visNetwork::visNetworkOutput(
        outputId = "networkPlot"
      )
    ),
    shiny::column(
      width = 6,
      # Define a tabsetPanel to contain the dynamic tabs
      shiny::tabsetPanel(
        id = "fileTabs"  # Set an ID for the tabsetPanel
      )
    )
  )
)

server <- function(input, output, session) {
  # Function to create a closable tab
  makeClosableTab <- function(tabName, contentOutputId) {
    shiny::tabPanel(
      title = div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        span(tabName, style = "flex-grow: 1;"),
        actionButton(
          inputId = "close",
          label = HTML("&times;"),
          class = "close-tab",
          onclick = sprintf(
            "Shiny.setInputValue('close_tab', '%s');",
            contentOutputId
          )
        )
      ),
      shiny::verbatimTextOutput(outputId = contentOutputId),
      value = contentOutputId  # Assign the contentOutputId as the tab's value for easy identification
    )
  }

  # Keep track of the current tab's output ID
  currentTabId <- reactiveVal(NULL)

  output$networkPlot <- visNetwork::renderVisNetwork({
    # Create your visNetwork plot here
    visualise_project(
      project_path = "tests/testthat/example_project",
      foo_path = "R",
      test_path = "tests/testthat",
      run_coverage = TRUE
    )
  })

  # Observer to handle opening files in a new tab
  shiny::observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    eventExpr = input$openInShiny, {
    if(input$openInShiny != "") {
      file_location <- input$openInShiny
      file_location <- assertHE::clean_file_path(
        file_location = file_location
      )
      tab_name <- basename(file_location)

      # If there's a current tab open, remove it
      if (!is.null(currentTabId())) {
        print("I found a tab")
        print(currentTabId())
        removeTab(
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

  # Observer to remove the current tab
  observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    eventExpr = input$close_tab, {
      print("detecting closer")
      print(input$close_tab)
      print("compare")
      print(currentTabId())
    if (!is.null(currentTabId())) {
      print("Print closing!")
      removeTab(
        inputId = "fileTabs",
        target = currentTabId()
      )
      # Reset the current tab ID
      currentTabId(NULL)
    }
  })
}

shiny::shinyApp(ui, server)

#' Define head tags for the Shiny app
#'
#' @importFrom htmltools htmlDependency
#' @importFrom shiny tags
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter use_waiter
#' @return Head tags for the Shiny app
app_ui_head_tags <- function() {
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"
    ),
    htmlDependency(
      name = "custom_css",
      version = "0.0.1",
      src = system.file("assets", package = "assertHE"),
      stylesheet = "custom_css.css"
    ),
    htmlDependency(
      name = "custom_js",
      version = "0.0.1",
      src = system.file("assets", package = "assertHE"),
      script = "custom_js.js"
    ),
    useShinyjs(),
    use_waiter()
  )
}

#' Create Shiny app UI
#'
#' @param network_title Character string representing the title of the network to be displayed above the network.
#' @importFrom bslib bs_theme
#' @return Shiny app user interface
app_ui <- function(network_title) {
  # Use bslib theme with flatly preset
  theme <- bs_theme(version = 5, bootswatch = "flatly")

  bslib::page_fluid(
    class = "p-3",
    title = network_title,
    theme = theme,
    app_ui_head_tags(),
    # Define network plot title/subtitle divs
    div(
      class = "title-container",
      span(
        class = "title-text",
        network_title
      ),
      span(
        id = "question-icon-click",
        class = "help-icon-container",
        icon(
          class = "help-icon text-muted",
          name = "question-circle"
        )
      )
    ),
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

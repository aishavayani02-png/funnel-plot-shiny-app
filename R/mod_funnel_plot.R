# Shiny module for the standalone funnel plot interface.

#' Build the funnel plot interface
#' @param id Module ID shared with funnelPlotServer().
#' @return A Shiny page with the controls and results.
funnelPlotUI <- function(id) {
  ns <- NS(id)

  return(fluidPage(
    tags$head(
      tags$style(HTML("
        .well { max-width: 360px; }
        .result-block {
          border-top: 1px solid #ddd;
          padding-top: 12px;
          margin-top: 12px;
        }
        .method-links {
          color: #444;
          font-size: 0.95em;
        }
        .small-note {
          color: #555;
          font-size: 0.95em;
        }
      "))
    ),
    titlePanel("Funnel Plot with Publication Bias Tests"),

    sidebarLayout(
      sidebarPanel(funnelOptionsUI(ns("options"))),
      mainPanel(funnelResultsUI(ns("results")))
    )
  ))
}

#' Connect the funnel plot controls and results
#' @param id Module ID shared with funnelPlotUI().
#' @return The result of registering the results module.
funnelPlotServer <- function(id) {
  return(moduleServer(id, function(input, output, session) {
    settings <- funnelOptionsServer("options")
    return(funnelResultsServer("results", settings))
  }))
}

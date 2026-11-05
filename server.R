# Server
# -----------------------------------------------------------------------------
#' Register the standalone app's funnel plot module
#' @param input,output,session Objects supplied by Shiny.
#' @return The module server result.
server <- function(input, output, session) {
  return(funnelPlotServer("funnel"))
}

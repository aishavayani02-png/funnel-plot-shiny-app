#' Build the funnel plot controls
#' @param id Module ID shared with funnelOptionsServer().
#' @return A list of Shiny controls for the sidebar.
funnelOptionsUI <- function(id) {
  ns <- NS(id)

  return(tagList(
    radioButtons(ns("datatype"), "Type of Data:",
                 choices = c("Binary Outcomes" = "binary",
                             "Continuous Outcomes" = "continuous"),
                 selected = "binary"),

    uiOutput(ns("measure_ui")),

    radioButtons(ns("method"), "Meta-Analysis Model:",
                 choices = c("Fixed-effects" = "FE",
                             "Random-effects (REML)" = "REML"),
                 selected = "FE"),

    radioButtons(ns("reflineMode"), "Reference Line:",
                 choices = c("Current pooled estimate" = "pooled",
                             "User-defined value" = "custom",
                             "No reference line" = "none"),
                 selected = "pooled"),
    uiOutput(ns("custom_refline_ui")),

    radioButtons(ns("yaxis"), "Y-Axis Scale:",
                 choices = c("Standard Error" = "sei",
                             "Inverse Standard Error" = "seinv",
                             "Variance" = "vi",
                             "Inverse Variance" = "vinv"),
                 selected = "sei"),

    uiOutput(ns("xscale_ui")),

    radioButtons(ns("regionShading"), "Region Shading:",
                 choices = c("None" = "none",
                             "Contour-enhanced p-value regions" = "contour",
                             "Heterogeneity band" = "heterogeneity"),
                 selected = "none"),

    uiOutput(ns("biastest_ui")),
    conditionalPanel(
      condition = "input.biastest == 'egger'",
      checkboxInput(ns("showTestLine"), "Show Egger regression line", value = FALSE),
      ns = ns
    ),

    radioButtons(ns("showLimit"), "Limit Estimate Lines:",
                 choices = c("No" = "no", "Yes" = "yes"),
                 selected = "no"),
    conditionalPanel(
      condition = "input.showLimit == 'yes'",
      checkboxGroupInput(ns("limitPredictors"), "Limit Line Predictors:",
                         choices = c("Standard error" = "sei",
                                     "Sampling variance" = "vi"),
                         selected = "sei"),
      checkboxInput(ns("limitExtrapolate"), "Extrapolate to SE = 0", value = TRUE),
      ns = ns
    ),

    radioButtons(ns("trimfill"), "Trim-and-Fill Adjustment:",
                 choices = c("No" = "no", "Yes" = "yes"),
                 selected = "no"),

    radioButtons(ns("labelMode"), "Study Labels:",
                 choices = c("None" = "none",
                             "Label all studies inside plot" = "all",
                             "Number studies and show key" = "key"),
                 selected = "none"),

    sliderInput(ns("plotHeight"), "Plot Height:",
                min = 420, max = 820, value = 560, step = 20)
  ))
}

#' Register the controls and expose their current settings
#' @param id Module ID shared with funnelOptionsUI().
#' @return A named list of reactives for the selected analysis and plot settings.
funnelOptionsServer <- function(id) {
  return(moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$measure_ui <- renderUI({
      if (identical(input$datatype, "binary")) {
        radioButtons(ns("measure"), "Effect Size:",
                     choices = c("Odds Ratio" = "OR",
                                 "Risk Ratio" = "RR",
                                 "Risk Difference" = "RD"),
                     selected = "OR")
      } else {
        radioButtons(ns("measure"), "Effect Size:",
                     choices = c("Mean Difference" = "MD",
                                 "Standardized Mean Difference" = "SMD"),
                     selected = "MD")
      }
    })

    output$xscale_ui <- renderUI({
      req(input$datatype, input$measure)
      if (is_log_effect(input$datatype, input$measure)) {
        checkboxInput(ns("naturalScale"),
                      paste0("Show x-axis on natural ", input$measure, " scale"),
                      value = TRUE)
      }
    })

    output$biastest_ui <- renderUI({
      req(input$datatype)

      choices <- c("None" = "none", "Egger's test" = "egger")
      if (identical(input$datatype, "binary")) {
        choices <- c(choices, "Peters' test" = "peters")
      }

      current <- isolate(input$biastest)
      selected <- if (!is.null(current) && current %in% unname(choices)) {
        current
      } else {
        "none"
      }

      radioButtons(ns("biastest"), "Publication Bias Test:",
                   choices = choices,
                   selected = selected)
    })

    output$custom_refline_ui <- renderUI({
      req(input$datatype, input$measure)
      if (!identical(input$reflineMode, "custom")) {
        return(NULL)
      }

      natural <- isTRUE(input$naturalScale)
      if (is_log_effect(input$datatype, input$measure) && natural) {
        label <- paste0("Custom reference line (", input$measure, " scale):")
        default_value <- 1
      } else {
        label <- "Custom reference line (analysis scale):"
        default_value <- 0
      }

      numericInput(ns("customRefline"), label, value = default_value, step = 0.1)
    })

    return(list(
      datatype = reactive(input$datatype),
      measure = reactive(input$measure),
      method = reactive(input$method),
      refline_mode = reactive(input$reflineMode),
      custom_refline = reactive(input$customRefline),
      natural_scale = reactive(input$naturalScale),
      yaxis = reactive(input$yaxis),
      region_shading = reactive(input$regionShading),
      biastest = reactive(input$biastest),
      show_test_line = reactive(input$showTestLine),
      show_limit = reactive(input$showLimit),
      limit_predictors = reactive(input$limitPredictors),
      limit_extrapolate = reactive(input$limitExtrapolate),
      trimfill = reactive(input$trimfill),
      label_mode = reactive(input$labelMode),
      plot_height = reactive(input$plotHeight)
    ))
  }))
}

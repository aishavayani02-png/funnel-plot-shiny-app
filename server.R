# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  output$measure_ui <- renderUI({
    if (identical(input$datatype, "binary")) {
      radioButtons("measure", "Effect Size:",
                   choices = c("Odds Ratio" = "OR",
                               "Risk Ratio" = "RR",
                               "Risk Difference" = "RD"),
                   selected = "OR")
    } else {
      radioButtons("measure", "Effect Size:",
                   choices = c("Mean Difference" = "MD",
                               "Standardized Mean Difference" = "SMD"),
                   selected = "MD")
    }
  })

  output$xscale_ui <- renderUI({
    req(input$datatype, input$measure)
    if (is_log_effect(input$datatype, input$measure)) {
      checkboxInput("naturalScale",
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
    selected <- if (!is.null(current) && current %in% unname(choices)) current else "none"

    radioButtons("biastest", "Publication Bias Test:",
                 choices = choices,
                 selected = selected)
  })

  output$custom_refline_ui <- renderUI({
    req(input$datatype, input$measure)
    if (!identical(input$reflineMode, "custom")) return(NULL)

    natural <- isTRUE(input$naturalScale)
    label <- if (is_log_effect(input$datatype, input$measure) && natural) {
      paste0("Custom reference line (", input$measure, " scale):")
    } else {
      "Custom reference line (analysis scale):"
    }
    default_value <- if (is_log_effect(input$datatype, input$measure) && natural) 1 else 0

    numericInput("customRefline", label, value = default_value, step = 0.1)
  })

  effect_data <- reactive({
    req(input$datatype, input$measure)
    prepare_effect_data(input$datatype, input$measure)
  })

  model <- reactive({
    fit_meta_model(effect_data(), input$method)
  })

  x_uses_natural_scale <- reactive({
    is_log_effect(input$datatype, input$measure) && isTRUE(input$naturalScale)
  })

  selected_limit_predictors <- reactive({
    predictors <- input$limitPredictors
    if (is.null(predictors)) character(0) else predictors
  })

  selected_refline <- reactive({
    res <- model()
    req(res)
    select_refline(res,
                   input$reflineMode,
                   input$customRefline,
                   input$datatype,
                   input$measure,
                   x_uses_natural_scale())
  })

  trimfill_model <- reactive({
    res <- model()
    req(res)
    run_trimfill(res, input$trimfill)
  })

  bias_test <- reactive({
    res <- model()
    req(res)
    run_bias_test(res, input$biastest, input$datatype)
  })

  pvalue_distribution <- reactive({
    res <- model()
    req(res)
    study_pvalue_distribution(res)
  })

  limit_rows <- reactive({
    res <- model()
    req(res)
    limit_line_estimates(res,
                         input$showLimit,
                         selected_limit_predictors(),
                         isTRUE(input$limitExtrapolate),
                         input$datatype,
                         input$measure,
                         x_uses_natural_scale())
  })

  plot_options <- reactive({
    req(input$datatype, input$measure, input$method, input$yaxis)
    list(
      datatype = input$datatype,
      measure = input$measure,
      method = input$method,
      yaxis = input$yaxis,
      natural_scale = x_uses_natural_scale(),
      refline_mode = input$reflineMode,
      region_shading = input$regionShading,
      biastest = input$biastest,
      show_test_line = isTRUE(input$showTestLine),
      show_limit = input$showLimit,
      limit_predictors = selected_limit_predictors(),
      limit_extrapolate = isTRUE(input$limitExtrapolate),
      label_mode = input$labelMode
    )
  })

  analysis_lines <- reactive({
    res <- model()
    req(res)
    analysis_results_lines(res,
                           input$datatype,
                           input$measure,
                           input$method,
                           x_uses_natural_scale(),
                           input$biastest,
                           bias_test(),
                           input$showLimit,
                           limit_rows(),
                           input$trimfill,
                           trimfill_model(),
                           input$regionShading)
  })

  output$funnelMain <- renderPlot({
    res <- model()
    validate(need(!is.null(res), "Waiting for a valid model..."))
    draw_funnel_plot(res,
                     effect_data(),
                     plot_options(),
                     trimfill_model(),
                     selected_refline())
  }, height = function() input$plotHeight)

  output$analysisSummary <- renderUI({
    res <- model()
    validate(need(!is.null(res), "Waiting for a valid model..."))

    HTML(analysis_summary_html(res,
                               input$datatype,
                               input$measure,
                               input$method,
                               x_uses_natural_scale(),
                               input$reflineMode))
  })

  output$biasTestOutput <- renderUI({
    if (identical(input$biastest, "none")) return(NULL)
    if (identical(input$biastest, "peters") && !identical(input$datatype, "binary")) return(NULL)

    res <- model()
    validate(need(!is.null(res), "Waiting for a valid model..."))

    HTML(bias_test_html(bias_test(),
                        input$biastest,
                        input$datatype,
                        input$measure,
                        x_uses_natural_scale(),
                        isTRUE(input$showTestLine)))
  })

  output$limitLineOutput <- renderUI({
    if (!identical(input$showLimit, "yes")) return(NULL)

    res <- model()
    validate(need(!is.null(res), "Waiting for a valid model..."))

    HTML(limit_line_html(limit_rows()))
  })

  output$trimfillInfo <- renderUI({
    if (!identical(input$trimfill, "yes")) return(NULL)

    HTML(trimfill_html(trimfill_model(),
                       input$datatype,
                       input$measure,
                       x_uses_natural_scale()))
  })

  output$regionInfo <- renderUI({
    if (identical(input$regionShading, "none")) return(NULL)

    counts <- if (identical(input$regionShading, "contour")) {
      pvalue_distribution()
    } else {
      NULL
    }

    HTML(region_info_html(input$regionShading, input$method, counts))
  })

  output$studyKey <- renderUI({
    if (!identical(input$labelMode, "key")) return(NULL)
    HTML(study_key_html(effect_data()$labels))
  })

  output$methodInfo <- renderUI({
    HTML(method_info_html(input$regionShading,
                          input$biastest,
                          input$datatype,
                          input$showLimit,
                          input$trimfill))
  })

  output$downloadCurrentPlot <- downloadHandler(
    filename = function() {
      paste0("funnel_plot_", Sys.Date(), ".", tolower(input$formatCurrent))
    },
    content = function(file) {
      res <- model()
      if (is.null(res)) return()

      include_results <- isTRUE(input$downloadResults)
      include_key <- identical(input$labelMode, "key")
      extra_height <- 1.4
      extra_height <- extra_height + if (include_results) 2 else 0
      extra_height <- extra_height + if (include_key) 1.8 else 0
      height_in <- max(6, min(15, input$plotHeight / 80 + extra_height))

      if (identical(input$formatCurrent, "PDF")) {
        grDevices::pdf(file, width = 9, height = height_in)
      } else {
        grDevices::png(file, width = 9, height = height_in, units = "in", res = 300)
      }

      if (include_results || include_key) {
        heights <- c(4.8,
                     0.9,
                     if (include_key) 1.1 else NULL,
                     if (include_results) 1.25 else NULL)
        layout(matrix(seq_along(heights), ncol = 1), heights = heights)
        legend_info <- draw_funnel_plot(res,
                                        effect_data(),
                                        plot_options(),
                                        trimfill_model(),
                                        selected_refline(),
                                        draw_legend = FALSE)
        draw_plot_legend_footer(legend_info)
        if (include_key) draw_study_key_footer(study_key_lines(effect_data()$labels))
        if (include_results) draw_results_footer(analysis_lines())
        layout(1)
      } else {
        heights <- c(4.8, 0.9)
        layout(matrix(seq_along(heights), ncol = 1), heights = heights)
        legend_info <- draw_funnel_plot(res,
                                        effect_data(),
                                        plot_options(),
                                        trimfill_model(),
                                        selected_refline(),
                                        draw_legend = FALSE)
        draw_plot_legend_footer(legend_info)
        layout(1)
      }
      grDevices::dev.off()
    }
  )

  output$tabs_ui <- renderUI({
    plot_h <- paste0(input$plotHeight, "px")
    tags$div(
      h3("Funnel Plot"),
      plotOutput("funnelMain", height = plot_h),
      uiOutput("studyKey"),
      uiOutput("analysisSummary"),
      uiOutput("biasTestOutput"),
      uiOutput("limitLineOutput"),
      uiOutput("trimfillInfo"),
      uiOutput("regionInfo"),
      uiOutput("methodInfo"),
      tags$hr(),
      radioButtons("formatCurrent", "Download current plot as:",
                   choices = c("PDF", "PNG"), inline = TRUE),
      checkboxInput("downloadResults", "Include analysis results below downloaded plot", value = FALSE),
      downloadButton("downloadCurrentPlot", "Download Plot"),
      tags$hr()
    )
  })
}

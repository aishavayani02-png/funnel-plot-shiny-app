#' Build the funnel plot, summaries and download controls
#' @param id Module ID shared with funnelResultsServer().
#' @return A list of Shiny outputs and download controls.
funnelResultsUI <- function(id) {
  ns <- NS(id)

  return(tagList(
    h3("Funnel Plot"),
    uiOutput(ns("plot_ui")),
    uiOutput(ns("studyKey")),
    uiOutput(ns("analysisSummary")),
    uiOutput(ns("biasTestOutput")),
    uiOutput(ns("limitLineOutput")),
    uiOutput(ns("trimfillInfo")),
    uiOutput(ns("regionInfo")),
    uiOutput(ns("methodInfo")),
    tags$hr(),
    uiOutput(ns("download_ui")),
    tags$hr()
  ))
}

#' Update the funnel plot and results as settings change
#' @param id Module ID shared with funnelResultsUI().
#' @param settings Named list of reactives, as returned by funnelOptionsServer().
#' @return The value returned by shiny::moduleServer().
funnelResultsServer <- function(id, settings) {
  return(moduleServer(id, function(input, output, session) {
    ns <- session$ns

    effect_data <- reactive({
      req(settings$datatype(), settings$measure())
      prepare_effect_data(settings$datatype(), settings$measure())
    })

    model <- reactive({
      fit_meta_model(effect_data(), settings$method())
    })

    x_uses_natural_scale <- reactive({
      is_log_effect(settings$datatype(), settings$measure()) && isTRUE(settings$natural_scale())
    })

    selected_limit_predictors <- reactive({
      predictors <- settings$limit_predictors()
      if (is.null(predictors)) {
        return(character(0))
      }
      predictors
    })

    selected_refline <- reactive({
      res <- model()
      req(res)
      select_refline(res,
                     settings$refline_mode(),
                     settings$custom_refline(),
                     settings$datatype(),
                     settings$measure(),
                     x_uses_natural_scale())
    })

    trimfill_model <- reactive({
      res <- model()
      req(res)
      run_trimfill(res, settings$trimfill())
    })

    bias_test <- reactive({
      res <- model()
      req(res)
      run_bias_test(res, settings$biastest(), settings$datatype())
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
                           settings$show_limit(),
                           selected_limit_predictors(),
                           isTRUE(settings$limit_extrapolate()),
                           settings$datatype(),
                           settings$measure(),
                           x_uses_natural_scale())
    })

    plot_options <- reactive({
      req(settings$datatype(), settings$measure(), settings$method(), settings$yaxis())
      list(
        datatype = settings$datatype(),
        measure = settings$measure(),
        method = settings$method(),
        yaxis = settings$yaxis(),
        natural_scale = x_uses_natural_scale(),
        refline_mode = settings$refline_mode(),
        region_shading = settings$region_shading(),
        biastest = settings$biastest(),
        show_test_line = isTRUE(settings$show_test_line()),
        show_limit = settings$show_limit(),
        limit_predictors = selected_limit_predictors(),
        limit_extrapolate = isTRUE(settings$limit_extrapolate()),
        label_mode = settings$label_mode()
      )
    })

    analysis_lines <- reactive({
      res <- model()
      req(res)
      analysis_results_lines(res,
                             settings$datatype(),
                             settings$measure(),
                             settings$method(),
                             x_uses_natural_scale(),
                             settings$biastest(),
                             bias_test(),
                             settings$show_limit(),
                             limit_rows(),
                             settings$trimfill(),
                             trimfill_model(),
                             settings$region_shading())
    })

    output$funnelMain <- renderPlot({
      res <- model()
      validate(need(!is.null(res), "Waiting for a valid model..."))
      draw_funnel_plot(res,
                       effect_data(),
                       plot_options(),
                       trimfill_model(),
                       selected_refline())
    }, height = function() settings$plot_height())

    output$analysisSummary <- renderUI({
      res <- model()
      validate(need(!is.null(res), "Waiting for a valid model..."))

      HTML(analysis_summary_html(res,
                                 settings$datatype(),
                                 settings$measure(),
                                 settings$method(),
                                 x_uses_natural_scale(),
                                 settings$refline_mode()))
    })

    output$biasTestOutput <- renderUI({
      if (identical(settings$biastest(), "none")) {
        return(NULL)
      }
      if (identical(settings$biastest(), "peters") && !identical(settings$datatype(), "binary")) {
        return(NULL)
      }

      res <- model()
      validate(need(!is.null(res), "Waiting for a valid model..."))

      HTML(bias_test_html(bias_test(),
                          settings$biastest(),
                          settings$datatype(),
                          settings$measure(),
                          x_uses_natural_scale(),
                          isTRUE(settings$show_test_line())))
    })

    output$limitLineOutput <- renderUI({
      if (!identical(settings$show_limit(), "yes")) {
        return(NULL)
      }

      res <- model()
      validate(need(!is.null(res), "Waiting for a valid model..."))

      HTML(limit_line_html(limit_rows()))
    })

    output$trimfillInfo <- renderUI({
      if (!identical(settings$trimfill(), "yes")) {
        return(NULL)
      }

      HTML(trimfill_html(trimfill_model(),
                         settings$datatype(),
                         settings$measure(),
                         x_uses_natural_scale()))
    })

    output$regionInfo <- renderUI({
      if (identical(settings$region_shading(), "none")) {
        return(NULL)
      }

      counts <- if (identical(settings$region_shading(), "contour")) {
        pvalue_distribution()
      } else {
        NULL
      }

      HTML(region_info_html(settings$region_shading(), settings$method(), counts))
    })

    output$studyKey <- renderUI({
      if (!identical(settings$label_mode(), "key")) {
        return(NULL)
      }
      HTML(study_key_html(effect_data()$labels))
    })

    output$methodInfo <- renderUI({
      HTML(method_info_html(settings$region_shading(),
                            settings$biastest(),
                            settings$datatype(),
                            settings$show_limit(),
                            settings$trimfill()))
    })

    output$downloadCurrentPlot <- downloadHandler(
      filename = function() {
        paste0("funnel_plot_", Sys.Date(), ".", tolower(input$formatCurrent))
      },
      content = function(file) {
        res <- model()
        if (is.null(res)) {
          return()
        }

        include_results <- isTRUE(input$downloadResults)
        include_key <- identical(settings$label_mode(), "key")
        extra_height <- 1.4
        if (include_results) {
          extra_height <- extra_height + 2
        }
        if (include_key) {
          extra_height <- extra_height + 1.8
        }
        height_in <- max(6, min(15, settings$plot_height() / 80 + extra_height))

        if (identical(input$formatCurrent, "PDF")) {
          grDevices::pdf(file, width = 9, height = height_in)
        } else {
          grDevices::png(file, width = 9, height = height_in, units = "in", res = 300)
        }

        heights <- c(4.8, 0.9)
        if (include_key) {
          heights <- c(heights, 1.1)
        }
        if (include_results) {
          heights <- c(heights, 1.25)
        }

        layout(matrix(seq_along(heights), ncol = 1), heights = heights)
        legend_info <- draw_funnel_plot(res,
                                        effect_data(),
                                        plot_options(),
                                        trimfill_model(),
                                        selected_refline(),
                                        draw_legend = FALSE)
        draw_plot_legend_footer(legend_info)
        if (include_key) {
          draw_study_key_footer(effect_data()$labels)
        }
        if (include_results) {
          draw_results_footer(analysis_lines())
        }
        layout(1)
        return(grDevices::dev.off())
      }
    )

    output$plot_ui <- renderUI({
      plotOutput(ns("funnelMain"), height = paste0(settings$plot_height(), "px"))
    })

    # Rebuild the download controls on resize, as in the original layout.
    output$download_ui <- renderUI({
      req(settings$plot_height())
      tagList(
        radioButtons(ns("formatCurrent"), "Download current plot as:",
                     choices = c("PDF", "PNG"), inline = TRUE),
        checkboxInput(ns("downloadResults"), "Include analysis results below downloaded plot", value = FALSE),
        downloadButton(ns("downloadCurrentPlot"), "Download Plot")
      )
    })
  }))
}

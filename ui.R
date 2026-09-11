# UI
# -----------------------------------------------------------------------------
ui <- fluidPage(
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
    sidebarPanel(
      radioButtons("datatype", "Type of Data:",
                   choices = c("Binary Outcomes" = "binary",
                               "Continuous Outcomes" = "continuous"),
                   selected = "binary"),

      uiOutput("measure_ui"),

      radioButtons("method", "Meta-Analysis Model:",
                   choices = c("Fixed-effects" = "FE",
                               "Random-effects (REML)" = "REML"),
                   selected = "FE"),

      radioButtons("reflineMode", "Reference Line:",
                   choices = c("Current pooled estimate" = "pooled",
                               "User-defined value" = "custom",
                               "No reference line" = "none"),
                   selected = "pooled"),
      uiOutput("custom_refline_ui"),

      radioButtons("yaxis", "Y-Axis Scale:",
                   choices = c("Standard Error" = "sei",
                               "Inverse Standard Error" = "seinv",
                               "Variance" = "vi",
                               "Inverse Variance" = "vinv"),
                   selected = "sei"),

      uiOutput("xscale_ui"),

      radioButtons("regionShading", "Region Shading:",
                   choices = c("None" = "none",
                               "Contour-enhanced p-value regions" = "contour",
                               "Heterogeneity band" = "heterogeneity"),
                   selected = "none"),

      uiOutput("biastest_ui"),
      conditionalPanel(
        condition = "input.biastest == 'egger'",
        checkboxInput("showTestLine", "Show Egger regression line", value = FALSE)
      ),

      radioButtons("showLimit", "Limit Estimate Lines:",
                   choices = c("No" = "no", "Yes" = "yes"),
                   selected = "no"),
      conditionalPanel(
        condition = "input.showLimit == 'yes'",
        checkboxGroupInput("limitPredictors", "Limit Line Predictors:",
                           choices = c("Standard error" = "sei",
                                       "Sampling variance" = "vi"),
                           selected = "sei"),
        checkboxInput("limitExtrapolate", "Extrapolate to SE = 0", value = TRUE)
      ),

      radioButtons("trimfill", "Trim-and-Fill Adjustment:",
                   choices = c("No" = "no", "Yes" = "yes"),
                   selected = "no"),

      radioButtons("labelMode", "Study Labels:",
                   choices = c("None" = "none",
                               "Label all studies inside plot" = "all",
                               "Number studies and show key" = "key"),
                   selected = "none"),

      sliderInput("plotHeight", "Plot Height:",
                  min = 420, max = 820, value = 560, step = 20)
    ),

    mainPanel(
      uiOutput("tabs_ui")
    )
  )
)

# -----------------------------------------------------------------------------

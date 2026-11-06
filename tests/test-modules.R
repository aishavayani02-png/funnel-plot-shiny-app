# Run from the repository root with Rscript tests/test-modules.R.
shiny::testServer(".", {
  session$setInputs(
    `funnel-options-datatype` = "binary",
    `funnel-options-measure` = "OR",
    `funnel-options-method` = "FE",
    `funnel-options-reflineMode` = "pooled",
    `funnel-options-yaxis` = "sei",
    `funnel-options-regionShading` = "none",
    `funnel-options-biastest` = "peters",
    `funnel-options-showLimit` = "no",
    `funnel-options-trimfill` = "no",
    `funnel-options-labelMode` = "none",
    `funnel-options-naturalScale` = TRUE,
    `funnel-options-plotHeight` = 560
  )
  stopifnot(
    grepl("Odds Ratio", session$getOutput("funnel-results-analysisSummary")$html, fixed = TRUE),
    grepl("Peters", session$getOutput("funnel-results-biasTestOutput")$html, fixed = TRUE),
    grepl("560px", session$getOutput("funnel-results-plot_ui")$html, fixed = TRUE)
  )

  session$setInputs(
    `funnel-options-datatype` = "continuous",
    `funnel-options-measure` = "MD",
    `funnel-options-biastest` = "egger",
    `funnel-options-plotHeight` = 700
  )
  stopifnot(
    grepl("Mean Difference", session$getOutput("funnel-results-analysisSummary")$html, fixed = TRUE),
    grepl("Egger", session$getOutput("funnel-results-biasTestOutput")$html, fixed = TRUE),
    grepl("700px", session$getOutput("funnel-results-plot_ui")$html, fixed = TRUE)
  )
})

message("Module checks passed.")

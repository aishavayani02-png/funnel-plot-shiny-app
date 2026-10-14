# Funnel Plot Shiny App

This is a standalone Shiny app for exploring funnel plot variations, publication bias tests, limit estimate lines, trim-and-fill adjustment, study labelling, plot legends and downloadable plots.

The app currently uses example datasets from the `metadat` package. The code has been split up so that the statistical and plotting work is not tied too tightly to the standalone Shiny app. That should make it easier to reuse these pieces later in MetaPairwise.

## Structure

The standalone app still runs through the usual Shiny files:

- `global.R` loads packages, sets shared constants, and sources the files in `R/`.
- `ui.R` calls the funnel plot module UI.
- `server.R` calls the funnel plot module server.

Most of the work now lives in the `R/` folder:

- `R/mod_funnel_plot.R` contains the Shiny module, `funnelPlotUI()` and `funnelPlotServer()`. This is where the user controls and Shiny reactive wiring sit.
- `R/helpers.R` contains small shared helpers for labels, number formatting, p-values, scale conversions and y-axis transformations.
- `R/model_analysis.R` prepares the example data, fits the meta-analysis models, runs bias tests, trim-and-fill, p-value counts and limit-line calculations.
- `R/plotting.R` draws the funnel plot, plot legends, study labels/key and the extra footer panels used in downloaded plots.
- `R/results_text.R` builds the text shown below the plot, including analysis results, method notes, study key text and further reading links.

## Requirements

Install the required R packages:

```r
install.packages(c("shiny", "metafor", "metadat"))
```

## Run The App

From this folder:

```r
shiny::runApp(".")
```

Or from another R session:

```r
shiny::runApp("path/to/funnel-plot-shiny-app")
```

Or directly from GitHub:

```r
shiny::runGitHub("funnel-plot-shiny-app", "aishavayani02-png", ref = "main")
```

## Notes

This repository currently contains the standalone app only. It still fits the example meta-analysis models internally from raw example datasets, then draws the funnel plot and associated results from those fitted models.

For MetaPairwise integration, the app now has two useful layers: reusable analysis/plotting functions, and a Shiny module wrapped around them. The reproducible script-download system in MetaPairwise has not been connected yet.

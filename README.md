# Funnel Plot Shiny App

Standalone Shiny app for exploring funnel plot variations, publication bias tests, limit estimate lines, trim-and-fill adjustment, study labelling, plot legends, and downloadable plots.

The current version uses example datasets from the `metadat` package. It is intended to be shared for review and as the starting point for later integration into the MetaPairwise app.

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

## Notes For MetaPairwise Integration

This repository currently contains the standalone app only. The app fits the example meta-analysis models internally, then draws the funnel plot and associated results from those fitted models.

For MetaPairwise integration, the plotting and output functions may later need to be connected to MetaPairwise's existing data and model objects. The reproducible script-download system in MetaPairwise has not been connected at this stage.


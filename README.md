# Funnel Plot Shiny App

Standalone Shiny app for exploring funnel plot variations, publication bias tests, limit estimate lines, trim-and-fill adjustment, study labelling, plot legends and downloadable plots.

The current version uses example datasets from the `metadat` package. It is intended to be the starting point for later integration into MetaPairwise.

## Structure

The app uses the standard Shiny split-file structure:

- `global.R`: package loading, constants, and shared helper functions.
- `ui.R`: user interface layout and controls.
- `server.R`: reactive data/model code, plotting, result text, and download logic.

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

## Notes
This repository currently contains the standalone app only. The app fits the example meta-analysis models internally, then draws the funnel plot and associated results from those fitted models.

For MetaPairwise integration, the plotting and output functions may later need to be connected to MetaPairwise's existing data and model objects. The reproducible script-download system in MetaPairwise has not been connected at this stage.

# Funnel Plot Shiny App

Standalone Shiny app for exploring funnel plot variations, publication bias tests, limit estimate lines, trim-and-fill adjustment, study labelling, plot legends and downloadable plots.

The current version uses example datasets from the `metadat` package. It is intended to be the starting point for later integration into MetaPairwise.

## Structure

The app uses the standard Shiny split-file structure with reusable business logic separated into `R/` files:

- `global.R`: package loading, constants, and sourcing of shared functions.
- `ui.R`: user interface layout and controls.
- `server.R`: Shiny reactive wiring and output definitions.
- `R/helpers.R`: shared formatting, scale conversion, labels, and small utility functions.
- `R/model_analysis.R`: example data preparation, model fitting, bias tests, trim-and-fill, p-value counts, and limit-line estimates.
- `R/plotting.R`: funnel plot generation, plot legends, study key footer, and download plot footers.
- `R/results_text.R`: app result text, further-information links, and download result summaries.

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
This repository currently contains the standalone app only. The app fits the example meta-analysis models internally from raw example datasets, then draws the funnel plot and associated results from those fitted models.

For MetaPairwise integration, the analysis and plotting functions have been separated from Shiny-specific code so they can be reused more easily. The reproducible script-download system in MetaPairwise has not been connected at this stage.

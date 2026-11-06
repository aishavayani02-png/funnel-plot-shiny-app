# Funnel Plot Shiny App

This is a standalone Shiny app for exploring funnel plot variations, publication bias tests, limit estimate lines, trim-and-fill adjustment, study labelling, plot legends and downloadable plots.

The app currently uses example datasets from the metadat package. The code has been split up so that the statistical and plotting work is not tied too tightly to the Shiny app. That should make it easier to reuse these pieces later in MetaPairwise.

## Structure

The app still runs through the usual Shiny files:

- `global.R` loads the packages and sets the shared plot defaults. Shiny loads the files in `R/` when the app starts.
- `ui.R` calls the funnel plot module UI.
- `server.R` calls the funnel plot module server.

Most of the work now lives in the R folder:

- `R/mod_funnel_plot.R` puts the options and results panels together.
- `R/mod_funnel_options.R` builds the sidebar controls and returns their settings as a named list of reactives.
- `R/mod_funnel_results.R` uses those settings to update the plot, summaries and downloads.
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

## Tests

Run these checks from the repository root in a terminal:

```sh
Rscript tests/test-analysis.R
Rscript tests/test-modules.R
```

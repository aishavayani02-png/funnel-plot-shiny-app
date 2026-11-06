# Funnel Plot Shiny App

A Shiny app for exploring funnel plots using binary and continuous example datasets from `metadat`. It supports asymmetry tests, limit estimates, trim-and-fill, and exports to PNG and PDF.

## Structure

The app starts from `ui.R` and `server.R`. The packages and plot defaults are set in `global.R`. Shiny loads the files in `R/` automatically.

- `R/mod_funnel_plot.R` puts the options and results panels together.
- `R/mod_funnel_options.R` builds the sidebar controls and returns their settings as a named list of reactives.
- `R/mod_funnel_results.R` uses those settings to update the plot, summaries and downloads.
- `R/helpers.R` handles shared labels, number formatting and effect scales.
- `R/model_analysis.R` prepares the study data, fits the models and runs the analysis.
- `R/plotting.R` draws the plot, legend and export footers.
- `R/results_text.R` formats the summaries, study key and reading links.

The analysis and plotting functions take ordinary R objects so they can be reused when the app is integrated into MetaPairwise.

## Requirements

Install the required R packages:

```r
install.packages(c("shiny", "metafor", "metadat"))
```

## Running the app

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

To try a different branch, set `ref` to its name.

## Tests

Run these checks from the repository root in a terminal:

```sh
Rscript tests/test-analysis.R
Rscript tests/test-modules.R
```

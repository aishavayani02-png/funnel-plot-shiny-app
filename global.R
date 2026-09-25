# =============================================================================
# Shiny App: Interactive Funnel Plot with bias tests and trim-and-fill
# =============================================================================

library(shiny)
library(metafor)
library(metadat)

plot_digits <- 2L
plot_steps <- 5L
plot_level <- 95

source("R/helpers.R", local = TRUE)
source("R/model_analysis.R", local = TRUE)
source("R/plotting.R", local = TRUE)
source("R/results_text.R", local = TRUE)

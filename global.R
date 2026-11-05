# =============================================================================
# Shiny App: Interactive Funnel Plot with bias tests and trim-and-fill
# =============================================================================

library(shiny)
library(metafor)
library(metadat)

plot_digits <- 2L
plot_steps <- 5L
plot_level <- 95

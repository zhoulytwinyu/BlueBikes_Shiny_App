#!/usr/bin/env Rscript
library(shiny)

options(shiny.port=5050)
options(shiny.autoreload=TRUE)
runApp("BlueBikesShinyApp")

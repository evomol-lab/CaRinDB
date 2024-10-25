# Load libraries
library(shiny)
library(here)

options(shiny.autoreload = TRUE)

# Run on local RStudio
runApp(appDir = here())

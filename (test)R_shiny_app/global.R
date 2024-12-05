# global.R
library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)

# Source all helper files
source("R_shiny_app/helper_files/data_processing.R")
source("R_shiny_app/helper_files/plot_functions.R")
source("R_shiny_app/helper_files/plot_styling.R")
source("R_shiny_app/helper_files/utils.R")

# Load and preprocess data
shiny.merged.temp <- read.csv("data/processed_data/shiny_merged_temp.csv")

shiny.merged.precip <- read.csv("data/processed_data/shiny_merged_precip.csv")

shiny.merged.anom <- read.csv("data/processed_data/shiny_merged_anom.csv")

shiny.merged.precip.anom <- read.csv("data/processed_data/shiny_merged_precip_anom.csv")

# ... other data loading
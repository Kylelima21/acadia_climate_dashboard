#global 

#### R shiny dashboard displaying climate data from NOAA (nClimGrid) and McFarland Hill station in Acadia NP ####

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(shiny)
library(shinydashboard)
library(fresh)
library(ggplot2)
library(readr)
library(plotly)
library(tidyverse)
library(dplyr)

#-----------------------#
####    Read Data    ####
#-----------------------#

shiny.merged.temp <- read.csv("data/processed_data/shiny_merged_temp.csv")

shiny.merged.precip <- read.csv("data/processed_data/shiny_merged_precip.csv")

shiny.merged.anom <- read.csv("data/processed_data/shiny_merged_anom.csv")

shiny.merged.precip.anom <- read.csv("data/processed_data/shiny_merged_precip_anom.csv")

shiny.monthly.records <- read.csv("data/processed_data/shiny_monthly_records.csv")

shiny.monthly.precip.records <- read.csv("data/processed_data/shiny_monthly_precip_records.csv")

shiny.daily.temp.records <- read.csv("data/processed_data/shiny_daily_temp_records.csv")


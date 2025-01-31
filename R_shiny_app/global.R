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
library(leaflet)

#-----------------------#
####    Read Data    ####
#-----------------------#

temp.data.merged <- read.csv("data/processed_data/temp_data_merged.csv")

precip.data.merged <- read.csv("data/processed_data/precip_data_merged.csv")

anom.temp.merged <- read.csv("data/processed_data/anom_temp_merged.csv")

anom.precip.merged <- read.csv("data/processed_data/anom_precip_merged.csv")

records.noaa.daily <- read.csv("data/processed_data/records_noaa_daily.csv")

records.noaa.monthly <- read.csv("data/processed_data/records_noaa_monthly.csv")


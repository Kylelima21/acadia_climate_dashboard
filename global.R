#global 

#### R shiny dashboard displaying climate data from Acadia National Park gathered from local weather stations, the National Oceanic and Atmospheric Administration (NOAA), and the National Oceanography Centre (NOC). ####

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
library(bslib)


#--------------------------#
####    Read-In Data    ####
#--------------------------#

temp.data.merged <- read.csv("data/processed_data/temp_data_merged.csv")

precip.data.merged <- read.csv("data/processed_data/precip_data_merged.csv")

anom.temp.merged <- read.csv("data/processed_data/anom_temp_merged.csv")

anom.precip.merged <- read.csv("data/processed_data/anom_precip_merged.csv")

records.noaa.daily <- read.csv("data/processed_data/records_noaa_daily.csv")

records.noaa.monthly <- read.csv("data/processed_data/records_noaa_monthly.csv")

frenchman.monthly.clean <- read.csv("data/processed_data/frenchman_monthly_clean.csv")

frenchman.annual.clean <- read.csv("data/processed_data/frenchman_annual_clean.csv")



#--------------------------#
####    Combine Data    ####
#--------------------------#

# as_tibble(temp.data.merged)
# as_tibble(precip.data.merged)
# as_tibble(records.noaa.monthly)
# as_tibble(records.noaa.daily)
# as_tibble(frenchman.annual.clean)
# 
# as_tibble(anom.temp.merged)
# as_tibble(anom.precip.merged)
# as_tibble(frenchman.monthly.clean)

annualdata <- left_join(temp.data.merged, precip.data.merged, by = "year") %>% 
  as_tibble() %>% 
  left_join(., records.noaa.monthly, by = "year") %>% 
  left_join(., records.noaa.daily, by = "year") %>% 
  left_join(., frenchman.annual.clean, by = "year") %>% 
  select(year, noaa.temp, noaa.max.temp, noaa.min.temp, mcfarland.temp, serc.temp,
         noaa.precip, mcfarland.precip, serc.precip, tmean.max.x:ppt.min.ym, 
         tmean.max.y:ppt.min.date, mean.sea.level.mm)

monthlydata <- left_join(anom.temp.merged, anom.precip.merged, by = c("year", "month")) %>% 
  as_tibble() %>% 
  left_join(., frenchman.monthly.clean, by = c("year", "month")) %>% 
  select(year, month, noaa.date = noaa.year.month.x, noaa.temp.anom:serc.temp.anom,
         noaa.precip.anom:serc.percent.precip.anom, mean.sea.level.mm)


#--------------------------#
####     Functions      ####
#--------------------------#

## function for generating data manipulation panel next to plots (e.i. check boxes, sliders, etc.)
create_temp_records_panel <- function(plots_config) {
  # plots_config should be a list of lists, each containing configuration for one plot
  lapply(plots_config, function(config) {
    # Get the data source for the year range
    data_source <- config$data_source
    
    fluidRow(
      # Column for the checkbox group input
      column(
        width = 4,
        box(
          title = "Data Filtering Tools",
          status = "primary",
          solidHeader = TRUE,
          width = 10,
          # Add checkbox group for line selection
          checkboxGroupInput(
            inputId = config$checkbox_id,
            label = "Select data to display:",
            choices = config$checkbox_choices,
            selected = config$default_selected
            ),
          sliderInput(
            inputId = config$year_range_id,
            label = "Select year range:",
            min = min(data_source$year),
            max = max(data_source$year),
            value = c(min(data_source$year), max(data_source$year)),
            sep = "",
            step = 20
          )
        )
      ),
      # Plot output
      column(
        width = 8,
        box(
          #title = config$plot_title,
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput(config$plot_id, height = "600px")
        )
      )
    )
  })
}
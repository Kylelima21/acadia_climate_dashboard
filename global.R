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
library(measurements)


#--------------------------#
####    Read-In Data    ####
#--------------------------#

temp.data.merged <- read.csv("data/processed_data/temp_data_merged.csv") %>% 
  mutate(noaa.temp = conv_unit(noaa.temp, "C", "F"),
         noaa.max.temp = conv_unit(noaa.max.temp, "C", "F"),
         noaa.min.temp = conv_unit(noaa.min.temp, "C", "F"),
         mcfarland.temp = conv_unit(mcfarland.temp, "C", "F"),
         serc.temp = conv_unit(serc.temp, "C", "F"))

precip.data.merged <- read.csv("data/processed_data/precip_data_merged.csv") %>% 
  mutate(year = round(year, digits = 0))
  # mutate(noaa.precip = conv_unit(noaa.precip, "mm", "inch"),
  #        mcfarland.precip = conv_unit(mcfarland.precip, "mm", "inch"),
  #        serc.precip = conv_unit(serc.precip, "mm", "inch"))

anom.temp.merged <- read.csv("data/processed_data/anom_temp_merged.csv") %>% 
  mutate(noaa.temp.anom = round(noaa.temp.anom, digits = 2),
         mcfarland.temp.anom = round(mcfarland.temp.anom, digits = 2),
         serc.temp.anom = round(serc.temp.anom, digits = 2)) %>%
  group_by(month) %>%
  mutate(
    noaa.rank = case_when(
      noaa.temp.anom <= 0 ~ rank(noaa.temp.anom[noaa.temp.anom <= 0], ties.method = "min")[match(noaa.temp.anom, noaa.temp.anom[noaa.temp.anom <= 0])],
      noaa.temp.anom > 0 ~ rank(-noaa.temp.anom[noaa.temp.anom > 0], ties.method = "min")[match(noaa.temp.anom, noaa.temp.anom[noaa.temp.anom > 0])],
      TRUE ~ NA_real_),
    mcfarland.rank = case_when(
      mcfarland.temp.anom <= 0 ~ rank(mcfarland.temp.anom[mcfarland.temp.anom <= 0], ties.method = "min")[match(mcfarland.temp.anom, mcfarland.temp.anom[mcfarland.temp.anom <= 0])],
      mcfarland.temp.anom > 0 ~ rank(-mcfarland.temp.anom[mcfarland.temp.anom > 0], ties.method = "min")[match(mcfarland.temp.anom, mcfarland.temp.anom[mcfarland.temp.anom > 0])],
      TRUE ~ NA_real_),
    serc.rank = case_when(
      serc.temp.anom <= 0 ~ rank(serc.temp.anom[serc.temp.anom <= 0], ties.method = "min")[match(serc.temp.anom, serc.temp.anom[serc.temp.anom <= 0])],
      serc.temp.anom > 0 ~ rank(-serc.temp.anom[serc.temp.anom > 0], ties.method = "min")[match(serc.temp.anom, serc.temp.anom[serc.temp.anom > 0])],
      TRUE ~ NA_real_)
  ) %>%
  ungroup()

anom.precip.merged <- read.csv("data/processed_data/anom_precip_merged.csv") %>% 
  mutate(noaa.precip.anom = round(conv_unit(noaa.precip.anom, "mm", "inch"), digits = 2),
         mcfarland.precip.anom = round(conv_unit(mcfarland.precip.anom, "mm", "inch"), digits = 2),
         serc.precip.anom = round(conv_unit(serc.precip.anom, "mm", "inch"), digits = 2)) %>% 
  group_by(month) %>%
  mutate(
    noaa.rank = case_when(
      noaa.precip.anom <= 0 ~ rank(noaa.precip.anom[noaa.precip.anom <= 0], ties.method = "min")[match(noaa.precip.anom, noaa.precip.anom[noaa.precip.anom <= 0])],
      noaa.precip.anom > 0 ~ rank(-noaa.precip.anom[noaa.precip.anom > 0], ties.method = "min")[match(noaa.precip.anom, noaa.precip.anom[noaa.precip.anom > 0])],
      TRUE ~ NA_real_),
    mcfarland.rank = case_when(
      mcfarland.precip.anom <= 0 ~ rank(mcfarland.precip.anom[mcfarland.precip.anom <= 0], ties.method = "min")[match(mcfarland.precip.anom, mcfarland.precip.anom[mcfarland.precip.anom <= 0])],
      mcfarland.precip.anom > 0 ~ rank(-mcfarland.precip.anom[mcfarland.precip.anom > 0], ties.method = "min")[match(mcfarland.precip.anom, mcfarland.precip.anom[mcfarland.precip.anom > 0])],
      TRUE ~ NA_real_),
    serc.rank = case_when(
      serc.precip.anom <= 0 ~ rank(serc.precip.anom[serc.precip.anom <= 0], ties.method = "min")[match(serc.precip.anom, serc.precip.anom[serc.precip.anom <= 0])],
      serc.precip.anom > 0 ~ rank(-serc.precip.anom[serc.precip.anom > 0], ties.method = "min")[match(serc.precip.anom, serc.precip.anom[serc.precip.anom > 0])],
      TRUE ~ NA_real_)
  ) %>%
  ungroup()

records.noaa.daily <- read.csv("data/processed_data/records_noaa_daily.csv") %>% 
  mutate(tmean.max = round(conv_unit(tmean.max, "C", "F"), digits = 2),
         tmax.max = round(conv_unit(tmax.max, "C", "F"), digits = 2),
         tmean.min = round(conv_unit(tmean.min, "C", "F"), digits = 2),
         tmin.min = round(conv_unit(tmin.min, "C", "F"), digits = 2))

records.noaa.monthly <- read.csv("data/processed_data/records_noaa_monthly.csv") %>% 
  mutate(tmean.max = round(conv_unit(tmean.max, "C", "F"), digits = 2),
         tmax.max = round(conv_unit(tmax.max, "C", "F"), digits = 2),
         tmean.min = round(conv_unit(tmean.min, "C", "F"), digits = 2),
         tmin.min = round(conv_unit(tmin.min, "C", "F"), digits = 2))

frenchman.monthly.clean <- read.csv("data/processed_data/frenchman_monthly_clean.csv") %>% 
  mutate(mean.sea.level.in = conv_unit(mean.sea.level.mm, "mm", "inch"),
         mean.sea.level.in = round(mean.sea.level.in, digits = 1))

frenchman.annual.clean <- read.csv("data/processed_data/frenchman_annual_clean.csv") %>% 
  mutate(mean.sea.level.in = conv_unit(mean.sea.level.mm, "mm", "inch"),
         mean.sea.level.in = round(mean.sea.level.in, digits = 1))



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
         tmean.max.y:ppt.min.date, mean.sea.level.in)

monthlydata <- left_join(anom.temp.merged, anom.precip.merged, by = c("year", "month")) %>% 
  as_tibble() %>% 
  left_join(., frenchman.monthly.clean, by = c("year", "month")) %>% 
  select(year, month, noaa.date = noaa.year.month.x, noaa.temp.anom:serc.temp.anom,
         noaa.precip.anom:serc.percent.precip.anom, mean.sea.level.in)


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
          width = 11,
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
            sep = ""
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
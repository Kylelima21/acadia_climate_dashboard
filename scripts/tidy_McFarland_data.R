## Tidy McFarland data  

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)

#-----------------------#
####    Read Data    ####
#-----------------------#

#Reading in CSV as a tibble

McFarland.data <- read.csv("data/McFarland_Hill_export_20241022.csv") %>%
  as_tibble()

#-----------------------#
####    Data Manip   ####
#-----------------------#

# Convert to Date using as.POSIXct and format it
clean.McFarland <- McFarland.data %>% 
  mutate(
    
    #change date column into standard format
    DATE_TIME = as.POSIXct(DATE_TIME, format = "%m/%d/%y %H:%M"),
    DATE_TIME = format(DATE_TIME, "%Y-%m-%d %H:%M"),
    
    #create new columns for year, month, and day
    year = year(DATE_TIME),
    month = month(DATE_TIME),
    day = day(DATE_TIME),
    
    #replace -999 values with NAs across the entire data set
    across(everything(), ~ case_when(is.numeric(.) & . == -999 ~ NA, TRUE ~ .))
  )






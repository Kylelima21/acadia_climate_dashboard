## Tidy sea level data

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

#-----------------------#
####    Read Data    ####
#-----------------------#

#Reading in file

frenchman.buoy.data <- read.csv("data/test_sea_data/525.rlrdata", 
                                sep = ";", 
                                header = FALSE, 
                                fill = TRUE, 
                                stringsAsFactors = FALSE, 
                                colClasses = c("character", "numeric", "numeric", "numeric"))

#-----------------------#
####    Data Manip   ####
#-----------------------#

# add column names
colnames(frenchman.buoy.data) <- c("year", "mean.sea.level.mm", "flag1", "flag2")

# 
frenchman.buoy.data.clean <- frenchman.buoy.data %>%
  mutate(
    
    # create year and month columns
    year = as.numeric(year),
    month = round(((year %% 1) * 12) + 0.5), # calculate month
    year = floor(year),                        # then extract year
    
    # replace -99999 with NAS
    mean.sea.level.mm = na_if(mean.sea.level.mm, -99999)
    
  )



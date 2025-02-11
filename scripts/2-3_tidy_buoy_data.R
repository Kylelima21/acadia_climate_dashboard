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

#Reading in files

frenchman.monthly.buoy.data <- read.csv("data/test_sea_data/525(monthly).rlrdata", 
                                sep = ";", 
                                header = FALSE, 
                                fill = TRUE, 
                                stringsAsFactors = FALSE, 
                                colClasses = c("character", "numeric", "numeric", "numeric"))

frenchman.annual.buoy.data <- read.csv("data/test_sea_data/525(annual).rlrdata", 
                                sep = ";", 
                                header = FALSE, 
                                fill = TRUE, 
                                stringsAsFactors = FALSE)

#-----------------------#
####    Data Manip   ####
#-----------------------#

# add column names
colnames(frenchman.monthly.buoy.data) <- c("year", "mean.sea.level.mm", "flag1", "flag2")

colnames(frenchman.annual.buoy.data) <- c("year", "mean.sea.level.mm", "flag1", "flag2")

# clean monthly data
frenchman.monthly.clean <- frenchman.monthly.buoy.data %>%
  mutate(
    
    # create year and month columns
    year = as.numeric(year),
    month = round(((year %% 1) * 12) + 0.5), # calculate month
    year = floor(year),                        # then extract year
    
    # create year.month column
    year.month = paste(year, sprintf("%02d", month), "01", sep = "-"),
    year.month = as.Date(year.month),
    
    # replace -99999 with NAS
    mean.sea.level.mm = na_if(mean.sea.level.mm, -99999),
    
    # convert sea level from mm to meters in new column
    mean.sea.level.m = (mean.sea.level.mm/1000)) %>% 
    
    # reorder columns
    select(year,
           month,
           year.month,
           mean.sea.level.mm,
           mean.sea.level.m,
           flag1,
           flag2)


# clean annual data
frenchman.annual.clean <- frenchman.annual.buoy.data %>%
  mutate(
    
    # replace -99999 with NAS
    mean.sea.level.mm = na_if(mean.sea.level.mm, -99999),
    
    # convert sea level from mm to meters in new column
    mean.sea.level.m = (mean.sea.level.mm/1000)) %>% 
  
  # reorder columns
  select(year,
         mean.sea.level.mm,
         mean.sea.level.m,
         flag1,
         flag2)

# Save outputs
# write.csv(frenchman.monthly.clean, "data/processed_data/frenchman_monthly_clean.csv", row.names = FALSE)
# write.csv(frenchman.annual.clean, "data/processed_data/frenchman_annual_clean.csv", row.names = FALSE)


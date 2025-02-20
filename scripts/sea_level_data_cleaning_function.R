## Sea level data cleaning function script ##

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

#--------------------------#
####    Read-In Data    ####
#--------------------------#

# monthly sea level data - Bar Harbor, Frenchman Bay, ME station
frenchman.monthly.data <- read.csv("data/raw_data/sea_level_data/525(monthly).rlrdata.txt", 
                                        sep = ";", 
                                        header = FALSE, 
                                        fill = TRUE, 
                                        stringsAsFactors = FALSE, 
                                        colClasses = c("character", "numeric", "numeric", "numeric"))

# annual sea level data - Bar Harbor, Frenchman Bay, ME station
frenchman.annual.data <- read.csv("data/raw_data/sea_level_data/525(annual).rlrdata.txt", 
                                sep = ";", 
                                header = FALSE, 
                                fill = TRUE, 
                                stringsAsFactors = FALSE)


#---------------------------------#
####   Data Cleaning Function  ####
#---------------------------------#

clean_sea_level_data <- function(data, data_type = c("monthly", "annual")) {
  
  # Match argument and error checking
  data_type <- match.arg(data_type)
  
  if (data_type == "monthly") {

# clean monthly data
clean_data <- data %>%
  
  # Rename columns
  rename(
    year = V1,
    mean.sea.level.mm = V2,
    flag1 = V3,
    flag2 = V4
  ) %>%
  
  mutate(
    
    # create year and month columns
    year = as.numeric(year),
    month = round(((year %% 1) * 12) + 0.5), # calculate month
    year = floor(year),                        # then extract year
    
    # create year.month column
    year.month = paste(year, sprintf("%02d", month), "01", sep = "-"),
    year.month = as.Date(year.month),
    
    # replace -99999 with NAs
    mean.sea.level.mm = na_if(mean.sea.level.mm, -99999),
    
    #add station metadata
    station.id = 525,
    station.name = "Bar Harbor, Frenchman Bay, ME",
    lat = 44.391667,
    long = -68.205000
  ) %>% 
    
    # reorder columns
    select(station.id,
           station.name,
           lat,
           long,
           year,
           month,
           year.month,
           mean.sea.level.mm,
           flag1,
           flag2)

  } else if (data_type == "annual") {


# clean annual data
clean_data <- data %>%
  
  # Rename columns
  rename(
    year = V1,
    mean.sea.level.mm = V2,
    flag1 = V3,
    flag2 = V4
  ) %>%
  
  mutate(
    
    # replace -99999 with NAS
    mean.sea.level.mm = na_if(mean.sea.level.mm, -99999),
    
    #add station metadata
    station.id = 525,
    station.name = "Bar Harbor, Frenchman Bay, ME",
    lat = 44.391667,
    long = -68.205000
  ) %>% 
  
  # reorder columns
  select(station.id,
         station.name,
         lat,
         long,
         year,
         mean.sea.level.mm,
         flag1,
         flag2)

  }
  
  return(clean_data)
}

# monthly sea level data
frenchman.monthly.clean <- clean_sea_level_data(frenchman.monthly.data, data_type = "monthly")

# annual sea level data
frenchman.annual.clean <- clean_sea_level_data(frenchman.annual.data, data_type = "annual")


# Save outputs
# write.csv(frenchman.monthly.clean, "data/processed_data/frenchman_monthly_clean.csv", row.names = FALSE)
# write.csv(frenchman.annual.clean, "data/processed_data/frenchman_annual_clean.csv", row.names = FALSE)







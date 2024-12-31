## Tidy SERC data  

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

serc.data <- read.csv("data/raw_data/SERC_D2258_export_20241119.csv") %>%
  as_tibble()

#-----------------------#
####    Data Manip   ####
#-----------------------#

# Convert to Date using as.POSIXct and format it
clean.serc <- serc.data  %>% 
  
  # Remove the first row with units
  slice(-1) %>%
  
  #transform dataset
  mutate(
    
    # parse the Date_Time column into POSIXct format in UTC
    Date_Time = as.POSIXct(Date_Time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    
    # convert Date_Time from UTC to EST
    date.time.est = with_tz(Date_Time, tzone = "America/New_York"),
    
    # extract new columns for year, month, day, and date
    year = year(date.time.est),
    month = month(date.time.est),
    day = day(date.time.est),
    date = as.Date(date.time.est), 
    
    #add missing columns 
    station.name = "Winter Harbor-SERC",
    lat = 44.33567,
    long = -68.06200
    
  ) %>% 
  
  # Remove unneeded columns and rename
  select(station.ID = Station_ID,
         station.name,
         lat,
         long,
         ppt = precip_accum_since_local_midnight_set_1,
         temp = air_temp_set_1,
         year,
         month,
         day,
         date)

  

##save outputs as csv
#write.csv(clean.McFarland, "data/McFarland_clean.csv", row.names = FALSE)

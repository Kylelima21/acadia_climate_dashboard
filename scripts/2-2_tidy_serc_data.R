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
serc.clean <- serc.data  %>% 
  
  # Remove the first row with units
  slice(-1) %>%
  
  #transform dataset
  mutate(
    
    # Convert non-character columns to numeric
    across(-c("Station_ID", "Date_Time", "wind_cardinal_direction_set_1d"), as.numeric),
    
    #replace empty cells with NA
    across(where(is.character), ~ na_if(., "")),
    
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
  select(station.id = Station_ID,
         station.name,
         lat,
         long,
         year,
         month,
         day,
         date,
         date.time.est,
         date.time.utc = Date_Time,
         ppt.midnight = precip_accum_since_local_midnight_set_1,
         ppt.24hr = precip_accum_24_hour_set_1,
         temp = air_temp_set_1,
         altimeter = altimeter_set_1,
         relative.humidity = relative_humidity_set_1,
         wind.speed = wind_speed_set_1,
         wind.direction = wind_direction_set_1,
         wind.gust = wind_gust_set_1,
         wind.chill = wind_chill_set_1d,
         wind.cardinal.direction = wind_cardinal_direction_set_1d,
         heat.index = heat_index_set_1d,
         dew.point.temp = dew_point_temperature_set_1d,
         pressure = pressure_set_1d,
         sea.level.pressure = sea_level_pressure_set_1d
         ) %>% 
  
  #remove empty columns
  select(where(~ !all(is.na(.))))


##save outputs as csv
#write.csv(clean.McFarland, "data/McFarland_clean.csv", row.names = FALSE)

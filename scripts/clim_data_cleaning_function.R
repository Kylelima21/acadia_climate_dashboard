## Climate data cleaning function script ##

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)

#--------------------------#
####    Read-In Data    ####
#--------------------------#

# Read-in CSV as a tibble

serc.data <- read.csv("data/raw_data/SERC_D2258_export_20241119.csv") %>%
  as_tibble()

mcfarland.data <- read.csv("data/raw_data/McFarland_Hill_export_20241022.csv") %>%
  as_tibble()

#-----------------------------------#
####    Data Cleaning Function   ####
#-----------------------------------#

clean_weather_data <- function(data, station_type = c("serc", "mcfarland")) {
  
  # Match argument and error checking
  station_type <- match.arg(station_type)
  
  if (station_type == "serc") {
    
    clean_data <- data %>%
      # Remove the first row with units
      slice(-1) %>%
      
      mutate(
        
        #replace empty cells with NA
        across(where(is.character), ~ na_if(na_if(., ""), "None")),
        
        # Convert non-character columns to numeric
        across(-c("Station_ID", "Date_Time", "wind_cardinal_direction_set_1d"), as.numeric),
        
        # parse the Date_Time column into POSIXct format in UTC
        Date_Time = as.POSIXct(Date_Time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        
        # convert Date_Time from UTC to EST
        date.time.est = with_tz(Date_Time, tzone = "America/New_York"),
        
        # extract new columns for year, month, day, and date
        year = year(date.time.est),
        month = month(date.time.est),
        day = day(date.time.est),
        date = date(date.time.est),
        
        #add station metadata
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
             sea.level.pressure = sea_level_pressure_set_1d) %>% 
    
    # remove any columns that are all NA
    select(where(~!all(is.na(.))))
    
  } else if (station_type == "mcfarland") {
    
    clean_data <- data %>%
      mutate(
        #change date columns into standard format
        DATE_TIME = as.POSIXct(DATE_TIME, format = "%m/%d/%y %H:%M"),
        UTC_DATE_TIME = as.POSIXct(UTC_DATE_TIME, format = "%m/%d/%y %H:%M", tz = "UTC"),
        
        #create new columns for year, month, and day
        year = year(DATE_TIME),
        month = month(DATE_TIME),
        day = day(DATE_TIME),
        date = date(DATE_TIME),
        
        #replace -999 values with NAs across the entire data set
        across(everything(), ~ case_when(is.numeric(.) & . == -999 ~ NA, TRUE ~ .)),
        
        #replace empty cells with NA
        across(where(is.character), ~ na_if(., "")),
        
        #combine temp columns into one column
        TMP_DEGC_combined = coalesce(TMP_DEGC, TMP_2_DEGC),
        
        #add station metadata
        station.name = "Acadia National Park McFarland Hill",
        lat = 44.3772,
        long = -68.2608
      ) %>%
      
      # Remove unneeded columns and rename
      select(station.id = ABBR,
             station.name,
             lat,
             long,
             year,
             month,
             day,
             date,
             date.time.est = DATE_TIME,
             date.time.utc = UTC_DATE_TIME,
             ppt = RNF_MM_HR,
             temp = TMP_DEGC_combined,
             relative.humidity = RH_PERCENT,
             scalar.wind.speed = SWS_M_S,
             vector.wind.speed = VWS_M_S,
             scalar.wind.direction = SWD_DEG,
             vector.wind.direction = VWD_DEG,
             solar.radiation = SOL_W_M2,
             o3.ppb = O3_PPB,
             so2.ppb = SO2_PPB,
             co.ppb = CO_PPM,
             no.ppb = NO_PPB,
             pm2.5b = PM2_5B_UG_M3_LC,
             pm2.5 = PM2_5_UG_M3_LC,
             pm2.5f = PM2_5F_2_UG_M3_LC) %>% 
      
      # remove any columns that are all NA
      select(where(~!all(is.na(.))))
  }
  
  return(clean_data)
}

# SERC data
serc.clean <- clean_weather_data(serc.data, station_type = "serc")

# McFarland data
mcfarland.clean <- clean_weather_data(mcfarland.data, station_type = "mcfarland")

##save outputs as csv
# write.csv(serc.clean, "data/processed_data/serc_clean.csv", row.names = FALSE)
# write.csv(mcfarland.clean, "data/processed_data/mcfarland_clean.csv", row.names = FALSE)


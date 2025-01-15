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


#testing further tiding - isolating the last precip records per day

process_precipitation_data <- function(data, datetime_col, precip_col) { 
  result <- data %>%
    # Ensure datetime is in correct format
    mutate(
      datetime = as.POSIXct(!!sym(datetime_col)),
      date = lubridate::date(datetime),  
      # Extract hour and minute for sorting
      hour = as.numeric(format(datetime, "%H")),
      minute = as.numeric(format(datetime, "%M"))
    ) %>%
    # Group by date
    group_by(date) %>%
    # Arrange by hour and minute in descending order to get latest time first
    arrange(desc(hour), desc(minute), .by_group = TRUE) %>%
    # Take first record (which will be the latest time) of each day
    slice_head(n = 1) %>%
    # Select only needed columns
    select(
      year,
      date,
      datetime,
      precipitation = !!sym(precip_col)
    ) %>%
    ungroup()
  
  # Print some diagnostic information
  print("Sample of processed data:")
  print(head(result %>% 
               mutate(time = format(datetime, "%H:%M:%S")) %>%
               select(year, date, time, precipitation), 
             n = 10))
  
  return(result)
}

filtered_data <- process_precipitation_data(
  data = serc.clean,
  datetime_col = "date.time.est",
  precip_col = "ppt.midnight"
)

filtered_data_2 <- process_precipitation_data(
  data = serc.clean,
  datetime_col = "date.time.est",
  precip_col = "ppt.24hr"
)

#isolate midnight precip data and get the yearly mean precip from SERC data
serc.precip.midnight <- filtered_data %>%
  drop_na(precipitation, year) %>%
  mutate(precipitation = precipitation * 0.0393701) %>% #convert mm of rain into inches of rain
  group_by(year) %>%
  summarize(serc.precip = sum(precipitation, na.rm = TRUE))

#isolate 24hr precip data and get the yearly mean precip from SERC data
serc.precip.24hr <- filtered_data_2 %>%
  drop_na(precipitation, year) %>%
  mutate(precipitation = precipitation * 0.0393701) %>% #convert mm of rain into inches of rain
  group_by(year) %>%
  summarize(serc.precip = sum(precipitation, na.rm = TRUE))

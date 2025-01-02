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

mcfarland.data <- read.csv("data/raw_data/McFarland_Hill_export_20241022.csv") %>%
  as_tibble()

#-----------------------#
####    Data Manip   ####
#-----------------------#

# Convert to Date using as.POSIXct and format it
mcfarland.clean <- mcfarland.data %>% 
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
    
    #add missing columns 
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
         pm2.5f = PM2_5F_2_UG_M3_LC
         
  ) %>% 
  
  #remove empty columns
  select(where(~ !all(is.na(.))))


##save outputs as csv
# write.csv(clean.McFarland, "data/McFarland_clean.csv", row.names = FALSE)




# #Checking data, distributions, outliers, etc. --------------------------------
# 
# #rain
# clean.McFarland %>% 
#   ggplot(aes(x=date, y=RNF_MM_HR)) +
#   geom_line()
# 
# clean.McFarland %>% 
#   select(date, RNF_MM_HR, TMP_DEGC_combined) %>% 
#   slice_max(n=5, RNF_MM_HR)
# 
# clean.McFarland %>% 
#   ggplot(aes(x=RNF_MM_HR)) +
#   geom_histogram() +
#   scale_y_continuous(limits = c(0, 50))
# 
# #temp
# clean.McFarland %>% 
#   ggplot(aes(x=date, y=TMP_DEGC_combined)) +
#   geom_line()
# 
# clean.McFarland %>% 
#   ggplot(aes(x=TMP_DEGC_combined)) +
#   geom_histogram(binwidth = 1)
# 
# clean.McFarland %>% 
#   select(date, RNF_MM_HR, TMP_DEGC_combined) %>% 
#   slice_max(n=5, TMP_DEGC_combined)
# 
# 
# #Removing outliers
# clean.McFarland %>% 
#   #removes entire day
#   #filter(RNF_MM_HR < 200) %>% 
#   #only changing the day for the days that are problematic
#   mutate(RNF_MM_HR = if_else(RNF_MM_HR < 200, RNF_MM_HR, NA_real_),
#          #categorical outliers
#          date = na_if(date, "1998-02-01"),
#          date = if_else(date == "1998-02-01", as.Date(NA_character_), date)) %>% 
#   drop_na()



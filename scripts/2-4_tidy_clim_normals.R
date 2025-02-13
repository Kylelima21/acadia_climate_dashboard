## Normals cleaning script ##

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

monthly.baseline.normals <- read.csv("data/raw_data/NOAA_monthly_normals_1981_2010.csv") %>%
  as_tibble()

#-----------------------#
####    Data Manip   ####
#-----------------------#

normals.clean <- monthly.baseline.normals %>%
  
  select(station.id = STATION,
         month = DATE,
         tmean = MLY.TAVG.NORMAL,
         tmax = MLY.TMAX.NORMAL,
         tmin = MLY.TMIN.NORMAL,
         ppt = MLY.PRCP.NORMAL,
         snow = MLY.SNOW.NORMAL) %>% 
         
  
  mutate(
    
    # replace -7777 with NAS
    snow = na_if(snow, -7777),
    
    # add year range column
    year.range = "1981-2010",
    
    # add decimals to values in each column
    tmean = as.numeric(paste0(substr(tmean, 1, 2), ".", substr(tmean, 3, nchar(tmean)))),
    tmax = as.numeric(paste0(substr(tmax, 1, 2), ".", substr(tmax, 3, nchar(tmax)))),
    tmin = as.numeric(paste0(substr(tmin, 1, 2), ".", substr(tmin, 3, nchar(tmin)))),
    ppt = as.numeric(paste0(substr(ppt, 1, 1), ".", substr(ppt, 2, nchar(ppt)))),
    snow = ifelse(is.na(snow), NA, as.numeric(paste0(substr(snow, 1, nchar(snow) - 1), ".", substr(snow, nchar(snow), nchar(snow))))),
    
    # convert temperature to Celsius
    tmean = (tmean - 32) * 5 / 9,
    tmax = (tmax - 32) * 5 / 9,
    tmin = (tmin - 32) * 5 / 9
    
    
    )



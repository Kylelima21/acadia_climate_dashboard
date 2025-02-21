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
         tmean.normal = MLY.TAVG.NORMAL,
         tmax.normal = MLY.TMAX.NORMAL,
         tmin.normal = MLY.TMIN.NORMAL,
         ppt.normal = MLY.PRCP.NORMAL,
         snow.normal = MLY.SNOW.NORMAL) %>% 
         
  
  mutate(
    
    # replace -7777 with NAS
    snow.normal = na_if(snow.normal, -7777),
    
    # add year range column
    year.range = "1981-2010",
    
    # add decimals to values in each column
    tmean.normal = as.numeric(paste0(substr(tmean.normal, 1, 2), ".", substr(tmean.normal, 3, nchar(tmean.normal)))),
    tmax.normal = as.numeric(paste0(substr(tmax.normal, 1, 2), ".", substr(tmax.normal, 3, nchar(tmax.normal)))),
    tmin.normal = as.numeric(paste0(substr(tmin.normal, 1, 2), ".", substr(tmin.normal, 3, nchar(tmin.normal)))),
    ppt.normal = as.numeric(paste0(substr(ppt.normal, 1, 1), ".", substr(ppt.normal, 2, nchar(ppt.normal)))),
    snow.normal = ifelse(is.na(snow.normal), NA, as.numeric(paste0(substr(snow.normal, 1, nchar(snow.normal) - 1), ".", substr(snow.normal, nchar(snow.normal), nchar(snow.normal))))),
    
    # convert temperature to Celsius
    tmean.normal = (tmean.normal - 32) * 5 / 9,
    tmax.normal = (tmax.normal - 32) * 5 / 9,
    tmin.normal = (tmin.normal - 32) * 5 / 9
    
    
    )

##save outputs as csv
write.csv(normals.clean, "data/processed_data/noaa_normals_clean.csv", row.names = FALSE)


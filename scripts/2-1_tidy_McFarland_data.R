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

McFarland.data <- read.csv("data/raw_data/McFarland_Hill_export_20241022.csv") %>%
  as_tibble()

#-----------------------#
####    Data Manip   ####
#-----------------------#

# Convert to Date using as.POSIXct and format it
clean.McFarland <- McFarland.data %>% 
  mutate(
    
    #change date column into standard format
    DATE_TIME = as.POSIXct(DATE_TIME, format = "%m/%d/%y %H:%M"),
    DATE_TIME = format(DATE_TIME, "%Y-%m-%d %H:%M"),
    
    #create new columns for year, month, and day
    year = year(DATE_TIME),
    month = month(DATE_TIME),
    day = day(DATE_TIME),
    date = date(DATE_TIME),
    
    #replace -999 values with NAs across the entire data set
    across(everything(), ~ case_when(is.numeric(.) & . == -999 ~ NA, TRUE ~ .)),
    
    #combine temp columns into one column
    TMP_DEGC_combined = coalesce(TMP_DEGC, TMP_2_DEGC)
  )


##save outputs as csv
write.csv(clean.McFarland, "data/McFarland_clean.csv", row.names = FALSE)




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



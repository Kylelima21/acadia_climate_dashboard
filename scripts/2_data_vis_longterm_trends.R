## Create data visualizations from NOAA climate data
## Data vis 1: long-term temperature and precipitation trends  

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(tidyverse)
library(dplyr)
library(ggplot2)

#-----------------------#
####    Read Data    ####
#-----------------------#

#Reading in CSVs as a tibble

#NOAA daily 
daily.noaa.data <- read.csv("data/processed_data/nClimGrid_daily_clean.csv") %>%
  as_tibble()

#NOAA monthly
monthly.noaa.data <- read.csv("data/processed_data/nClimGrid_monthly_clean.csv") %>%
  as_tibble()

#McFarland
clean.McFarland <- read.csv("data/processed_data/McFarland_clean.csv")


#-----------------------#
####    Data Manip   ####
#-----------------------#

#### Temperature trends overtime -----------------------------

## Graph long-term temperature trends using monthly NOAA climate data

# Find the average mean, max, and min temp for each year 
yearly_data <- monthly.noaa.data %>%
  group_by(year) %>% 
  summarize(YearlyAvgTemp = mean(tmean, na.rm = TRUE),
            YearlyAvgMax = mean(tmax, na.rm = TRUE),
            YearlyAvgMin = mean(tmin, na.rm = TRUE))

# Reshape data to long format for easier plotting
yearly_data_long <- yearly_data %>%
  pivot_longer(cols = c(YearlyAvgTemp, YearlyAvgMax, YearlyAvgMin),
               names_to = "temp.type",
               values_to = "temp")

#isolate temperature data and get the yearly mean temperature from McFarland data
temp.McFarland <- clean.McFarland %>%
  drop_na(TMP_DEGC_combined, year) %>%
  group_by(year) %>%
  summarize(temp = mean(TMP_DEGC_combined))


#add column for data source
#NOAA
  yearly_data_long_source <- yearly_data_long %>% 
    mutate(source = "NOAA")
#McFarland
  temp.McFarland.source <- temp.McFarland %>%
    mutate(source = "McFarland",
           temp.type = "McFarlandYearlyAvgTemp")

#merge data sets together
merged.temp.noaa.McFarland <- bind_rows(yearly_data_long_source, temp.McFarland.source)

##save outputs as csv
#write.csv(merged.temp.noaa.McFarland, "data/processed_data/merged_temp_noaa_McFarland.csv", row.names = FALSE)

# Plot all yearly temp data on one graph  
ggplot(merged.temp.noaa.McFarland, aes(x = year, y = temp, color = temp.type)) +
  geom_point(size = 1) +
  geom_line(size = 1, alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_continuous(breaks = pretty(merged.temp.noaa.McFarland$year)) +
  scale_color_manual(
    values = c("YearlyAvgMax" = "#CC3300", "YearlyAvgMin" = "#003399", "YearlyAvgTemp" = "#000000", "McFarlandYearlyAvgTemp" = "#00CC00"),
    labels = c("YearlyAvgMax" = "Average Maximum Temp.", "YearlyAvgTemp" = "Average Temp.", "YearlyAvgMin" = "Average Minimum Temp.", "McFarlandYearlyAvgTemp" = "McFarland Average Temp.")) +
  labs(title = "Average Temperature (1895-2024)",
       x = "Year",
       y = "Temperature (°C)",
       color = "Temperature Type") +
  theme_minimal()

#manipulate data and create merged temp data set for R shiny dashboard graph
# create filter for current year - first get current year
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# create shiny data set
shiny.merged.temp <- yearly_data %>% 
  left_join(temp.McFarland %>% select(year, temp), by = "year") %>% 
  rename(max.noaa = YearlyAvgMax,
         min.noaa = YearlyAvgMin,
         temp.noaa = YearlyAvgTemp,
         mcfarland = temp) %>% 
  filter(year < current_year) %>% 
  mutate(
    mcfarland = if_else(year == 1998, NA_real_, mcfarland))

##save outputs as csv
#write.csv(shiny.merged.temp, "data/processed_data/shiny_merged_temp.csv", row.names = FALSE)

### significance tests - linear regression model
#noaa max
LR.max <- lm(max.noaa ~ year, data = shiny.merged.temp)
summary(LR.max)

#noaa min
LR.min <- lm(min.noaa ~ year, data = shiny.merged.temp)
summary(LR.min)

#noaa mean
LR.temp <- lm(temp.noaa ~ year, data = shiny.merged.temp)
summary(LR.temp)

#mcfarland mean
LR.mcfarland <- lm(mcfarland ~ year, data = shiny.merged.temp)
summary(LR.mcfarland)

#### Precipitation trends overtime -----------------------------

#NOAA yearly precipitation trends
precip.noaa <- monthly.noaa.data %>%
  mutate(ppt_IN_HR = ppt * 0.0393701) %>% #convert mm of rain into inches of rain
  group_by(year) %>% 
  summarize(noaa.precip = sum(ppt_IN_HR, na.rm = TRUE))

#isolate precip data and get the yearly mean precip from McFarland data
precip.McFarland <- clean.McFarland %>% 
  drop_na(RNF_MM_HR, year) %>%
  mutate(RNF_IN_HR = RNF_MM_HR * 0.0393701) %>% #convert mm of rain into inches of rain
  group_by(year) %>% 
  summarize(McFarland.precip = sum(RNF_IN_HR, na.rm = TRUE))


#BASIC - plot yearly precip data 
ggplot(precip.noaa, aes(x = year, y = noaa.precip)) +
  geom_line(linewidth = 1, alpha = 0.5) +
  geom_point(size = 1) +
  #geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = pretty(precip.noaa$year)) +
  labs(title = "Total Precipitation (1895-2024)",
       x = "Year",
       y = "Precipitation (in)") +
  theme_minimal()

#create merged data set for R shiny dashboard graph
# create filter for current year - first get current year
current_year <- as.numeric(format(Sys.Date(), "%Y"))

shiny.merged.precip <- precip.noaa %>% 
  left_join(precip.McFarland %>% select(year, McFarland.precip), by = "year")%>% 
  filter(year < current_year)

##save outputs as csv
#write.csv(shiny.merged.precip, "data/processed_data/shiny_merged_precip.csv", row.names = FALSE)

### significance tests - linear regression model
#noaa max
LR.noaa.precip <- lm(noaa.precip ~ year, data = shiny.merged.precip)
summary(LR.noaa.precip)

#noaa min
LR.mcfarland.precip <- lm(McFarland.precip ~ year, data = shiny.merged.precip)
summary(LR.mcfarland.precip)


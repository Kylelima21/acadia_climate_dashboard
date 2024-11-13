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

#daily 
daily.noaa.data <- read.csv("data/nClimGrid_daily_clean.csv") %>%
  as_tibble()

#monthly
monthly.noaa.data <- read.csv("data/nClimGrid_monthly_clean.csv") %>%
  as_tibble()

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
               names_to = "TemperatureType",
               values_to = "Temperature")

# Plot all yearly temp data on one graph  
ggplot(yearly_data_long, aes(x = year, y = Temperature, color = TemperatureType)) +
  geom_point(size = 1) +
  geom_line(size = 1, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = pretty(yearly_data_long$year)) +
  scale_color_manual(
    values = c("YearlyAvgMax" = "#CC3300", "YearlyAvgMin" = "#003399", "YearlyAvgTemp" = "#000000"),
    labels = c("YearlyAvgMax" = "Average Maximum Temp.", "YearlyAvgTemp" = "Average Temp.", "YearlyAvgMin" = "Average Minimum Temp.")) +
  labs(title = "Average Temperature (1895-2024)",
       x = "Year",
       y = "Temperature (°C)",
       color = "Temperature Type") +
  theme_minimal()

#### Precipitation trends overtime -----------------------------

#yearly precipitation trends
precip_yearly <- monthly.noaa.data %>%
  group_by(year) %>% 
  summarize(YearlyAvgPrecip = mean(ppt, na.rm = TRUE))

#BASIC - plot yearly precip data 
ggplot(precip_yearly, aes(x = year, y = YearlyAvgPrecip)) +
  geom_line(linewidth = 1, alpha = 0.5) +
  geom_point(size = 1) +
  #geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = pretty(precip_yearly$year)) +
  labs(title = "Average Precipitation (1895-2024)",
       x = "Year",
       y = "Precipitation (in)") +
  theme_minimal()

#monthly precipitation trends
precip_monthly <- monthly.noaa.data %>%
  group_by(year, month) %>% 
  summarize(MonthlyAvgPrecip = mean(ppt, na.rm = TRUE)) %>%
  mutate(year_month = as.Date(paste(year, sprintf("%02d", month), "01", sep = "-")))

#BASIC - plot monthly precip data 
ggplot(precip_monthly, aes(x = year_month, y = MonthlyAvgPrecip)) +
  geom_line(size = 0.5, alpha = 0.5) +
  geom_point(size = 1) +
  #geom_smooth(method = "lm", se = FALSE) +
  scale_x_date(breaks = "5 years", labels = scales::date_format("%Y")) +
  labs(title = "Average Precipitation (1895-2024)",
       x = "Year",
       y = "Precipitation (in)") +
  theme_minimal()



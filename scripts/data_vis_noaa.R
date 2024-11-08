## Create data visualizations from NOAA climate data


#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(tidyverse)
library(dplyr)
library(ggplot2)

#-----------------------#
####    Read Data    ####
#-----------------------#

#Reading in CSV as a tibble
noaa.data <- read.csv("data/nClimGrid_daily_clean.csv") %>%
  as_tibble()

#-----------------------#
####    Data Manip   ####
#-----------------------#

#average.year.temp <- noaa.data %>%
 # group_by(year) %>%
 # summarize(AvgTemp = mean(tmean))

#### perform linear regression for temperature over time----------------
# source: https://rpubs.com/zmalesker2/1139127

LR <- lm(tmean ~ year, data = noaa.data)

summary(LR)

#plotting the residuals

ResidualsPlot <- 
  ggplot(noaa.data, aes(x = year, y = residuals(LR))) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(title = "Residuals Plot",
       x = "Year",
       y = "Residuals")


print(ResidualsPlot)


#### Temperature trends over time ------------------------------

#yearly temperature trends
yearly_data <- noaa.data %>%
  group_by(year) %>% 
  summarize(YearlyAvgTemp = mean(tmean, na.rm = TRUE),
            YearlyAvgMax = mean(tmax, na.rm = TRUE),
            YearlyAvgMin = mean(tmin, na.rm = TRUE))

# Reshape data to long format for easier plotting
yearly_data_long <- yearly_data %>%
  pivot_longer(cols = c(YearlyAvgTemp, YearlyAvgMax, YearlyAvgMin),
               names_to = "TemperatureType",
               values_to = "Temperature")

#plot yearly temp data - average, max, and min
ggplot(yearly_data_long, aes(x = year, y = Temperature, color = TemperatureType)) +
  geom_line(size = 1) +
  labs(title = "Yearly Temperature Trends",
       x = "Year",
       y = "Temperature (°C)",
       color = "Temperature Type") +
  theme_minimal()


#plot yearly temperature trends
#ggplot(yearly_data, aes(x = as.numeric(year), y = YearlyAvgTemp)) +
 # geom_line(color = "blue") +
 # labs(title = "Average Yearly Temperature",
  #     x = "Year",
  #     y = "Average Temperature (°F)") +
#  theme_minimal()


#monthly temperature trends
monthly_data <- noaa.data %>%
  group_by(year, month) %>%
  summarize(MonthlyAvgTemp = mean(tmean, na.rm = TRUE)) 

#monthly_data <- noaa.data %>%
 # mutate(YearMonth = format(date, "%Y-%m" )) %>% 
#  group_by(YearMonth) %>%
#  summarize(MonthlyAvgTemp = mean(tmean, na.rm = TRUE)) %>% 
  
#plot monthly temperature trends
ggplot(monthly_data, aes(x = as.numeric(year), y = MonthlyAvgTemp)) +
  geom_line(color = "blue") +
  labs(title = "Average Monthly Temperature",
       x = "Date",
       y = "Average Temperature (°C)") +
  theme_minimal()



# Temperature anomalies --------------------------------------------

# Check and convert the Date column to Date format
noaa.data$date <- as.Date(noaa.data$date)

# extract day of year
new.noaa.data <- noaa.data %>%
 mutate(DayOfYear = format(date, "%j"))

# baseline as the average temperature per day across years
daily.baseline <- new.noaa.data %>%
  group_by(DayOfYear) %>%
  summarize(BaselineTemp = mean(tmean, na.rm = TRUE))

# join the baseline with the original data
new.noaa.data <- new.noaa.data %>%
  left_join(daily.baseline, by = "DayOfYear")

# calculate the daily temperature anomaly
new.noaa.data  <- new.noaa.data %>%
  mutate(TempAnomaly = tmean - BaselineTemp)

# visualize the anomalies over time

ggplot(new.noaa.data , aes(x = year, y = TempAnomaly)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0, linetype = "solid", color = "red") +
  labs(title = "Daily Temperature Anomalies Over Time",
       x = "Year", y = "Temperature Anomaly (°C)") +
  theme_minimal()



# Calculate a baseline average temperature

# baseline as the average temperature per month across years
monthly_data <- noaa.data %>%
  group_by(year, month) %>%
  summarize(MonthlyAvgTemp = mean(tmean, na.rm = TRUE))

# Calculate baseline monthly average temperature
monthly_baseline <- monthly_data %>%
  group_by(month) %>%
  summarize(BaselineTemp = mean(MonthlyAvgTemp, na.rm = TRUE))

# Join baseline with original data
monthly_data <- monthly_data %>%
  left_join(monthly_baseline, by = "month")

# Calculate the monthly temperature anomaly
monthly_data <- monthly_data %>%
  mutate(TempAnomaly = MonthlyAvgTemp - BaselineTemp)

# visualize the anomalies over time using ggplot2
ggplot(new.monthly.data, aes(x = year, y = TempAnomaly)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0, linetype = "solid", color = "red") +
  labs(title = "Monthly Temperature Anomalies Over Time",
       x = "year", y = "Temperature Anomaly (°C)") +
  theme_minimal()


#attempting this - https://www.youtube.com/watch?v=DrNQMaIVEVo 

new.monthly.data <- monthly_data %>% 
  mutate(month = as.numeric(month),
         year = as.numeric(year),
         month = month.abb[month],
         month = factor(month, levels = month.abb))



new.monthly.data %>% 
  ggplot(aes(x = month, y = TempAnomaly, group = year)) + 
  geom_line()

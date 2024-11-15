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

#average.year.temp <- noaa.data %>%
 # group_by(year) %>%
 # summarize(AvgTemp = mean(tmean))

#### perform linear regression for temperature over time----------------
# source: https://rpubs.com/zmalesker2/1139127

LR <- lm(tmean ~ year, data = daily.noaa.data)

summary(LR)

#plotting the residuals

ResidualsPlot <- 
  ggplot(daily.noaa.data, aes(x = year, y = residuals(LR))) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(title = "Residuals Plot",
       x = "Year",
       y = "Residuals")


print(ResidualsPlot)


#### Temperature trends over time ------------------------------

##DAILY DATA##

#yearly temperature trends
#yearly_data <- daily.noaa.data %>%
 # group_by(year) %>% 
 # summarize(YearlyAvgTemp = mean(tmean, na.rm = TRUE),
  #          YearlyAvgMax = mean(tmax, na.rm = TRUE),
   #         YearlyAvgMin = mean(tmin, na.rm = TRUE))

# Reshape data to long format for easier plotting
#yearly_data_long <- yearly_data %>%
 # pivot_longer(cols = c(YearlyAvgTemp, YearlyAvgMax, YearlyAvgMin),
  #             names_to = "TemperatureType",
   #            values_to = "Temperature")

#plot yearly temp data - average, max, and min
#ggplot(yearly_data_long, aes(x = year, y = Temperature, color = TemperatureType)) +
 # geom_line(size = 1) +
  # labs(title = "Yearly Temperature Trends",
    #   x = "Year",
     #  y = "Temperature (°C)",
     #  color = "Temperature Type") +
  # theme_minimal()


#plot yearly temperature trends - single variable
#ggplot(yearly_data, aes(x = as.numeric(year), y = YearlyAvgTemp)) +
 # geom_line(color = "blue") +
 # labs(title = "Average Yearly Temperature",
  #     x = "Year",
  #     y = "Average Temperature (°F)") +
#  theme_minimal()

#monthly temperature trends from daily data
# monthly_data <- noaa.data %>%
  #group_by(year, month) %>%
  #summarize(MonthlyAvgTemp = mean(tmean, na.rm = TRUE)) 

#monthly_data <- noaa.data %>%
 # mutate(YearMonth = format(date, "%Y-%m" )) %>% 
#  group_by(YearMonth) %>%
#  summarize(MonthlyAvgTemp = mean(tmean, na.rm = TRUE)) %>% 
  
#plot monthly temperature trends
#ggplot(monthly_data, aes(x = as.numeric(year), y = MonthlyAvgTemp)) +
#  geom_line(color = "blue") +
 # labs(title = "Average Monthly Temperature",
  #     x = "Date",
   #    y = "Average Temperature (°C)") +
  # theme_minimal()

##MONTHLY DATA##

#yearly temperature trends
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

#BASIC - plot yearly temp data - average, max, and min
ggplot(yearly_data_long, aes(x = year, y = Temperature, color = TemperatureType)) +
  geom_line(size = 1) +
  labs(title = "Average Temperature (1895-2024)",
       x = "Year",
       y = "Temperature (°C)",
       color = "Temperature Type") +
  theme_minimal()

#FRANCY - plot yearly temp data - average, max, and min
ggplot(yearly_data_long, aes(x = year, y = Temperature, color = TemperatureType)) +
  geom_point(size = 1) +
  geom_line(size = 1, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = pretty(yearly_data_long$year)) +
  scale_color_manual(values = c("YearlyAvgMax" = "#CC3300", "YearlyAvgMin" = "#003399", "YearlyAvgTemp" = "#000000")) +
  labs(title = "Average Temperature (1895-2024)",
       x = "Year",
       y = "Temperature (°C)",
       color = "Temperature Type") +
  theme_minimal()

#monthly temperature trends
# monthly_data <- monthly.noaa.data %>%
# group_by(year, month) %>%
# summarize(MonthlyAvgTemp = mean(tmean, na.rm = TRUE),
#         MonthlyAvgMax = mean(tmax, na.rm = TRUE),
#        MonthlyAvgMin = mean(tmin, na.rm = TRUE))
# 
# # Reshape data to long format for easier plotting
# monthly_data_long <- monthly_data %>%
#   pivot_longer(cols = c(MonthlyAvgTemp, MonthlyAvgMax, MonthlyAvgMin),
#                names_to = "TemperatureType",
#                values_to = "Temperature") %>%
#   mutate(year_month = paste(year, sprintf("%02d", month), "01", sep = "-"),
#         year_month = as.Date(year_month))
# 
# 
# #FRANCY - plot monthly temp data - average, max, and min
# ggplot(monthly_data_long, aes(x = year_month, y = Temperature, color = TemperatureType)) +
#   geom_point(size = 1) +
#   geom_line(size = 1, alpha = 0.5) +
#   geom_smooth(method = "lm", se = FALSE) +
#   scale_x_continuous(breaks = pretty(monthly_data_long$year)) +
#   scale_color_manual(values = c("MonthlyAvgMax" = "#CC3300",
#                                 "MonthlyAvgMin" = "#003399",
#                                 "MonthlyAvgTemp" = "#000000")) +
#  scale_x_date(breaks = "10 years", labels = scales::date_format("%Y")) +
# labs(title = "Average Temperature (1895-2024)",
#     x = "Year-month",
#    y = "Temperature (°C)",
#   color = "Temperature Type") +
#  theme_minimal()


# Temperature anomalies --------------------------------------------
#source: https://rpubs.com/zmalesker2/1139127 
  
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


# Calculate temp anomaly from monthly data

# baseline as the average temperature per month across years
monthly.climatology <- monthly.noaa.data %>%
  group_by(month) %>%
  summarize(climatology = mean(tmean, na.rm = TRUE))

# Join baseline with original data
temp.with.climatology <- monthly.noaa.data %>%
  left_join(monthly.climatology, by = "month")

# Calculate the monthly temperature anomaly
temp.with.anomalies <- temp.with.climatology %>%
  mutate(TempAnomaly = tmean - climatology) %>%
  mutate(year_month = paste(year, sprintf("%02d", month), "01", sep = "-"),
         year_month = as.Date(year_month))

# visualize the anomalies over time using ggplot2
ggplot(temp.with.anomalies, aes(x = year_month, y = TempAnomaly, fill = TempAnomaly > 0)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  #annotate("text", x = min(temp.with.anomalies$year), y = 0.1, label = "Average baseline", hjust = 0, color = "black") +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue"),
                    labels = c("TRUE" = "Below baseline", "FALSE" = "Above Baseline")) +
  scale_x_continuous(
    breaks = seq(1895, 2024, by = 5),
    limits = c(1895, 2024))   +
  scale_x_date(breaks = "10 years", labels = scales::date_format("%Y")) +
  labs(title = "Monthly Temperature Anomalies Over Time",
       x = "Year", 
       y = "Temperature Anomaly (°C)") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


###attempting this - https://www.youtube.com/watch?v=DrNQMaIVEVo #graphing temperature anomalies 

  #manipulate data to get month abbreviations and make month column factor class
  new.monthly.data <- temp.with.anomalies %>% 
    mutate(month = as.numeric(month),
         year = as.numeric(year),
         month = month.abb[month],
         month = factor(month, levels = month.abb),
         month_anom = tmean + TempAnomaly) %>% 
    group_by(year) %>% 
    mutate(ave = mean(month_anom)) %>% 
    ungroup()
  
  #graph temp anomalies 
  new.monthly.data %>% 
    ggplot(aes(x = month, y = month_anom, group = year, color = ave)) + 
      geom_line() +
    scale_color_gradient2(low = "darkblue", mid = "white", high = "darkred", midpoint = 0, guide = "none")
    
  

#### Temperature and precipitation  records -------------------------

#day with highest temp in historical record
record.temp <- daily.noaa.data %>% 
    select(tmax, ppt, date) %>%
    filter(tmax == max(tmax, na.rm = TRUE))

#day with highest precip in historical record
record.precip <- daily.noaa.data %>% 
    select(tmax, ppt, date) %>%
    filter(ppt == max(ppt, na.rm = TRUE))

#number of record highs per year - days above a certain threshold (above the average/baseline??)
#average daily max temp across all years and then for each year, calculate the number of days above that average

#mean highest max temp
temp.max.mean <- daily.noaa.data %>% 
  summarise(mean.max = mean(tmax, na.rm = TRUE))

#top 20 highest max temps
top20_highest_temps <- daily.noaa.data %>% 
  arrange(desc(tmax)) %>%    
  slice_head(n = 20)

#graph of max temps
ggplot(top20_highest_temps, aes(x = date, y = tmax)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(
       x = "Date",
       y = "Record High Temperatures") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#summarizing the number of days each year with temp maximums above the average temp maximum
temp.highs <- daily.noaa.data %>% 
  group_by(year) %>% 
  summarise(days.above.mean.max = sum(tmax > 12.10306, na.rm = TRUE))
  
# Create a bar graph using ggplot2
ggplot(temp.highs, aes(x = factor(year), y = days.above.mean.max)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Number of Days Above Average Max Each Year",
       x = "Year",
       y = "Days Above Average Max") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### Precipitation trends over time ---------------------------------

#yearly precipitation trends
precip_yearly <- monthly.noaa.data %>%
  group_by(year) %>% 
  summarize(YearlyAvgPrecip = mean(ppt, na.rm = TRUE))

#BASIC - plot yearly precip data 
ggplot(precip_yearly, aes(x = year, y = YearlyAvgPrecip)) +
  geom_line(size = 1, alpha = 0.5) +
  geom_point(size = 1) +
  #geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = pretty(precip_yearly$year)) +
  labs(title = "Average Precipitation (1895-2024)",
       x = "Year",
       y = "Precipitation (in)") +
  theme_minimal()

# #monthly precipitation trends
# precip_monthly <- monthly.noaa.data %>%
#   group_by(year, month) %>% 
#   summarize(MonthlyAvgPrecip = mean(ppt, na.rm = TRUE)) %>%
#   mutate(year_month = as.Date(paste(year, sprintf("%02d", month), "01", sep = "-")))
# 
# #BASIC - plot monthly precip data 
# ggplot(precip_monthly, aes(x = year_month, y = MonthlyAvgPrecip)) +
#   geom_line(size = 0.5, alpha = 0.5) +
#   geom_point(size = 1) +
#   #geom_smooth(method = "lm", se = FALSE) +
#   scale_x_date(breaks = "5 years", labels = scales::date_format("%Y")) +
#   labs(title = "Average Precipitation (1895-2024)",
#        x = "Year",
#        y = "Precipitation (in)") +
#   theme_minimal()

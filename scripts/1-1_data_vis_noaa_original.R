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
daily.noaa.data <- read.csv("data/processed_data/nClimGrid_daily_clean.csv") %>%
  as_tibble()

#monthly
monthly.noaa.data <- read.csv("data/processed_data/nClimGrid_monthly_clean.csv") %>%
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

## Look at temp data outliers
# temp.McFarland %>% 
#    ggplot(aes(x=year, y=temp)) +
#    geom_line()

# #Check if any rows have NAs for each year 
# na_summary <- clean.McFarland %>% 
#   group_by(year, month, day) %>% 
#   summarize(has_na = any(is.na(TMP_DEGC_combined)))
# 
# #isolate temperature data and get the yearly mean temperature from McFarland data
# temp.McFarland <- clean.McFarland %>% 
#   group_by(year) %>% 
#   filter(all(!is.na(TMP_DEGC_combined))) %>% 
#   summarize(temp = mean(TMP_DEGC_combined))

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
  geom_smooth(method = "lm", se = FALSE) +
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

# create filter for current year - first get current year
current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
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

#top 20 highest mean temps
top20_mean_temps <- daily.noaa.data %>% 
  arrange(desc(tmean)) %>%    
  slice_head(n = 50) %>% 
  mutate(highlight = ifelse(row_number() <= 10, "Top 10", "Other"),
         date = as.Date(date))
  

#graph of high mean temps
ggplot(top20_mean_temps, aes(x = date, y = tmean, color = highlight)) +
  geom_segment(aes(xend = date, y = 15.5, yend = tmean), linetype = "solid", alpha = 0.6) +
  geom_point(size = 3, aes(color = highlight)) +
  scale_color_manual(values = c("Top 10" = "darkred", "Other" = "orange")) +
  scale_x_date(
    breaks = seq(from = min(top20_mean_temps$date), 
                 to = max(top20_mean_temps$date), 
                 by = "10 years"),
    labels = scales::date_format("%Y"),
    limits = c(min(top20_mean_temps$date), max(top20_mean_temps$date))
  ) +
  labs(
    title = "Highest Global Average Temperatures",
    x = "Year",
    y = "Daily average temperature (°C)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )



#summarizing the number of days each year with temp maximums above the average temp maximum
temp.highs <- daily.noaa.data %>% 
  group_by(year) %>% 
  summarise(days.above.mean.max = sum(tmax > 12.10306, na.rm = TRUE)) %>% 
  filter(year < current_year)
  
# Create a bar graph using ggplot2
ggplot(temp.highs, aes(x = factor(year), y = days.above.mean.max)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Number of Days Above Average Max Each Year",
       x = "Year",
       y = "Days Above Average Max") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a line graph using ggplot2
ggplot(temp.highs, aes(x = year, y = days.above.mean.max)) +
  geom_line(size = 1, alpha = 0.5) +
  geom_point(size = 1) +
  scale_x_continuous(breaks = pretty(temp.highs$year, n = 10)) +
  labs(title = "Number of Days Above Average Max Each Year",
       x = "Year",
       y = "Days Above Average Max") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Pat Riffomonas Project temp records summaries
## 95% confidence intervals on the day tells extremes
record.temp.summary <- daily.noaa.data %>% 
  group_by(month, day) %>% 
  summarise(ave_high = mean(tmax, na.rm=T),
            lci_high = quantile(tmax, prob = 0.025, na.rm=T),
            uci_high = quantile(tmax, prob=0.975),
            ave_low = mean(tmin, na.rm=T),
            lci_low = quantile(tmin, prob = 0.025, na.rm=T),
            uci_low = quantile(tmin, prob=0.975),
            n=n())

#### Record temperatures plot---------------------------------------------------

#highest daily average temps
highest_mean_daily_temps <- daily.noaa.data %>%
  mutate(year = lubridate::year(date)) %>% 
  group_by(year) %>%
  filter(tmean == max(tmean, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(UnitCode, long, lat, tmean, year, date) %>% 
  rename(tmean.max = tmean,
         tmean.max.date = date,
         unit.code = UnitCode)
# %>%
#   arrange(desc(tmean)) %>% 
#   mutate(highlight = ifelse(row_number() <= 10, "Top 10", "Other"),
#          date = as.Date(date))


#graph of high mean temps
ggplot(highest_mean_daily_temps, aes(x = date, y = tmean, color = highlight)) +
  geom_segment(aes(xend = date, y = min(tmean), yend = tmean), linetype = "solid", alpha = 0.6) +
  geom_point(size = 3, aes(color = highlight)) +
  scale_color_manual(values = c("Top 10" = "darkred", "Other" = "orange")) +
  scale_x_date(
    breaks = seq(from = min(highest_mean_daily_temps$date), 
                 to = max(highest_mean_daily_temps$date), 
                 by = "10 years"),
    labels = scales::date_format("%Y"),
    limits = c(min(highest_mean_daily_temps$date), max(highest_mean_daily_temps$date))
  ) +
  labs(
    x = "Year",
    y = "Daily average temperature (°C)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

##highest daily max temps

highest_max_daily_temps <- daily.noaa.data %>%
  mutate(year = lubridate::year(date)) %>% 
  group_by(year) %>%
  filter(tmax == max(tmax, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(UnitCode, long, lat, tmax, year, date) %>% 
  rename(tmax.max = tmax,
         tmax.max.date = date,
         unit.code = UnitCode)

##lowest daily mean temps

lowest_mean_daily_temps <- daily.noaa.data %>%
  mutate(year = lubridate::year(date)) %>% 
  group_by(year) %>%
  filter(tmean == min(tmean, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(UnitCode, long, lat, tmean, year, date) %>% 
  rename(tmean.min = tmean,
         tmean.min.date = date,
         unit.code = UnitCode)

##lowest daily min temps

lowest_min_daily_temps <- daily.noaa.data %>%
  mutate(year = lubridate::year(date)) %>% 
  group_by(year) %>%
  filter(tmin == min(tmin, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(UnitCode, long, lat, tmin, year, date) %>% 
  rename(tmin.min = tmin,
         tmin.min.date = date,
         unit.code = UnitCode)

#combined data table for R shiny app
shiny.daily.temp.records <- highest_mean_daily_temps %>%
  left_join(highest_max_daily_temps, by = c("year", "unit.code", "long", "lat")) %>%
  left_join(lowest_mean_daily_temps, by = c("year", "unit.code", "long", "lat")) %>%
  left_join(lowest_min_daily_temps, by = c("year", "unit.code", "long", "lat")) 

##save outputs as csv
#write.csv(shiny.daily.temp.records, "data/processed_data/shiny_daily_temp_records.csv", row.names = FALSE)


##monthly temp

#highest monthly average temps
highest_mean_monthly_temps <- monthly.noaa.data %>%
  # Create a year_month column for graphing
  mutate(year.month = paste(year, sprintf("%02d", month), "01", sep = "-"),
         year.month = as.Date(year.month)) %>% 
  group_by(year) %>%
  filter(tmean == max(tmean, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(UnitCode, long, lat, tmean, year, year.month) %>% 
  rename(tmean.max = tmean,
         tmean.max.ym = year.month)
# %>%
#   arrange(desc(tmean.max)) 
# %>% 
#   mutate(highlight = ifelse(row_number() <= 10, "Top 10", "Other"))

##save outputs as csv
#write.csv(, "data/processed_data/shiny_merged_temp.csv", row.names = FALSE)

#graph of high mean temps
ggplot(highest_mean_monthly_temps, aes(x = year.month, y = tmean, color = highlight)) +
  geom_segment(aes(xend = year.month, y = min(tmean), yend = tmean), linetype = "solid", alpha = 0.6) +
  geom_point(size = 3, aes(color = highlight)) +
  scale_color_manual(values = c("Top 10" = "darkred", "Other" = "orange")) +
  scale_x_date(
    breaks = seq(from = min(highest_mean_monthly_temps$year.month), 
                 to = max(highest_mean_monthly_temps$year.month), 
                 by = "10 years"),
    labels = scales::date_format("%Y"),
    limits = c(min(highest_mean_monthly_temps$year.month), max(highest_mean_monthly_temps$year.month))
  ) +
  labs(
    x = "Year",
    y = "Monthly average temperature (°C)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

#highest monthly average max temps
highest_max_monthly_temps <- monthly.noaa.data %>%
  # Create a year_month column for graphing
  mutate(year.month = paste(year, sprintf("%02d", month), "01", sep = "-"),
         year.month = as.Date(year.month)) %>% 
  group_by(year) %>%
  filter(tmax == max(tmax, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(UnitCode, long, lat, tmax, year, year.month) %>% 
  rename(tmax.max = tmax,
         tmax.max.ym = year.month) %>%
  arrange(desc(tmax.max)) %>%
  mutate(highlight.max = ifelse(row_number() <= 10, "Top 10", "Other"))

#graph of highest max temps
ggplot(highest_max_monthly_temps, aes(x = tmax.max.ym, y = tmax.max, color = highlight.max)) +
  geom_segment(aes(xend = tmax.max.ym, y = min(tmax.max), yend = tmax.max), linetype = "solid", alpha = 0.6) +
  geom_point(size = 3, aes(color = highlight.max)) +
  scale_color_manual(values = c("Top 10" = "darkred", "Other" = "orange")) +
  scale_x_date(
    breaks = seq(from = min(highest_max_monthly_temps$tmax.max.ym), 
                 to = max(highest_max_monthly_temps$tmax.max.ym), 
                 by = "10 years"),
    labels = scales::date_format("%Y"),
    limits = c(min(highest_max_monthly_temps$tmax.max.ym), max(highest_max_monthly_temps$tmax.max.ym))
  ) +
  labs(
    x = "Year",
    y = "Monthly average max temperature (°C)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

#lowest monthly average temps
lowest_mean_monthly_temps <- monthly.noaa.data %>%
  # Create a year_month column for graphing
  mutate(year.month = paste(year, sprintf("%02d", month), "01", sep = "-"),
         year.month = as.Date(year.month)) %>% 
  group_by(year) %>%
  filter(tmean == min(tmean, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(UnitCode, long, lat, tmean, year, year.month) %>% 
  rename(tmean.min = tmean,
         tmean.min.ym = year.month)

#lowest monthly min temps
lowest_min_monthly_temps <- monthly.noaa.data %>%
  # Create a year_month column for graphing
  mutate(year.month = paste(year, sprintf("%02d", month), "01", sep = "-"),
         year.month = as.Date(year.month)) %>% 
  group_by(year) %>%
  filter(tmin == min(tmin, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(UnitCode, long, lat, tmin, year, year.month) %>% 
  rename(tmin.min = tmin,
         tmin.min.ym = year.month)


##monthly precip

#highest monthly average precip
highest_monthly_precip <- monthly.noaa.data %>%
  # Create a year_month column for graphing
  mutate(year.month = paste(year, sprintf("%02d", month), "01", sep = "-"),
         year.month = as.Date(year.month)) %>% 
  group_by(year) %>%
  filter(ppt == max(ppt, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(UnitCode, long, lat, ppt, year, year.month) %>% 
  rename(ppt.max = ppt,
         ppt.max.ym = year.month)

#lowest monthly precip
lowest_monthly_precip <- monthly.noaa.data %>%
  # Create a year_month column for graphing
  mutate(year.month = paste(year, sprintf("%02d", month), "01", sep = "-"),
         year.month = as.Date(year.month)) %>% 
  group_by(year) %>%
  filter(ppt == min(ppt, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(UnitCode, long, lat, ppt, year, year.month) %>% 
  rename(ppt.min = ppt,
         ppt.min.ym = year.month)

#combined data table for R shiny app
shiny.monthly.precip.records <- highest_monthly_precip %>%
  left_join(lowest_monthly_precip, by = c("year", "UnitCode", "long", "lat")) 

##save outputs as csv
#write.csv(shiny.monthly.precip.records, "data/processed_data/shiny_monthly_precip_records.csv", row.names = FALSE)

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

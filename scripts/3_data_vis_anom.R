## Create data visualizations from NOAA climate data
## Data vis 2: Temperature anomalies    

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

#McFarland
clean.McFarland <- read.csv("data/McFarland_clean.csv")

#-----------------------#
####    Data Manip   ####
#-----------------------#

#### Temp anomalies graph type one ---------------------------------------------

## Calculate temp anomaly from monthly NOAA data
# First calculate the baseline as the average temperature per month across all years
monthly.baseline <- monthly.noaa.data %>%
  group_by(month) %>%
  summarize(baseline = mean(tmean, na.rm = TRUE))

# Join baseline with original data
temp.with.baseline <- monthly.noaa.data %>%
  left_join(monthly.baseline, by = "month")

# Calculate the monthly temperature anomaly by subtracting the monthly baseline from the actual mean temperature of each month
temp.with.anomalies <- temp.with.baseline %>%
  mutate(temp.anomaly = tmean - baseline) %>%
  
  # Create a year_month column for graphing
  mutate(year.month = paste(year, sprintf("%02d", month), "01", sep = "-"),
         year.month = as.Date(year.month))



## Calculate temp anomaly for McFarland data

#First calculate monthly temperature means
McFarland.monthly.data <- clean.McFarland %>%
  group_by(year, month) %>%
  summarize(tmean = mean(TMP_DEGC_combined, na.rm = TRUE))

# Calculate the baseline as the average temperature per month across all years
McFarland.baseline <- McFarland.monthly.data %>%
  group_by(month) %>%
  summarize(baseline = mean(tmean, na.rm = TRUE))

# Join baseline with original data
McFarland.temp.with.baseline <- McFarland.monthly.data %>%
  left_join(McFarland.baseline, by = "month")

# Calculate the monthly temperature anomaly by subtracting the monthly baseline from the actual mean temperature of each month
McFarland.temp.with.anomalies <- McFarland.temp.with.baseline %>%
  mutate(temp.anomaly = tmean - baseline) %>%
  filter(!is.na(year) & !is.na(month)) %>% 
  
  # Create a year_month column for graphing
  mutate(year.month = paste(year, sprintf("%02d", month), "01", sep = "-"),
         year.month = as.Date(year.month))

# Combine both data sets into one but keep separated by source
merged.anom <- bind_rows(
  temp.with.anomalies %>% mutate(source = "NOAA"),
  McFarland.temp.with.anomalies %>% mutate(source = "McFarland")
)

# Graph temp anomalies over time using ggplot2
ggplot(temp.with.anomalies, aes(x = year.month, y = temp.anomaly, fill = temp.anomaly > 0)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  #annotate("text", x = min(temp.with.anomalies$year), y = 0.1, label = "Average baseline", hjust = 0, color = "black") +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue"),
                    labels = c("TRUE" = "Below baseline", "FALSE" = "Above baseline")) +
  scale_x_date(
    breaks = seq(from = min(temp.with.anomalies$year.month), 
                 to = max(temp.with.anomalies$year.month), 
                 by = "10 years"),
    labels = scales::date_format("%Y"),
    limits = c(min(temp.with.anomalies$year.month), max(temp.with.anomalies$year.month))
  ) +
  labs(title = "Monthly Temperature Anomalies Over Time",
       x = "Year", 
       y = "Temperature Anomaly (°C)") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#Graph combining both data sources
# Graph temp anomalies over time using ggplot2
# ggplot(merged.anom, aes(x = year.month, y = temp.anomaly, fill = temp.anomaly > 0, color = source)) +
#   geom_bar(stat = "identity") +
#   geom_hline(yintercept = 0, linetype = "solid", color = "black") +
#   #annotate("text", x = min(merged.anom$year), y = 0.1, label = "Average baseline", hjust = 0, color = "black") +
#   scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue"),
#                     labels = c("TRUE" = "Below baseline", "FALSE" = "Above baseline")) +
#   scale_color_manual(values = c("NOAA" = "blue", "McFarland" = "green")) +
#   scale_x_date(
#     breaks = seq(from = min(merged.anom$year.month), 
#                  to = max(merged.anom$year.month), 
#                  by = "10 years"),
#     labels = scales::date_format("%Y"),
#     limits = c(min(merged.anom$year.month), max(merged.anom$year.month))
#   ) +
#   labs(title = "Monthly Temperature Anomalies Over Time",
#        x = "Year", 
#        y = "Temperature Anomaly (°C)") +
#   theme_minimal() +
#   theme(
#     legend.title = element_blank(),
#     legend.position = "bottom",
#     axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

##manipulate data and create  merged data set for R shiny dashboard graph
#NOAA
shiny.noaa.anom <- temp.with.anomalies %>% 
  select(year, month, year.month, temp.anomaly) %>% 
  rename(noaa.anom = temp.anomaly,
         noaa.year.month = year.month)
#McFarland
shiny.mcfarland.anom <- McFarland.temp.with.anomalies %>% 
  select(year, month, year.month, temp.anomaly) %>% 
  rename(mcfarland.anom = temp.anomaly,
         mcfarland.year.month = year.month)
#Merged anomaly data
shiny.merged.anom <- shiny.noaa.anom  %>% 
  left_join(shiny.mcfarland.anom, by = c("year", "month"))

##save outputs as csv
# write.csv(shiny.merged.anom, "data/shiny_merged_anom.csv", row.names = FALSE)


#### Temp anomalies graph type two ---------------------------------------------

###attempting this - https://www.youtube.com/watch?v=DrNQMaIVEVo 

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




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

#-----------------------#
####    Data Manip   ####
#-----------------------#

#### Temp anomalies graph type one

# Calculate temp anomaly from monthly NOAA data
# First calculate the baseline as the average temperature per month across all years
monthly.baseline <- monthly.noaa.data %>%
  group_by(month) %>%
  summarize(baseline = mean(tmean, na.rm = TRUE))

# Join baseline with original data
temp.with.baseline <- monthly.noaa.data %>%
  left_join(monthly.baseline, by = "month")

# Calculate the monthly temperature anomaly by subtracting the monthly baseline from the actual mean temperature of each month
temp.with.anomalies <- temp.with.baseline %>%
  mutate(TempAnomaly = tmean - baseline) %>%
  
  # Create a year_month column for graphing
  mutate(year_month = paste(year, sprintf("%02d", month), "01", sep = "-"),
         year_month = as.Date(year_month))

# Graph temp anomalies over time using ggplot2
ggplot(temp.with.anomalies, aes(x = year_month, y = TempAnomaly, fill = TempAnomaly > 0)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  #annotate("text", x = min(temp.with.anomalies$year), y = 0.1, label = "Average baseline", hjust = 0, color = "black") +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue"),
                    labels = c("TRUE" = "Below baseline", "FALSE" = "Above baseline")) +
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


#### Temp anomalies graph type two

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




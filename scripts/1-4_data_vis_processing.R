## Climate data processing ##

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
mcfarland.clean <- read.csv("data/processed_data/mcfarland_clean.csv")

#SERC 
serc.clean <- read.csv("data/processed_data/serc_clean.csv")

#-----------------------#
####    Data Manip   ####
#-----------------------#

# first get current year for data filtering
current.year <- as.numeric(format(Sys.Date(), "%Y"))

#### Temperature trends overtime -----------------------------------------------

##long-term temperature trends using monthly NOAA, McFarland Hill, and SERC climate data

# General function to calculate yearly temperature trends
calculate_yearly_temperature <- function(data, temp.col, max.col = NULL, min.col = NULL) {
  data %>%
    group_by(year) %>%
    summarize(
      YearlyAvgTemp = mean(.data[[temp.col]], na.rm = TRUE),
      YearlyAvgMax = if (!is.null(max.col)) mean(.data[[max.col]], na.rm = TRUE) else NA_real_,
      YearlyAvgMin = if (!is.null(min.col)) mean(.data[[min.col]], na.rm = TRUE) else NA_real_
    )
}

# General function to calculate yearly precipitation trends
calculate_yearly_precipitation <- function(data, precip.col) {
  data %>%
    mutate(ppt.in.hr = .data[[precip.col]] * 0.0393701) %>% # Convert mm to inches
    group_by(year) %>%
    summarize(YearlyTotalPrecip = sum(ppt.in.hr, na.rm = TRUE))
}

# General function to prepare merged data for shiny
prepare_shiny_data <- function(noaa.data, mcfarland.data, serc.data, value.name, current_year = Sys.Date()$year) {
  noaa <- noaa.data %>% rename_with(~ paste0("noaa.", .), -year)
  mcfarland <- mcfarland.data %>% rename_with(~ paste0("mcfarland.", .), -year)
  serc <- serc.data %>% rename_with(~ paste0("serc.", .), -year)
  
  merged.data <- noaa %>%
    left_join(mcfarland, by = "year") %>%
    left_join(serc, by = "year") %>%
    filter(year < current.year)
  
  return(merged.data)
}

# Calculate yearly temperature trends
yearly.temp.noaa <- calculate_yearly_temperature(noaa.monthly.data, temp.col = "tmean", max.col = "tmax", min.col = "tmin")
yearly.temp.mcfarland <- calculate_yearly_temperature(mcfarland.clean, temp.col = "temp")
yearly.temp.serc <- calculate_yearly_temperature(serc.clean, temp.col = "temp")

# Prepare shiny data for temperature trends
shiny.merged.temp <- prepare_shiny_data(
  noaa.data = yearly.temp.noaa,
  mcfarland.data = yearly.temp.mcfarland,
  serc.data = yearly.temp.serc,
  value.name = "temp",
  current.year = current.year
)

# Adjust specific data points (e.g., McFarland for 1998)
shiny.merged.temp <- shiny.merged.temp %>%
  mutate(mcfarland.YearlyAvgTemp = if_else(year == 1998, NA_real_, mcfarland.YearlyAvgTemp))

# Calculate yearly precipitation trends
yearly.precip.noaa <- calculate_yearly_precipitation(noaa.monthly.data, precip.col = "ppt")
yearly.precip.mcfarland <- calculate_yearly_precipitation(mcfarland.clean, precip.col = "ppt")
yearly.precip.serc <- calculate_yearly_precipitation(serc.clean, precip.col = "ppt")

# Prepare shiny data for precipitation trends
shiny.merged.precip <- prepare_shiny_data(
  noaa.data = yearly.precip.noaa,
  mcfarland.data = yearly.precip.mcfarland,
  serc.data = yearly.precip.serc,
  value.name = "precip",
  current.year = current.year
)

# Save outputs to CSV
write.csv(shiny.merged.temp, "data/processed_data/shiny_merged_temp.csv", row.names = FALSE)
write.csv(shiny.merged.precip, "data/processed_data/shiny_merged_precip.csv", row.names = FALSE)


#### Calculate Anomalies--------------------------------------------------------

# General function to calculate the baseline
calculate_baseline <- function(data, value_col) {
  data %>%
    group_by(month) %>%
    summarize(baseline = mean(.data[[value_col]], na.rm = TRUE))
}

# General function to calculate anomalies and prepare data for graphing
calculate_anomalies <- function(data, baseline, value_col, anomaly_prefix = "anomaly") {
  data %>%
    left_join(baseline, by = "month") %>%
    mutate(
      !!sym(paste0(anomaly_prefix, ".value")) := .data[[value_col]] - baseline,
      !!sym(paste0(anomaly_prefix, ".percent")) := (.data[[value_col]] - baseline) / baseline * 100
    ) %>%
    filter(!is.na(year) & !is.na(month)) %>%
    mutate(year.month = as.Date(paste(year, sprintf("%02d", month), "01", sep = "-")))
}

# Combined workflow for processing anomalies (temperature or precipitation)
process_anomalies <- function(clean_data, value_col, anomaly_prefix, current_year = Sys.Date()$year) {
  # Calculate monthly data (averages per year and month)
  monthly_data <- clean_data %>%
    group_by(year, month) %>%
    summarize(average_value = mean(.data[[value_col]], na.rm = TRUE))
  
  # Calculate baseline
  baseline <- calculate_baseline(monthly_data, value_col = "average_value")
  
  # Calculate anomalies
  anomalies <- calculate_anomalies(monthly_data, baseline, value_col = "average_value", anomaly_prefix = anomaly_prefix)
  
  # Prepare for shiny dashboard
  shiny_anomalies <- anomalies %>%
    select(year, month, year.month, starts_with(anomaly_prefix)) %>%
    filter(year < current_year)
  
  return(shiny_anomalies)
}

# Process temperature anomalies
shiny.temp.anomalies.noaa <- process_anomalies(noaa.monthly.data, value_col = "tmean", anomaly_prefix = "temp", current_year)
shiny.temp.anomalies.mcfarland <- process_anomalies(mcfarland.clean, value_col = "temp", anomaly_prefix = "temp", current_year)
shiny.temp.anomalies.serc <- process_anomalies(serc.clean, value_col = "temp", anomaly_prefix = "temp", current_year)

# Process precipitation anomalies
shiny.precip.anomalies.noaa <- process_anomalies(noaa.monthly.data, value_col = "ppt", anomaly_prefix = "precip", current_year)
shiny.precip.anomalies.mcfarland <- process_anomalies(mcfarland.clean, value_col = "ppt", anomaly_prefix = "precip", current_year)
shiny.precip.anomalies.serc <- process_anomalies(serc.clean, value_col = "ppt", anomaly_prefix = "precip", current_year)

# Rename columns for clarity
rename_anomalies <- function(data, prefix) {
  data %>%
    rename_with(
      ~ paste0(prefix, ".", .),
      starts_with("temp.") | starts_with("precip.")
    )
}

shiny.temp.anomalies.noaa <- rename_anomalies(shiny.temp.anomalies.noaa, "noaa")
shiny.temp.anomalies.mcfarland <- rename_anomalies(shiny.temp.anomalies.mcfarland, "mcfarland")
shiny.temp.anomalies.serc <- rename_anomalies(shiny.temp.anomalies.serc, "serc")
shiny.precip.anomalies.noaa <- rename_anomalies(shiny.precip.anomalies.noaa, "noaa")
shiny.precip.anomalies.mcfarland <- rename_anomalies(shiny.precip.anomalies.mcfarland, "mcfarland")
shiny.precip.anomalies.serc <- rename_anomalies(shiny.precip.anomalies.serc, "serc")

# Merge datasets
shiny.merged.anomalies <- shiny.temp.anomalies.noaa %>%
  left_join(shiny.temp.anomalies.mcfarland, by = c("year", "month")) %>%
  left_join(shiny.temp.anomalies.serc, by = c("year", "month")) %>%
  left_join(shiny.precip.anomalies.noaa, by = c("year", "month")) %>%
  left_join(shiny.precip.anomalies.mcfarland, by = c("year", "month")) %>% 
  left_join(shiny.precip.anomalies.serc, by = c("year", "month"))


##save outputs as csv
# write.csv(shiny.merged.anomalies, "data/processed_data/shiny_merged_anom.csv", row.names = FALSE)



  

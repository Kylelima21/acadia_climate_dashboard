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
noaa.daily.data <- read.csv("data/processed_data/nClimGrid_daily_clean.csv") %>%
  as_tibble()

#NOAA monthly
noaa.monthly.data <- read.csv("data/processed_data/nClimGrid_monthly_clean.csv") %>%
  as_tibble()

#McFarland
mcfarland.clean <- read.csv("data/processed_data/mcfarland_clean.csv")

#SERC 
serc.clean <- read.csv("data/processed_data/serc_clean.csv")

#-----------------------#
####    Data Manip   ####
#-----------------------#

# Constants
MM_TO_INCHES <- 0.0393701

#' Get current year
#' @return numeric Current year
get_current_year <- function() {
  as.numeric(format(Sys.Date(), "%Y"))
}

#### Processing climate data for long-term trend plots ####

#' Process precipitation data with datetime handling
#' @param data Dataframe containing precipitation data
#' @param datetime.col Name of datetime column
#' @param precip.col Name of precipitation column
#' @return Processed precipitation data with daily values
process_precipitation_data <- function(data, datetime.col, precip.col) { 
  result <- data %>%
    mutate(
      datetime = as.POSIXct(!!sym(datetime.col)),
      date = lubridate::date(datetime),
      hour = as.numeric(format(datetime, "%H")),
      minute = as.numeric(format(datetime, "%M"))
    ) %>%
    group_by(date) %>%
    arrange(desc(hour), desc(minute), .by_group = TRUE) %>%
    slice_head(n = 1) %>%
    select(
      year,
      date,
      datetime,
      precipitation = !!sym(precip.col)
    ) %>%
    ungroup()
  
  return(result)
}

#' Calculate yearly temperature statistics
#' @param data Dataframe containing temperature data
#' @param temp.col Name of temperature column
#' @param max.col Optional: name of maximum temperature column
#' @param min.col Optional: name of minimum temperature column
#' @return Dataframe with yearly temperature statistics
calculate_yearly_temperature <- function(data, temp.col, max.col = NULL, min.col = NULL) {
  if (!temp.col %in% names(data)) {
    stop(sprintf("Column '%s' not found in data", temp.col))
  }
  
  result <- data %>%
    group_by(year) %>%
    summarise(
      YearlyAvgTemp = ifelse(all(is.na(.data[[temp.col]])),
                             NA_real_,
                             mean(.data[[temp.col]], na.rm = TRUE)),
      .groups = "drop"
    )
  
  # Add max temperature if column provided
  if (!is.null(max.col) && max.col %in% names(data)) {
    result <- result %>%
      left_join(
        data %>%
          group_by(year) %>%
          summarise(
            YearlyAvgMax = ifelse(all(is.na(.data[[max.col]])),
                                  NA_real_,
                                  mean(.data[[max.col]], na.rm = TRUE)),
            .groups = "drop"
          ),
        by = "year"
      )
  }
  
  # Add min temperature if column provided
  if (!is.null(min.col) && min.col %in% names(data)) {
    result <- result %>%
      left_join(
        data %>%
          group_by(year) %>%
          summarise(
            YearlyAvgMin = ifelse(all(is.na(.data[[min.col]])),
                                  NA_real_,
                                  mean(.data[[min.col]], na.rm = TRUE)),
            .groups = "drop"
          ),
        by = "year"
      )
  }
  
  return(result)
}

#' Calculate yearly precipitation
#' @param data Dataframe containing precipitation data
#' @param precip.col Name of precipitation column
#' @param datetime.col Optional: datetime column for temporal processing
#' @return Dataframe with yearly precipitation totals
calculate_yearly_precipitation <- function(data, precip.col, datetime.col = NULL) {
  if (!precip.col %in% names(data)) {
    stop(sprintf("Column '%s' not found in data", precip.col))
  }
  
  # Process datetime data if provided
  if (!is.null(datetime.col)) {
    data <- process_precipitation_data(data, datetime.col, precip.col)
    precip.col <- "precipitation"
  }
  
  data %>%
    mutate(ppt.in.hr = .data[[precip.col]] * MM_TO_INCHES) %>%
    group_by(year) %>%
    summarise(
      YearlyTotalPrecip = ifelse(all(is.na(ppt.in.hr)), 
                                 NA_real_,
                                 sum(ppt.in.hr, na.rm = TRUE)),
      .groups = "drop"
    )
}

#' Prepare merged data for Shiny dashboard
#' @param noaa.data NOAA data
#' @param mcfarland.data McFarland data
#' @param serc.data SERC data
#' @param value.name Type of data being merged
#' @param current_year Current year for filtering
#' @return Merged dataset
prepare_shiny_data <- function(noaa.data, mcfarland.data, serc.data, 
                               value.name, current_year = get_current_year()) {
  # Input validation
  required_cols <- c("year")
  for (df in list(
    list(noaa.data, "NOAA"),
    list(mcfarland.data, "McFarland"),
    list(serc.data, "SERC")
  )) {
    if (!all(required_cols %in% names(df[[1]]))) {
      stop(sprintf("%s data missing required columns", df[[2]]))
    }
  }
  
  # Add source prefixes to column names
  noaa <- noaa.data %>% 
    rename_with(~ paste0("noaa.", .), -year)
  
  mcfarland <- mcfarland.data %>% 
    rename_with(~ paste0("mcfarland.", .), -year)
  
  serc <- serc.data %>% 
    rename_with(~ paste0("serc.", .), -year)
  
  # Merge datasets
  merged.data <- noaa %>%
    left_join(mcfarland, by = "year") %>%
    left_join(serc, by = "year") %>%
    filter(year < current_year)
  
  return(merged.data)
}

# Main execution function
process_weather_data <- function() {
  tryCatch({
    current.year <- get_current_year()
    
    # Clean McFarland data (remove 1998)
    mcfarland.clean <- mcfarland.clean %>%
      mutate(
        temp = if_else(year == 1998, NA_real_, temp),
        ppt = if_else(year == 1998, NA_real_, ppt)
      )
    
    # Calculate temperature trends
    yearly.temp.noaa <- calculate_yearly_temperature(
      noaa.monthly.data, 
      temp.col = "tmean", 
      max.col = "tmax", 
      min.col = "tmin"
    )
    
    yearly.temp.mcfarland <- calculate_yearly_temperature(
      mcfarland.clean, 
      temp.col = "temp"
    )
    
    yearly.temp.serc <- calculate_yearly_temperature(
      serc.clean, 
      temp.col = "temp"
    )
    
    # Calculate precipitation trends with datetime handling for SERC
    yearly.precip.noaa <- calculate_yearly_precipitation(
      noaa.monthly.data, 
      precip.col = "ppt"
    )
    
    yearly.precip.mcfarland <- calculate_yearly_precipitation(
      mcfarland.clean, 
      precip.col = "ppt"
    )
    
    yearly.precip.serc <- calculate_yearly_precipitation(
      serc.clean, 
      precip.col = "ppt.24hr",
      datetime.col = "date.time.est"
    )
    
    # Merge data
    list(
      temperature = prepare_shiny_data(
        yearly.temp.noaa,
        yearly.temp.mcfarland,
        yearly.temp.serc,
        "temp",
        current.year
      ),
      precipitation = prepare_shiny_data(
        yearly.precip.noaa,
        yearly.precip.mcfarland,
        yearly.precip.serc,
        "precip",
        current.year
      )
    )
    
  }, error = function(e) {
    message("Error in data processing: ", e$message)
    NULL
  }, warning = function(w) {
    message("Warning in data processing: ", w$message)
  })
}

#generate results
results <- process_weather_data()
if (!is.null(results)) {
  temperature.data.merged <- results$temperature
  precipitation.data.merged <- results$precipitation
}

# Save outputs to CSV
# write.csv(temperature.data.merged, "data/processed_data/temperature_data_merged.csv", row.names = FALSE)
# write.csv(precipitation.data.merged, "data/processed_data/precipitation_data_merged.csv", row.names = FALSE)


#### Calculate Anomalies--------------------------------------------------------

# Function to calculate the baseline
calculate_baseline <- function(data, value.col) {
  data %>%
    group_by(month) %>%
    summarize(baseline = mean(.data[[value.col]], na.rm = TRUE))
}

# Function to calculate anomalies and prepare data for graphing
calculate_anomalies <- function(data, baseline, value.col, anomaly.prefix = "anomaly") {
  data %>%
    left_join(baseline, by = "month") %>%
    mutate(
      !!sym(paste0(anomaly.prefix, ".value")) := .data[[value.col]] - baseline,
      !!sym(paste0(anomaly.prefix, ".percent")) := (.data[[value.col]] - baseline) / baseline * 100
    ) %>%
    filter(!is.na(year) & !is.na(month)) %>%
    mutate(year.month = as.Date(paste(year, sprintf("%02d", month), "01", sep = "-")))
}

# Combined workflow for processing anomalies (temperature or precipitation)
process_anomalies <- function(clean.data, value.col, anomaly.prefix, current.year = Sys.Date()$year) {
  # Calculate monthly data (averages per year and month)
  monthly.data <- clean.data %>%
    group_by(year, month) %>%
    summarize(average.value = mean(.data[[value.col]], na.rm = TRUE))
  
  # Calculate baseline
  baseline <- calculate_baseline(monthly.data, value.col = "average.value")
  
  # Calculate anomalies
  anomalies <- calculate_anomalies(monthly.data, baseline, value.col = "average.value", anomaly.prefix = anomaly.prefix)
  
  # Prepare for shiny dashboard
  shiny.anomalies <- anomalies %>%
    select(year, month, year.month, starts_with(anomaly.prefix)) %>%
    filter(year < current.year)
  
  return(shiny.anomalies)
}

# Process temperature anomalies
shiny.temp.anomalies.noaa <- process_anomalies(noaa.monthly.data, value.col = "tmean", anomaly.prefix = "temp", current.year)
shiny.temp.anomalies.mcfarland <- process_anomalies(mcfarland.clean, value.col = "temp", anomaly.prefix = "temp", current.year)
shiny.temp.anomalies.serc <- process_anomalies(serc.clean, value.col = "temp", anomaly.prefix = "temp", current.year)

# Process precipitation anomalies
shiny.precip.anomalies.noaa <- process_anomalies(noaa.monthly.data, value.col = "ppt", anomaly.prefix = "precip", current.year)
shiny.precip.anomalies.mcfarland <- process_anomalies(mcfarland.clean, value.col = "ppt", anomaly.prefix = "precip", current.year)
shiny.precip.anomalies.serc <- process_anomalies(serc.clean, value.col = "ppt", anomaly.prefix = "precip", current.year)

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


#### Record plots---------------------------------------------------------------

calculate_weather_records <- function(daily.data, monthly.data) {
  # Function to create year.month date format
  create_year_month <- function(data) {
    data %>%
      mutate(year.month = paste(year, sprintf("%02d", month), "01", sep = "-"),
             year.month = as.Date(year.month))
  }
  
  # Function to find extremes for daily data
  find_daily_extremes <- function(data, var.name, stat.type) {
    data %>%
      mutate(year = lubridate::year(date)) %>%
      group_by(year) %>%
      filter(!!sym(var.name) == ifelse(stat.type == "max", 
                                       max(!!sym(var.name), na.rm = TRUE),
                                       min(!!sym(var.name), na.rm = TRUE))) %>%
      ungroup() %>%
      select(UnitCode, long, lat, !!sym(var.name), year, date) %>%
      rename(
        !!paste0(var.name, ".", stat.type) := !!sym(var.name),
        !!paste0(var.name, ".", stat.type, ".date") := date,
        unit.code = UnitCode
      )
  }
  
  # Function to find extremes for monthly data
  find_monthly_extremes <- function(data, var.name, stat.type) {
    data %>%
      create_year_month() %>%
      group_by(year) %>%
      filter(!!sym(var.name) == ifelse(stat.type == "max", 
                                       max(!!sym(var.name), na.rm = TRUE),
                                       min(!!sym(var.name), na.rm = TRUE))) %>%
      ungroup() %>%
      select(UnitCode, long, lat, !!sym(var.name), year, year.month) %>%
      rename(
        !!paste0(var.name, ".", stat.type) := !!sym(var.name),
        !!paste0(var.name, ".", stat.type, ".ym") := year.month,
        unit.code = UnitCode
      )
  }
  
  # Calculate daily records
  daily.records <- list(
    highest.mean = find_daily_extremes(daily.data, "tmean", "max"),
    highest.max = find_daily_extremes(daily.data, "tmax", "max"),
    lowest.mean = find_daily_extremes(daily.data, "tmean", "min"),
    lowest.min = find_daily_extremes(daily.data, "tmin", "min"),
    highest.precip = find_daily_extremes(daily.data, "ppt", "max"),
    lowest.precip = find_daily_extremes(daily.data, "ppt", "min")
  )
  
  # Combine daily records
  shiny.daily.records <- daily.records$highest.mean %>%
    left_join(daily.records$highest.max, 
              by = c("year", "unit.code", "long", "lat")) %>%
    left_join(daily.records$lowest.mean, 
              by = c("year", "unit.code", "long", "lat")) %>%
    left_join(daily.records$lowest.min, 
              by = c("year", "unit.code", "long", "lat")) %>%
    left_join(daily.records$highest.precip, 
              by = c("year", "unit.code", "long", "lat")) %>%
    left_join(daily.records$lowest.precip, 
              by = c("year", "unit.code", "long", "lat"))
  
  # Calculate monthly records
  monthly.records <- list(
    highest.mean = find_monthly_extremes(monthly.data, "tmean", "max"),
    highest.max = find_monthly_extremes(monthly.data, "tmax", "max"),
    lowest.mean = find_monthly_extremes(monthly.data, "tmean", "min"),
    lowest.min = find_monthly_extremes(monthly.data, "tmin", "min"),
    highest.precip = find_monthly_extremes(monthly.data, "ppt", "max"),
    lowest.precip = find_monthly_extremes(monthly.data, "ppt", "min")
  )
  
  # Combine monthly records
  shiny.monthly.records <- monthly.records$highest.mean %>%
    left_join(monthly.records$highest.max, 
              by = c("year", "UnitCode", "long", "lat")) %>%
    left_join(monthly.records$lowest.mean, 
              by = c("year", "UnitCode", "long", "lat")) %>%
    left_join(monthly.records$lowest.min, 
              by = c("year", "UnitCode", "long", "lat")) %>%
    left_join(monthly.records$highest.precip, 
              by = c("year", "UnitCode", "long", "lat")) %>%
    left_join(monthly.records$lowest.precip, 
              by = c("year", "UnitCode", "long", "lat"))
  
# Save outputs
# write.csv(shiny.daily.records, "data/processed_data/shiny_daily_records.csv", row.names = FALSE)
# write.csv(shiny.monthly.records, "data/processed_data/shiny_monthly_records.csv", row.names = FALSE)
  
}



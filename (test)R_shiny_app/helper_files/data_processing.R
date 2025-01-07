# Data processing functions
prepare_temperature_data <- function(data) {
  data %>% 
    rename(
      Year = year,
      `NOAA Average Temp` = temp.noaa,
      `NOAA Average Max Temp` = max.noaa,
      `NOAA Average Min Temp` = min.noaa,
      `McFarland Average Temp` = mcfarland
    )
}

prepare_anomaly_data <- function(data) {
  data %>%
    rename(
      Year = year,
      `Year-Month` = noaa.year.month,
      `NOAA Temp Anom` = noaa.anom
    ) %>%
    mutate(
      `Year-Month` = as.Date(`Year-Month`),
      hover_text = case_when(
        !is.na(`NOAA Temp Anom`) ~ paste(
          "Year-Month:", format(`Year-Month`, "%Y-%m"),
          "<br>NOAA Temp Anomaly:", round(`NOAA Temp Anom`, 4)
        )
      )
    )
}

calculate_linear_models <- function(data, selected_models) {
  list(
    noaa_avg = if ("lm_noaa_temp" %in% selected_models) 
      lm(`NOAA Average Temp` ~ Year, data = data),
    noaa_max = if ("lm_noaa_max_temp" %in% selected_models) 
      lm(`NOAA Average Max Temp` ~ Year, data = data),
    noaa_min = if ("lm_noaa_min_temp" %in% selected_models) 
      lm(`NOAA Average Min Temp` ~ Year, data = data)
  )
}

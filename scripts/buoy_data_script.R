#------------------------------------------------------------------------
# Compiling water temperature data from NOAA buoys for ACAD and BOHA
#   Code written by Kate Miller 11/8/2024
#------------------------------------------------------------------------

#devtools::install_github("NOAA-EDAB/buoydata")
library(buoydata)
library(tidyverse)
# ACAD buoys:  Frenchman Bay: ATGM1; South of MDI: 44034; Mt Desert Rock: MDRM1
# Only 44034 has water temp and windspeed.
# BOHA buoys: Boston: BHBM3; East of BOHA: 44013

# Native rnoaa package only allows you to download 1 year at a time and is going to be replaced
# soon by a new package. Using the buoydata package on github until that's ready. The buoydata
# package has you download buoy data to disk, then has functions to import and compile those
# files into multiple years of data.

# Here, we're interested in WTMP (water temp), and WSPD (wind speed). WTMP is helpful to
# compare with NETN loggers. Wind speed may help identify major storms that cause changes in
# water temp.

#----- Get buoy data from NOAA NBDC -----
# Data are logged hourly, but the hour column isn't included in the output. The function below will add that.

add_hour <- function(df){
  df <- df |> group_by(YEAR, MONTH, DAY, DATE) |>
    mutate(hour = sprintf("%02d", row_number()-1)) |> ungroup() |>
    mutate(timestamp = as.POSIXct(paste0(DATE, " ", hour, ":00"),
                                  "%Y-%m-%d %H:%M", tz = "America/New_York"))
  return(df)
}

dir <- "../data/rocky/temp_data/"

# Code below is downloading full buoy data by year for each station, then will bind
# the variable of interest together across all years. For now, just looking at
# water temperature (WTMP), wind speed (WSPD), wind direction (WDIR), and
# wave height-m ("WVHT"). Also available are atm pressure (PRES), air temp (ATMP),
# GST, DPD, APD, MWD, PRES, DEWP, VIS, and TIDE.

#---- ACAD ----
get_buoy_data(buoyid = 44034, year = 2013:2024, outDir = dir) # 2024 data not available yet
wtmp <- combine_buoy_data(buoyid = "44034", variable = "WTMP", inDir = dir) |>
  add_hour() |> mutate(WTMP_F = WTMP*(9/5)+32)
wspd <- combine_buoy_data(buoyid = "44034", variable = "WSPD", inDir = dir) |>
  add_hour()
wdir <- combine_buoy_data(buoyid = "44034", variable = "WDIR", inDir = dir) |>
  add_hour()
wvht <- combine_buoy_data(buoyid = "44034", variable = "WVHT", inDir = dir) |>
  add_hour()

comb_44034 <- purrr::reduce(list(wtmp, wspd, wdir, wvht),
                            full_join, by = c("YEAR", "MONTH", "DAY", "DATE", "hour", "timestamp"))

write.csv(comb_44034, "../data/rocky/temp_data/Buoy_data_2013-2023_ACAD_44034.csv", row.names = F)

# Frenchman Bay Buoy
b_id <- "ATGM1"
get_buoy_data(buoyid = b_id, year = c(2013:2024), outDir = dir)

wtmp <- combine_buoy_data(buoyid = b_id, variable = "WTMP", inDir = dir) |>
  add_hour() |> mutate(WTMP_F = WTMP*(9/5)+32)
wspd <- combine_buoy_data(buoyid = b_id, variable = "WSPD", inDir = dir) |>
  add_hour()
wdir <- combine_buoy_data(buoyid = b_id, variable = "WDIR", inDir = dir) |>
  add_hour()
wvht <- combine_buoy_data(buoyid = b_id, variable = "WVHT", inDir = dir) |>
  add_hour()

comb <- purrr::reduce(list(wtmp, wspd, wdir, wvht),
                           full_join, by = c("YEAR", "MONTH", "DAY", "DATE", "hour", "timestamp"))

write.csv(comb, paste0(dir, "Compiled_HT_water_temps/Buoy_data_2013-2023_ACAD_ATGM1.csv"), row.names = F)

ggpubr::ggarrange(
  ggplot(comb_44034, aes(x = DATE, y = WTMP_F)) +
    geom_line() + ylab("Water temp (F)") + rockyIntertidal::theme_rocky() ,
  ggplot(comb_44034, aes(x = DATE, y = WSPD)) +
    geom_line() + ylab("Wind speed (m/s)") +
    rockyIntertidal::theme_rocky(),
  ggplot(comb_44034, aes(x = DATE, y = WVHT)) +
    geom_line() + ylab("Wave height (m)") +
    rockyIntertidal::theme_rocky(),
  ggplot(comb_44034, aes(x = DATE, y = WDIR)) +
    geom_line() + ylab("Wind direction") +
    rockyIntertidal::theme_rocky(),
  nrow = 4
)


ggpubr::ggarrange(
  ggplot(comb, aes(x = DATE, y = WTMP_F)) +
    geom_line() + ylab("Water temp (F)") + rockyIntertidal::theme_rocky() ,
  ggplot(comb, aes(x = DATE, y = WSPD)) +
    geom_line() + ylab("Wind speed (m/s)") +
    rockyIntertidal::theme_rocky(),
  ggplot(comb, aes(x = DATE, y = WVHT)) +
    geom_line() + ylab("Wave height (m)") +
    rockyIntertidal::theme_rocky(),
  ggplot(comb, aes(x = DATE, y = WDIR)) +
    geom_line() + ylab("Wind direction") +
    rockyIntertidal::theme_rocky(),
  nrow = 4
)

#---- BOHA----
get_buoy_data(buoyid = 44013, year = 2013:2023, outDir = dir)
wtmp_44013 <- combine_buoy_data(buoyid = "44013", variable = "WTMP", inDir = dir) |>
  add_hour() |> mutate(WTMP_F = WTMP*(9/5)+32)
wspd_44013 <- combine_buoy_data(buoyid = "44013", variable = "WSPD", inDir = dir) |>
  add_hour()
wdir_44013 <- combine_buoy_data(buoyid = "44013", variable = "WDIR", inDir = dir) |>
  add_hour()
wvht_44013 <- combine_buoy_data(buoyid = "44013", variable = "WVHT", inDir = dir) |>
  add_hour()

comb_44013 <- purrr::reduce(list(wtmp_44013, wspd_44013, wdir_44013, wvht_44013),
                            full_join, by = c("YEAR", "MONTH", "DAY", "DATE", "hour", "timestamp"))

write.csv(comb_44013, "../data/rocky/temp_data/Buoy_data_2013-2023_BOHA_44013.csv", row.names = F)

# Boston Harbor Buoy
b_id <- "BHBM3"
get_buoy_data(buoyid = b_id, year = c(2013:2023), outDir = dir)

wtmp <- combine_buoy_data(buoyid = b_id, variable = "WTMP", inDir = dir) |>
  add_hour() |> mutate(WTMP_F = WTMP*(9/5)+32)
wspd <- combine_buoy_data(buoyid = b_id, variable = "WSPD", inDir = dir) |>
  add_hour()
wdir <- combine_buoy_data(buoyid = b_id, variable = "WDIR", inDir = dir) |>
  add_hour()
wvht <- combine_buoy_data(buoyid = b_id, variable = "WVHT", inDir = dir) |>
  add_hour()

comb <- purrr::reduce(list(wtmp, wspd, wdir, wvht),
                      full_join, by = c("YEAR", "MONTH", "DAY", "DATE", "hour", "timestamp"))

write.csv(comb, paste0(dir, "Compiled_HT_water_temps/Buoy_data_2013-2023_BOHA_BHBM3.csv"), row.names = F)

ggpubr::ggarrange(
  ggplot(comb_44013, aes(x = DATE, y = WTMP_F)) +
    geom_line() + ylab("Water temp (F)") + rockyIntertidal::theme_rocky() ,
  ggplot(comb_44013, aes(x = DATE, y = WSPD)) +
    geom_line() + ylab("Wind speed (m/s)") +
    rockyIntertidal::theme_rocky(),
  ggplot(comb_44013, aes(x = DATE, y = WVHT)) +
    geom_line() + ylab("Wave height (m)") +
    rockyIntertidal::theme_rocky(),
  ggplot(comb_44013, aes(x = DATE, y = WDIR)) +
    geom_line() + ylab("Wind direction") +
    rockyIntertidal::theme_rocky(),
  nrow = 4
)


ggpubr::ggarrange(
  ggplot(comb, aes(x = DATE, y = WTMP_F)) +
    geom_line() + ylab("Water temp (F)") + rockyIntertidal::theme_rocky() ,
  ggplot(comb, aes(x = DATE, y = WSPD)) +
    geom_line() + ylab("Wind speed (m/s)") +
    rockyIntertidal::theme_rocky(),
  ggplot(comb, aes(x = DATE, y = WVHT)) +
    geom_line() + ylab("Wave height (m)") +
    rockyIntertidal::theme_rocky(),
  ggplot(comb, aes(x = DATE, y = WDIR)) +
    geom_line() + ylab("Wind direction") +
    rockyIntertidal::theme_rocky(),
  nrow = 4
)

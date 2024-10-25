## This script builds off from Kate Miller's climateNETN repository and functions
## https://github.com/KateMMiller/climateNETN/tree/main

## The data this script compiles and cleans is the NOAA nClimGrid-Daily dataset
## which can downloaded at:
## https://www.ncei.noaa.gov/data/nclimgrid-daily/access/grids/


## I used terminal and the following lines of code to download each year/month
## of daily data from the https server. 3810 files (88G) in 1h 21m 53s, 2024-10-24.

# cd /Users/kylelima/Desktop/projects/acadia_climate_dashboard/data/nClimGrid_nc
# wget -r -np -nd -A "*.nc" "https://www.ncei.noaa.gov/data/nclimgrid-daily/access/grids/"




#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#
library(tidyverse)
library(ncdf4)
library(sf)
library(raster)
library(lubridate)

## Install the climateNETN package from Kate
devtools::install_github("KateMMiller/climateNETN")




#---------------------------------------------#
####        Set up with climateNETN        ####
#---------------------------------------------#

## Specify that we only need Acadia data
park = "ACAD"

## Get the NETN centroids for filtering
data("NETN_centroids")

## Use only Acadia centroid
cent <- if(any(park == "all")){NETN_centroids
} else {NETN_centroids[NETN_centroids$UnitCode %in% park,]}

## Filter data to a bounding box to make functions faster
NETN_bbox <- data.frame(lat = c(47.38, 44.80, 38.71, 43.40),
                        long = c(-68.71, -66.67, -74.84, -75.54)) %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  sf::st_bbox()




#---------------------------------------------#
####  Create compile_nclimgrid function    ####
#---------------------------------------------#

## Start function with two inputs of year (yr) and month (mon)
compile_nclimgrid <- function(yr, mon) {
  
  ## Specify file path using year and month which are in file name
  path <- paste0("data/nClimGrid_nc/ncdd-", yr, ifelse(mon < 10, "0", ""), mon, "-grd-scaled.nc")
  
  ## Get actual number of days in this month to get all bands of data 
  ## band 1 = the first day of month
  dim <- days_in_month(as.Date(paste0(yr, "-", ifelse(mon < 10, "0", ""), mon, "-01"), "%Y-%m-%d")) %>% 
    as.integer()
  
  ## Get an input number of bands for purrr mapping over this next function
  bands <- 1:dim %>% 
    as.list()
  
  ## Create a sub-function that loops over each band to get the full month's data
  compile_singleband <- function(band) {
  
    ## Precip
    prcp <- raster::raster(path, varname = "prcp", band = band)
    prcp_crop <- raster::crop(prcp, NETN_bbox)
    netn_prcp <- cbind(cent, prcp = raster::extract(prcp_crop, cent[,c("long", "lat")]))
    
    ## Temp max
    tmax <- raster::raster(path, varname = "tmax", band = band)
    tmax_crop <- raster::crop(tmax, NETN_bbox)
    netn_tmax <- cbind(cent, tmax = raster::extract(tmax_crop, cent[,c("long", "lat")]))
    
    ## Temp min
    tmin <- raster::raster(path, varname = "tmin", band = band)
    tmin_crop <- raster::crop(tmin, NETN_bbox)
    netn_tmin <- cbind(cent, tmin = raster::extract(tmin_crop, cent[,c("long", "lat")]))
    
    ## Temp mean
    tavg <- raster::raster(path, varname = "tavg", band = band)
    tavg_crop <- raster::crop(tavg, NETN_bbox)
    netn_tavg <- cbind(cent, tavg = raster::extract(tavg_crop, cent[,c("long", "lat")]))
    
    ## Join the separate metrics into one clean row of data
    clim_list <- list(netn_prcp, netn_tmax, netn_tmin, netn_tavg)
    netn_comb <- reduce(clim_list, full_join, by = c("UnitCode", "UnitName", "long", "lat"))
    
    ## Clean and make tibble
    netn_output <- netn_comb %>% 
      mutate(year = yr,
             month = as.numeric(mon),
             day = as.numeric(band)) %>% 
      as_tibble()
    
    ## Output
    netn_output

  }
  
  ## purrr map over the compile_singleband function to get each days data for that month
  onemon_daily <- purrr::map_dfr(bands, ~compile_singleband(.))
  
}




#---------------------------------------------#
####      Run Function + Clean Data        ####
#---------------------------------------------#

## Get the total year list for purrr mapping
years <- rep(1951:(year(Sys.Date())-1), 12) %>%
  sort()

yrcurrent <- rep(year(Sys.Date()), month(Sys.Date())-1)
 
year_input <- vctrs::vec_c(years, yrcurrent) %>% 
 as.list()


## Get the total month list for purrr mapping
months <- rep(1:12, (year(Sys.Date())-1951))

moncurrent <- (1:(month(Sys.Date())-1))

mon_input <- vctrs::vec_c(months, moncurrent) %>% 
  as.list()


# yrtest <- rep(1951:1952, 12) %>%
#   sort()
# 
# montest <- rep(1:12, 2)

## Run the map2_dfr function for all years and months to get fully compiled daily data
nclimgrid_daily <- purrr::map2_dfr(year_input, mon_input, compile_nclimgrid)


## Clean the compiled daily data
nclim_clean <- nclimgrid_daily %>% 
  rename(ppt = prcp, tmean = tavg) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-"), "%Y-%m-%d"))


## Export cleaned data
write.csv(nclim_clean, "data/nClimGrid_daily_clean.csv", row.names = F)


## This script builds off from Kate Miller's climateNETN package
## https://github.com/KateMMiller/climateNETN

## This script compiles and cleans the NOAA nClimGrid monthly dataset
## which can downloaded at:
## https://www.ncei.noaa.gov/thredds/catalog/data-in-development/nclimgrid/catalog.html


## I just downloaded the full dataset from the above link to speed things up
## You could also use the command line




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

## Get the NETN centroids for filtering
#data("NETN_centroids")
load("data/NETN_centroids.rda")


## Use only Acadia centroid
cent <- if(any(park == "all")){NETN_centroids
} else {NETN_centroids[NETN_centroids$UnitCode %in% park,]}


## Filter data to a bounding box to make functions faster
NETN_bbox <- data.frame(lat = c(47.38, 44.80, 38.71, 43.40),
                        long = c(-68.71, -66.67, -74.84, -75.54)) %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  sf::st_bbox()




#---------------------------------------------#
####  Create comp_mon_nclimgrid function   ####
#---------------------------------------------#

comp_mon_nclimgrid <- function(yr, mon){
  
  ## Create a band for the months per year
  bnd <- (yr - 1895) * 12 + mon
  
  ## Precip
  prcp <- raster("data/nClimGrid_mon/nclimgrid_prcp.nc", band = bnd)
  prcp_crop <- raster::crop(prcp, NETN_bbox)
  netn_prcp <- cbind(NETN_centroids, prcp = raster::extract(prcp_crop, NETN_centroids[,c("long", "lat")]))
  
  ## Temperature maximums
  tmax <- raster("data/nClimGrid_mon/nclimgrid_tmax.nc", band = bnd)
  tmax_crop <- raster::crop(tmax, NETN_bbox)
  netn_tmax <- cbind(NETN_centroids, tmax = raster::extract(tmax_crop, NETN_centroids[,c("long", "lat")]))
  
  ## Temperature minimums
  tmin <- raster("data/nClimGrid_mon/nclimgrid_tmin.nc", band = bnd)
  tmin_crop <- raster::crop(tmin, NETN_bbox)
  netn_tmin <- cbind(NETN_centroids, tmin = raster::extract(tmin_crop, NETN_centroids[,c("long", "lat")]))
  
  ## Temperature averages
  tavg <- raster("data/nClimGrid_mon/nclimgrid_tavg.nc", band = bnd)
  tavg_crop <- raster::crop(tavg, NETN_bbox)
  netn_tavg <- cbind(NETN_centroids, tavg = raster::extract(tavg_crop, NETN_centroids[,c("long", "lat")]))
  
  ## List and combine
  clim_list <- list(netn_prcp, netn_tmax, netn_tmin, netn_tavg)
  netn_comb <- reduce(clim_list, full_join, by = c("UnitCode", "UnitName", "long", "lat"))
  
  ## Adding dates
  netn_comb$year = yr
  netn_comb$month = as.numeric(mon)
  
  ## Return final dataframe
  return(data.frame(netn_comb))
}




#---------------------------------------------#
####      Run Function + Clean Data        ####
#---------------------------------------------#

## Establish years and months as inputs for the function
years <- 1895:2023
months <- 1:12


## Expand these as a grid
yrmon <- expand.grid(year = years, mon =  months)


## Map this grid through the function to get full data up to 2023
netn_hist <- map2(yrmon$year, yrmon$mon, function(x, y){
  comp_mon_nclimgrid(yr = x, mon = y)}, .progress = T) |> list_rbind()


## Add in available 2024 data
mon24 = 1:8
netn_2024 <- map(mon24, function(x){
  comp_mon_nclimgrid(yr = 2024, mon = x)}, .progress = T) |> list_rbind()


## Combine these two into a final data frame
netn_final <- rbind(netn_hist, netn_2024)


## Fix column names
names(netn_final) <- gsub("prcp", "ppt", names(netn_final))
names(netn_final) <- gsub("tavg", "tmean", names(netn_final))


## Clean and filter to ACAD
ACAD_clim_annual <- netn_final %>% 
  as_tibble() %>% 
  filter(UnitCode == "ACAD") %>% 
  arrange(year, month)


## Export cleaned data
write.csv(ACAD_clim_annual, "data/nClimGrid_monthly_clean.csv", row.names = F)



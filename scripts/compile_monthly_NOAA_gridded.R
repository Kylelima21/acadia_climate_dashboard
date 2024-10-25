library(tidyverse)
library(climateNETN)
library(sf)
library(raster)
library(ncdf4)

devtools::install_github("KateMMiller/climateNETN")

data("NETN_centroids")
netn_sf <- st_as_sf(NETN_centroids, coords = c("long", "lat"), crs = 4326)
netn_bbox <- st_bbox(netn_sf)

compnoaa <- function(yr, mon){
  bnd <- (yr - 1895) * 12 + mon
  prcp <- raster("data/nclimgrid_prcp.nc", band = bnd)
  prcp_crop <- raster::crop(prcp, netn_bbox)
  netn_prcp <- cbind(NETN_centroids, prcp = raster::extract(prcp_crop, NETN_centroids[,c("long", "lat")]))
  
  tmax <- raster("data/nclimgrid_tmax.nc", band = bnd)
  tmax_crop <- raster::crop(tmax, netn_bbox)
  netn_tmax <- cbind(NETN_centroids, tmax = raster::extract(tmax_crop, NETN_centroids[,c("long", "lat")]))
  
  tmin <- raster("data/nclimgrid_tmin.nc", band = bnd)
  tmin_crop <- raster::crop(tmin, netn_bbox)
  netn_tmin <- cbind(NETN_centroids, tmin = raster::extract(tmin_crop, NETN_centroids[,c("long", "lat")]))
  
  tavg <- raster("data/nclimgrid_tavg.nc", band = bnd)
  tavg_crop <- raster::crop(tavg, netn_bbox)
  netn_tavg <- cbind(NETN_centroids, tavg = raster::extract(tavg_crop, NETN_centroids[,c("long", "lat")]))
  
  clim_list <- list(netn_prcp, netn_tmax, netn_tmin, netn_tavg)
  netn_comb <- reduce(clim_list, full_join, by = c("UnitCode", "UnitName", "long", "lat"))
  
  netn_comb$year = yr
  netn_comb$month = as.numeric(mon)
  
  return(data.frame(netn_comb))
}

years <- 1895:2023
months <- 1:12
yrmon <- expand.grid(year = years, mon =  months)

netn_hist <- map2(yrmon$year, yrmon$mon, function(x, y){
  compnoaa(yr = x, mon = y)}, .progress = T) |> list_rbind()

mon24 = 1:8
netn_2024 <- map(mon24, function(x){
  compnoaa(yr = 2024, mon = x)}, .progress = T) |> list_rbind()

netn_final <- rbind(netn_hist, netn_2024)

NETN_clim_annual <- netn_final

names(NETN_clim_annual) <- gsub("prcp", "ppt", names(NETN_clim_annual))
names(NETN_clim_annual) <- gsub("tavg", "tmean", names(NETN_clim_annual))

ACAD_clim_annual <- NETN_clim_annual %>% 
  #filter(UnitName == "Acadia National Park") %>% 
  arrange(UnitName, year, month)

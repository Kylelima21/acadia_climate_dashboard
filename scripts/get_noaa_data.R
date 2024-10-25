library(raster)
library(sf)
library(ncdf4)
devtools::install_github("KateMMiller/climateNETN")


park = 'ACAD'


getClimNOAA <- function(park = 'all', year = as.integer(format(Sys.Date(), "%Y")), months = month(Sys.Date())-1){
  
  #--- error handling ---
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "LNETN", "ACAD", "BOHA", "MABI", "MIMA", "MORR",
                      "ROVA", "SAGA", "SAIR", "SARA", "WEFA"))
  if(any(park == "LNETN")){park = c("MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")} else {park}
  if(any(park == "all")){park = c("ACAD", "BOHA", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SAIR", "SARA", "WEFA")}
  stopifnot(class(year) %in% c("numeric", "integer"), year >= 1895)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  
  if(!requireNamespace("raster", quietly = TRUE)){
    stop("Package 'raster' needed to download NOAA gridded climate data. Please install it.", call. = FALSE)
  }
  
  if(!requireNamespace("sf", quietly = TRUE)){
    stop("Package 'sf' needed to download NOAA gridded climate data. Please install it.", call. = FALSE)
  }
  
  if(!requireNamespace("ncdf4", quietly = TRUE)){
    stop("Package 'ncdf4' needed to download NOAA gridded climate data. Please install it.", call. = FALSE)
  }
  
  #mos <- sprintf("%02d", months)
  
  #--- compile data ---
  # Create list of lat/longs to generate
  data("NETN_centroids")
  
  cent <- if(any(park == "all")){NETN_centroids
  } else {NETN_centroids[NETN_centroids$UnitCode %in% park,]}
  
  # bounding box to crop before extract to speed up function
  NETN_bbox <- data.frame(lat = c(47.38, 44.80, 38.71, 43.40),#, 39.994187),
                          long = c(-68.71, -66.67, -74.84, -75.54)) |> #, -80.521832)) |>
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326) |> sf::st_bbox()
  
  #"https://noaa-nclimgrid-monthly-pds.s3.amazonaws.com/202404.prcp.conus.pnt"
  
  getnoaadaily <- function(yr, mon, day){
    #url_base <- "https://www.ncei.noaa.gov/thredds/dodsC/nclimgrid-daily/"
    url_base <- "https://www.ncei.noaa.gov/pub/data/daily-grids/v1-0-0/grids/"
    url_yrmon <- paste0(url_base, yr, "/ncdd-", yr, ifelse(mon < 10, "0", ""), mon, "-grd-scaled.nc")
    
    band <- as.numeric(day)
    
    prcp <- raster::raster(url_yrmon, varname = "prcp", band = band)
    prcp_crop <- raster::crop(prcp, NETN_bbox)
    netn_prcp <- cbind(cent, prcp = raster::extract(prcp_crop, cent[,c("long", "lat")]))
    
    tmax <- raster::raster(url_yrmon, varname = "tmax", band = band)
    tmax_crop <- raster::crop(tmax, NETN_bbox)
    netn_tmax <- cbind(cent, tmax = raster::extract(tmax_crop, cent[,c("long", "lat")]))
    
    tmin <- raster::raster(url_yrmon, varname = "tmin", band = band)
    tmin_crop <- raster::crop(tmin, NETN_bbox)
    netn_tmin <- cbind(cent, tmin = raster::extract(tmin_crop, cent[,c("long", "lat")]))
    
    tavg <- raster::raster(url_yrmon, varname = "tavg", band = band)
    tavg_crop <- raster::crop(tavg, NETN_bbox)
    netn_tavg <- cbind(cent, tavg = raster::extract(tavg_crop, cent[,c("long", "lat")]))
    
    clim_list <- list(netn_prcp, netn_tmax, netn_tmin, netn_tavg)
    netn_comb <- reduce(clim_list, full_join, by = c("UnitCode", "UnitName", "long", "lat"))
    
    netn_output <- netn_comb %>% 
      mutate(year = yr,
             month = as.numeric(mon),
             day = as.numeric(day)) %>% 
      as_tibble()

    netn_output
  }
  

  days <- 1:30 %>%
    as.list()
  
  netn_final <- purrr::map_dfr(days, function(x){
    tryCatch(getnoaadaily(yr = year, mon = months, day = x),
               error = function(e){NULL})})

  
  names(netn_final) <- gsub("prcp", "ppt", names(netn_final))
  names(netn_final) <- gsub("tavg", "tmean", names(netn_final))
  
  if(nrow(netn_final) > 0){return(netn_final)} else {return(NULL)}
}

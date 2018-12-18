library(rayshader)
library(geoviz)
library(raster)
library(tidyverse)
library(plotKML)

#Here, I load a map with the raster package:
# elevation_file <- 'data/nashville_elevations.tif'
elevation_file <- 'data/ned19_n36x25_w087x00_tn_nashvillecity_2011.img'

localtif <- raster::raster(elevation_file)

hike_name <- 'radnor_cove_trail'
num_iterps <- 50000

interpolate_hike <- function(hike){
  list(lon = spline(hike$lon, n = num_iterps)$y, 
       lat = spline(hike$lat, n = num_iterps)$y) %>%
    as_tibble()
}

plot_hike <- function(
  hike_name, localtif, expansion = 0.1, zscale = 1.5, elevation_increase = 20,
  fov=30, theta=-100, phi=25 ){
  hike_file <- glue::glue(here::here('data/{hike_name}.gpx'))
  
  hike_data <- (readGPX(hike_file)$tracks[[1]]$`Morning Hike`) %>% 
    mutate(
      time = lubridate::ymd_hms(time),
      time_since_start = difftime(time, first(time))
    ) %>% 
    interpolate_hike() %>% 
    mutate(
      elevation = raster::extract(localtif, sp::SpatialPoints(.[c('lon', 'lat')])) + elevation_increase,
      iterpolated_elevation = spline(elevation,n = num_iterps)$y,
      smoothed_elevations = zoo::rollmean(elevation, 45, fill = "extend")
    )
  
  # Grab bounding box of the hike data for extraction
  lon_range <- range(hike_data$lon)
  lat_range <- range(hike_data$lat)
  
  # How much padding around the hike path do we want to keep in terms or proportion of total hike dimensions.
  lon_expansion <- diff(lon_range)*expansion
  lat_expansion <- diff(lat_range)*expansion
  
  new_extent <- c(
    lon_range[1] - lon_expansion, 
    lon_range[2] + lon_expansion, 
    lat_range[1] - lat_expansion, 
    lat_range[2] + lat_expansion
  ) %>% extent()
  
  cropped_tif <- crop(localtif, new_extent)
  
  #And convert it to a matrix:
  elev_mat <- cropped_tif %>% 
    raster::extract(raster::extent(cropped_tif),buffer=1000) %>% 
    matrix(nrow = ncol(cropped_tif), ncol = nrow(cropped_tif))
  
  raymat <- ray_shade(elev_mat,lambert = TRUE)
  
  #We use another one of rayshader's built-in textures:
  shaded_map <- elev_mat %>%
    sphere_shade(sunangle = 45, texture = "imhof1") 
  
  if(str_detect(hike_name, 'radnor')){
    shaded_map <- shaded_map %>%  
      add_water(detect_water(elev_mat), color="imhof1") %>%
      add_shadow(raymat,0.7)
  }
  
  shaded_map %>% 
    plot_3d(elev_mat, zscale=zscale, fov=fov, theta=theta, phi=phi)
  
  add_gps_to_rayshader(
    cropped_tif, 
    hike_data$lat,
    hike_data$lon,
    hike_data$smoothed_elevations,
    line_width = 4,
    lightsaber = FALSE,
    colour = "orangered",
    zscale,
    ground_shadow = FALSE
  )
}


plot_hike('burch_reserve', localtif, elevation_increase = 5, expansion = 0.7,
          fov=30, theta=0, phi=30 )
render_depth(focus=0.6,focallength = 200)


plot_hike('red_trail', localtif, elevation_increase = 20)

plot_hike('radnor_cove_trail', localtif, elevation_increase = 5)

plot_hike('radnor_ganier_ridge', localtif)

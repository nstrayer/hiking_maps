library(rayshader)
library(geoviz)
library(raster)
library(tidyverse)
library(plotKML)

#Here, I load a map with the raster package:
# elevation_file <- 'data/nashville_elevations.tif'
elevation_file <- 'data/ned19_n36x25_w087x00_tn_nashvillecity_2011.img'

localtif <- raster::raster(elevation_file)

hike_name <- 'burch_reserve_hike'

make_stl <- function(hike_name, localtif, expansion_amnt = 0.1, num_passes = 8, elevation_increase = 20){
  hike_file <- glue::glue('data/{hike_name}.gpx')
  
  hike_data <- (readGPX(hike_file)$tracks[[1]]$`Morning Hike`) %>% 
    mutate(
      time = lubridate::ymd_hms(time),
      time_since_start = difftime(time, first(time))
    )
  
  num_iterps <- 50000
  
  hike_interpolated <- list(lon = spline(hike_data$lon, n = num_iterps)$y, 
                            lat = spline(hike_data$lat, n = num_iterps)$y) %>%
    as_tibble()
  
  # Grab bounding box of the hike data for extraction
  lon_range <- range(hike_data$lon)
  lat_range <- range(hike_data$lat)
  
  # How much padding around the hike path do we want to keep in terms or proportion of total hike dimensions.
  expansion <- 0.1
  
  lon_expansion <- diff(lon_range)*expansion
  lat_expansion <- diff(lat_range)*expansion
  
  lon_min <- lon_range[1] - lon_expansion
  lon_max <- lon_range[2] + lon_expansion
  lat_min <- lat_range[1] - lat_expansion
  lat_max <- lat_range[2] + lat_expansion
  
  new_extent <- extent(c(lon_min, lon_max, lat_min, lat_max))
  
  cropped_tif <- crop(localtif, new_extent)
  
  
  expand_path <- function(path_data, expansion){
    bind_rows(
      mutate(path_data, lon = lon + expansion, lat = lat + expansion),
      mutate(path_data, lon = lon - expansion, lat = lat + expansion),
      mutate(path_data, lon = lon + expansion, lat = lat - expansion),
      mutate(path_data, lon = lon - expansion, lat = lat - expansion)
    )
  }
  
  path_widths <- lon_expansion * (seq(0,expansion_amnt, length.out = num_passes+1)[-1])
  widened_path <- path_widths %>% purrr::map_df(
    ~expand_path(hike_interpolated, .)
  )

  hike_locations_expanded <- as.matrix(widened_path[c('lon', 'lat')]) %>%
    sp::SpatialPoints()
  
  hike_cell_locs <- raster::cellFromXY(cropped_tif, hike_locations_expanded) 
  
  
  extruded_tif <- cropped_tif
  extruded_tif[hike_cell_locs] <- extruded_tif[hike_cell_locs] + elevation_increase
  
  
  elmat_ridge = matrix(raster::extract(extruded_tif,raster::extent(extruded_tif),buffer=1000),
                       nrow=ncol(extruded_tif),ncol=nrow(extruded_tif))
  
  shaded_map <- elmat_ridge %>%
    sphere_shade(sunangle = 45, texture = "imhof1")
  
  zscale <- 1.5
  shaded_map %>%
    plot_3d(elmat_ridge, zscale = zscale)
  
  save_3dprint(glue::glue("{hike_name}.stl"), maxwidth = 5, unit = "in")
}


make_stl('burch_reserve', localtif, elevation_increase = 15)

make_stl('red_trail', localtif, num_passes = 10, elevation_increase = 20)

make_stl('radnor_cove_trail', localtif)

make_stl('radnor_ganier_ridge', localtif)

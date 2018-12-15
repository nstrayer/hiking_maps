# devtools::install_github("tylermorganwall/rayshader")
# devtools::install_github("neilcharles/geoviz")

library(rayshader)
library(geoviz)
library(raster)
library(tidyverse)
library(plotKML)


# hike_file <- 'data/radnor_ganier_ridge.gpx'
hike_file <- 'data/burch_reserve_hike.gpx'


hike_data <- (readGPX(hike_file)$tracks[[1]]$`Morning Hike`) %>% 
  mutate(
    time = lubridate::ymd_hms(time),
    time_since_start = difftime(time, first(time))
  )


num_iterps <- 50000

hike_interpolated <- list(lon = spline(hike_data$lon, n = num_iterps)$y, 
     lat = spline(hike_data$lat, n = num_iterps)$y) %>%
  as_tibble()

hike_locations <- as.matrix(hike_interpolated[c('lon', 'lat')]) %>%
  sp::SpatialPoints()



#Here, I load a map with the raster package:
# elevation_file <- 'data/nashville_elevations.tif'
elevation_file <- 'data/ned19_n36x25_w087x00_tn_nashvillecity_2011.img'

localtif <- raster::raster(elevation_file)
# %>% 
#   projectRaster(crs = '+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs') 

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

# Now we can reach into the elevation data to extract elevations for hike path.
hike_elevations <- raster::extract(cropped_tif, hike_locations)

new_hike_data <- hike_interpolated %>%
  mutate(
    elevation = hike_elevations + 10,
    iterpolated_elevation = spline(elevation,n = num_iterps)$y,
    smoothed_elevations = zoo::rollmean(elevation, 45, fill = "extend")
  )


#And convert it to a matrix:
elmat <- matrix(raster::extract(cropped_tif,raster::extent(cropped_tif),buffer=1000),
               nrow=ncol(cropped_tif),ncol=nrow(cropped_tif))

#We use another one of rayshader's built-in textures:
shaded_map <- elmat %>%
  sphere_shade(sunangle = 45, texture = "imhof1") 

# 2d plot
# shaded_map %>% plot_map()

# 3d plot
zscale <- 1.5
shaded_map %>%
  plot_3d(elmat, zscale = zscale)


add_gps_to_rayshader(
  cropped_tif, 
  new_hike_data$lat,
  new_hike_data$lon,
  new_hike_data$smoothed_elevations,
  line_width = 4,
  lightsaber = FALSE,
  colour = "orangered",
  zscale,
  ground_shadow = FALSE
)

render_depth(focus = 0.4, focallength = 35, fstop = 20)

shaded_map %>% rayshader::save_png('hike_data_plot.png')


ggplot(hike_data, aes(x = lon, y = lat, color = ele)) + 
  guides(color = FALSE) + 
  geom_point(size = 0.5)


# Modifies the original cropped tif to be raised against surrounding terrain. Dont run before the 
# code above. 

path_width <- lon_expansion*0.1


expand_path <- function(path_data, expansion){
    bind_rows(
      mutate(path_data, lon = lon + expansion, lat = lat + expansion),
      mutate(path_data, lon = lon - expansion, lat = lat + expansion),
      mutate(path_data, lon = lon + expansion, lat = lat - expansion),
      mutate(path_data, lon = lon - expansion, lat = lat - expansion)
    )
}

expanded_hike_points <- hike_interpolated %>% 
  bind_rows(
    expand_path(., path_width),
    expand_path(., path_width*0.5),
    expand_path(., path_width*0.25)
  )




hike_locations_expanded <- as.matrix(expanded_hike_points[c('lon', 'lat')]) %>%
  sp::SpatialPoints()

hike_cell_locs <- raster::cellFromXY(cropped_tif, hike_locations_expanded) 


extruded_tif <- cropped_tif
extruded_tif[hike_cell_locs] <- extruded_tif[hike_cell_locs] + 15


elmat_ridge = matrix(raster::extract(extruded_tif,raster::extent(extruded_tif),buffer=1000),
                     nrow=ncol(extruded_tif),ncol=nrow(extruded_tif))

shaded_map <- elmat_ridge %>%
  sphere_shade(sunangle = 45, texture = "imhof1")

shaded_map %>%
  plot_3d(elmat_ridge, zscale = zscale)

save_3dprint("radnor_3d.stl", maxwidth = 4, unit = "in")



# devtools::install_github("tylermorganwall/rayshader")
# devtools::install_github("neilcharles/geoviz")

library(rayshader)
library(geoviz)
library(raster)
library(tidyverse)
library(plotKML)


zscale <- 4

radnor_hike <- (readGPX('data/radnor_hike.gpx')$tracks[[1]]$`Morning Hike`) %>% 
  mutate(
    time = lubridate::ymd_hms(time),
    time_since_start = difftime(time, first(time))
  )


num_iterps <- 50000

radnor_interpolated <- list(lon = spline(radnor_hike$lon, n = num_iterps)$y, 
     lat = spline(radnor_hike$lat, n = num_iterps)$y) %>%
  as_tibble()

hike_locations <- as.matrix(radnor_interpolated[c('lon', 'lat')]) %>%
  sp::SpatialPoints()

hike_elevations <- raster::extract(cropped_tif, hike_locations)


new_hike_data <- radnor_interpolated %>%
  mutate(
    elevation = hike_elevations + 5,
    iterpolated_elevation = spline(elevation,n = num_iterps)$y,
    smoothed_elevations = zoo::rollmean(elevation, 45, fill = "extend")
  )


#Here, I load a map with the raster package:
localtif <- raster::raster("data/USGS_NED_13_n37w087_IMG.img")

# new_extent <- extent(-86.814364,-86.779664, 36.03867,36.073216)
lon_range <- range(radnor_hike$lon)
lat_range <- range(radnor_hike$lat)

expansion <- 0.1

lon_expansion <- diff(lon_range)*expansion
lat_expansion <- diff(lat_range)*expansion


lon_min <- lon_range[1] - lon_expansion
lon_max <- lon_range[2] + lon_expansion
lat_min <- lat_range[1] - lat_expansion
lat_max <- lat_range[2] + lat_expansion

new_extent <- extent(c(lon_min, lon_max, lat_min, lat_max))

cropped_tif <- crop(localtif, new_extent)

#And convert it to a matrix:
elmat = matrix(raster::extract(cropped_tif,raster::extent(cropped_tif),buffer=1000),
               nrow=ncol(cropped_tif),ncol=nrow(cropped_tif))

# ggmap_overlay <- ggmap_image(cropped_tif, source = "stamen", maptype = "terrain", zoom = 10)
# ggmap_overlay[,,4] <- 0.9

#We use another one of rayshader's built-in textures:
shaded_map <- elmat %>%
  sphere_shade(sunangle = 45, texture = "imhof1") %>% 
  add_water(detect_water(elmat), color="imhof1")

# 2d plot
# shaded_map %>% plot_map()

# 3d plot
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

shaded_map %>% write_png()


ggplot(radnor_hike, aes(x = lon, y = lat, color = ele)) + 
  guides(color = FALSE) + 
  geom_point(size = 0.5)


# Modifies the original cropped tif to be raised against surrounding terrain. Dont run before the 
# code above. 

hike_cell_locs <- raster::cellFromXY(cropped_tif, hike_locations) 

cropped_tif[hike_cell_locs] <- cropped_tif[hike_cell_locs] + 250


elmat_ridge = matrix(raster::extract(cropped_tif,raster::extent(cropped_tif),buffer=1000),
               nrow=ncol(cropped_tif),ncol=nrow(cropped_tif))

shaded_map <- elmat_ridge %>%
  sphere_shade(sunangle = 45, texture = "imhof1")

shaded_map %>%
  plot_3d(elmat, zscale = zscale)

save_3dprint("radnor_3d.stl", maxwidth = 4, unit = "in")


#############################
## Working with LiDAR data
#############################

unzip("26902E233974N.zip")

whitebox::wbt_lidar_tin_gridding(here::here("26902E233974N.las"),
                                 output = here::here("rittenhouse.tif"), minz = 0,
                                 resolution = 1, exclude_cls = '3,4,5,7,13,14,15,16,18')

phillyraster = raster::raster("rittenhouse.tif")

library(rayshader)
library(lubridate)
library(tidyverse)

building_mat = raster_to_matrix(phillyraster)
building_mat_small = reduce_matrix_size(building_mat, 0.5)
suncalc::getSunlightTimes(as.Date("2019-06-21"), lat = 39.9526, lon = -75.1652,tz = "EST")

philly_time_start = lubridate::ymd_hms("2019-06-21 05:30:00", tz = "EST")
philly_time_end = lubridate::ymd_hms("2019-06-21 18:30:00", tz = "EST")

temptime = philly_time_start
philly_existing_shadows = list()
sunangles = list()
counter = 1

while(temptime < philly_time_end) {
  sunangles[[counter]] = suncalc::getSunlightPosition(date = temptime, lat = 39.9526, lon = -75.1652)[4:5]*180/pi
  print(temptime)
  philly_existing_shadows[[counter]] = ray_shade(building_mat_small,
                                  sunangle = sunangles[[counter]]$azimuth+180,
                                  sunaltitude = sunangles[[counter]]$altitude,
                                  lambert = FALSE, zscale = 2,
                                  multicore = TRUE)
  temptime = temptime + duration("3600s")
  counter = counter + 1
}

plot_map(philly_existing_shadows[[2]])

philly_existing_shadows[[2]] %>% 
  reshape2::melt(varnames = c("x","y"), value.name = "light") %>%
  ggplot() + 
  geom_raster(aes(x = rev(x), y = rev(y), fill = light)) +
  scico::scale_fill_scico(palette = 'grayC', direction = -1, guide = 'none') +
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  coord_fixed() + 
  theme_void() +
  ggsave("rittenhouse_shadow.png", height = 8, width = 8, dpi = 300)

shadow_coverage = Reduce(`+`, philly_existing_shadows)/length(philly_existing_shadows)

shadow_coverage %>%
  reshape2::melt(varnames = c("x","y"), value.name = "light") %>%
  ggplot() + 
  geom_raster(aes(x = rev(x), y = rev(y), fill = light)) +
  scico::scale_fill_scico(palette = 'hawaii', name = "Daily Light\nCoverage", guide = 'none') + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  coord_fixed() + 
  theme_void() +
  ggsave("rittenhouse_daylight.png", height = 8, width = 8, dpi = 300)

unzip("26902E236614N.zip")

whitebox::wbt_lidar_tin_gridding(here::here("26902E236614N.las"),
                                 output = here::here("logan.tif"), minz = 0,
                                 resolution = 1, exclude_cls = '3,4,5,7,13,14,15,16,18')

phillyraster = raster::raster("logan.tif")


building_mat = raster_to_matrix(phillyraster)
building_mat_small = reduce_matrix_size(building_mat, 0.5)

temptime = philly_time_start
philly_existing_shadows = list()
sunangles = list()
counter = 1

while(temptime < philly_time_end) {
  sunangles[[counter]] = suncalc::getSunlightPosition(date = temptime, lat = 39.9526, lon = -75.1652)[4:5]*180/pi
  print(temptime)
  philly_existing_shadows[[counter]] = ray_shade(building_mat_small,
                                                 sunangle = sunangles[[counter]]$azimuth+180,
                                                 sunaltitude = sunangles[[counter]]$altitude,
                                                 lambert = FALSE, zscale = 2,
                                                 multicore = TRUE)
  temptime = temptime + duration("3600s")
  counter = counter + 1
}

philly_existing_shadows[[2]] %>% 
  reshape2::melt(varnames = c("x","y"), value.name = "light") %>%
  ggplot() + 
  geom_raster(aes(x = rev(x), y = rev(y), fill = light)) +
  scico::scale_fill_scico(palette = 'grayC', direction = -1, guide = 'none') +
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  coord_fixed() + 
  theme_void() +
  ggsave("logan_shadow.png", height = 8, width = 8, dpi = 300)

shadow_coverage = Reduce(`+`, philly_existing_shadows)/length(philly_existing_shadows)

shadow_coverage %>%
  reshape2::melt(varnames = c("x","y"), value.name = "light") %>%
  ggplot() + 
  geom_raster(aes(x = rev(x), y = rev(y), fill = light)) +
  scico::scale_fill_scico(palette = 'hawaii', name = "Daily Light\nCoverage", guide = 'none') + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  coord_fixed() + 
  theme_void() +
  ggsave("logan_daylight.png", height = 8, width = 8, dpi = 300)
###############################
## Area Characteristics
###############################

# packages
library(tidyverse)
library(sf)
library(lwgeom)
library(stars)
library(tmap)
library(tmaptools)
library(exactextractr)
library(glue)
library(tigris)

## set the state
geography <- "24"

## block groups
blocks <- 
  block_groups(state = geography, cb = TRUE, class = 'sf') %>% 
  st_transform(3857) %>% 
  transmute(GEOID, 
            area_total = units::set_units(st_area(geometry), km^2))

plot(st_geometry(blocks))

## protected lands
protected <- 
  read_sf("data/PADUS2_0MD_Shapefile/PADUS2_0Combined_DOD_Fee_Designation_Easement_MD.shp") %>%
  st_transform(3857) %>% 
  st_union() %>% 
  st_combine()

plot(st_geometry(protected))

test <- 
  blocks %>% 
  st_difference(protected) %>%  
  mutate(area_free = units::set_units(st_area(geometry), km^2)) %>% 
  st_drop_geometry()

test %>% 
  transmute(GEOID,
            area_total_km = units::drop_units(area_total),
            area_free_km = units::drop_units(area_free),
            area_protected_km = area_total_km - area_free_km,
            area_protected_pct = area_protected_km / area_total_km,
            area_free_pct = area_free_km / area_total_km) %>% 
  write_csv("protected_lands.csv")

## other remote sensing
sensing_1 <- raster::stack("data/predictors_md-2.tif")
sensing_2 <- raster::stack("data/predictors_md.tif")

blocks_1 <- blocks
blocks_2 <- blocks

pixels_1 <- raster::reclassify(sensing_1$landcover, matrix(c(0, 100, 1), ncol = 3, byrow =TRUE))
pixels_2 <- raster::reclassify(sensing_2$landcover, matrix(c(0, 100, 1), ncol = 3, byrow =TRUE))

blocks_1$pixels <- exact_extract(pixels_1, blocks, 'sum')
blocks_2$pixels <- exact_extract(pixels_2, blocks, 'sum')

blocks_1$ndvi_mean <- exact_extract(sensing_1$NDVI, blocks, 'mean')
blocks_2$ndvi_mean <- exact_extract(sensing_2$NDVI, blocks, 'mean')

blocks_1$ndvi_sum <- exact_extract(sensing_1$NDVI, blocks, 'sum')
blocks_2$ndvi_sum <- exact_extract(sensing_2$NDVI, blocks, 'sum')

blocks_1$impervious_mean <- exact_extract(sensing_1$impervious, blocks, 'mean')
blocks_2$impervious_mean <- exact_extract(sensing_2$impervious, blocks, 'mean')

resensing_1 <- raster::reclassify(sensing_1$impervious, matrix(c(0, 50, 0,  50, 100, 1), ncol = 3, byrow =TRUE))
resensing_2 <- raster::reclassify(sensing_2$impervious, matrix(c(0, 50, 0,  50, 100, 1), ncol = 3, byrow =TRUE))

blocks_1$impervious_count <- exact_extract(resensing_1, blocks, 'sum')
blocks_2$impervious_count <- exact_extract(resensing_2, blocks, 'sum')

blocks_1$slope_mean <- exact_extract(sensing_1$slope, blocks, 'mean')
blocks_2$slope_mean <- exact_extract(sensing_2$slope, blocks, 'mean')

blocks_1$slope_sum <- exact_extract(sensing_1$slope, blocks, 'sum')
blocks_2$slope_sum <- exact_extract(sensing_2$slope, blocks, 'sum')

resensing_1 <- raster::reclassify(sensing_1$slope, matrix(c(0, 20, 0,  20, 100, 1), ncol = 3, byrow =TRUE))
resensing_2 <- raster::reclassify(sensing_2$slope, matrix(c(0, 20, 0,  20, 100, 1), ncol = 3, byrow =TRUE))

blocks_1$slope_count <- exact_extract(resensing_1, blocks, 'sum')
blocks_2$slope_count <- exact_extract(resensing_2, blocks, 'sum')

total_dev_1 <- raster::reclassify(sensing_1$landcover, matrix(c(10, 20, 0,
                                                                20, 21, 1, 
                                                                21, 22, 2, 
                                                                22, 23, 3,
                                                                23, 24, 4,
                                                                30, 95, 0), 
                                                              ncol = 3, byrow =TRUE))

total_dev_2 <- raster::reclassify(sensing_2$landcover, matrix(c(10, 20, 0,
                                                                20, 21, 1, 
                                                                21, 22, 2, 
                                                                22, 23, 3,
                                                                23, 24, 4,
                                                                30, 95, 0), 
                                                              ncol = 3, byrow =TRUE))

blocks_1$total_development <- exact_extract(total_dev_1, blocks, 'sum')
blocks_2$total_development <- exact_extract(total_dev_2, blocks, 'sum')

resensing_1 <- raster::reclassify(sensing_1$landcover, matrix(c(0, 23, 0, 23, 24, 1, 30, 100, 0), ncol = 3, byrow =TRUE))
resensing_2 <- raster::reclassify(sensing_2$landcover, matrix(c(0, 23, 0, 23, 24, 1, 30, 100, 0), ncol = 3, byrow =TRUE))

blocks_1$high_development <- exact_extract(resensing_1, blocks, 'sum')
blocks_2$high_development <- exact_extract(resensing_2, blocks, 'sum')

resensing_1 <- raster::reclassify(sensing_1$landcover, matrix(c(0, 22, 0, 22, 23, 1, 23, 100, 0), ncol = 3, byrow =TRUE))
resensing_2 <- raster::reclassify(sensing_2$landcover, matrix(c(0, 22, 0, 22, 23, 1, 23, 100, 0), ncol = 3, byrow =TRUE))

blocks_1$medium_development <- exact_extract(resensing_1, blocks, 'sum')
blocks_2$medium_development <- exact_extract(resensing_2, blocks, 'sum')

resensing_1 <- raster::reclassify(sensing_1$landcover, matrix(c(0, 21, 0, 21, 22, 1, 22, 100, 0), ncol = 3, byrow =TRUE))
resensing_2 <- raster::reclassify(sensing_2$landcover, matrix(c(0, 21, 0, 21, 22, 1, 22, 100, 0), ncol = 3, byrow =TRUE))

blocks_1$low_development <- exact_extract(resensing_1, blocks, 'sum')
blocks_2$low_development <- exact_extract(resensing_2, blocks, 'sum')

## historic
total_dev_1 <- raster::reclassify(sensing_1$landcover_1, matrix(c(10, 20, 0,
                                                                  20, 21, 1, 
                                                                  21, 22, 2, 
                                                                  22, 23, 3,
                                                                  23, 24, 4,
                                                                  30, 95, 0), 
                                                                ncol = 3, byrow =TRUE))

total_dev_2 <- raster::reclassify(sensing_2$landcover_1, matrix(c(10, 20, 0,
                                                                  20, 21, 1, 
                                                                  21, 22, 2, 
                                                                  22, 23, 3,
                                                                  23, 24, 4,
                                                                  30, 95, 0), 
                                                                ncol = 3, byrow =TRUE))

resensing_1 <- raster::reclassify(sensing_1$impervious_1, matrix(c(0, 50, 0,  50, 100, 1), ncol = 3, byrow =TRUE))
resensing_2 <- raster::reclassify(sensing_2$impervious_1, matrix(c(0, 50, 0,  50, 100, 1), ncol = 3, byrow =TRUE))

blocks_1$historic_development <- exact_extract(total_dev_1, blocks, 'sum')
blocks_2$historic_development <- exact_extract(total_dev_2, blocks, 'sum')

blocks_1$historic_impervious <- exact_extract(resensing_1, blocks, 'sum')
blocks_2$historic_impervious <- exact_extract(resensing_2, blocks, 'sum')

## aggregating
blocks_complete <- 
  rbind(blocks_1[!is.nan(blocks_1$impervious_mean), ],
        blocks_2[!is.nan(blocks_2$impervious_mean), ]) %>% 
  st_as_sf() %>% 
  select(-area_total) %>% 
  mutate(total_development_norm = total_development / pixels,
         total_development_norm_historic = historic_development / pixels,
         high_development_norm = high_development / pixels,
         medium_development_norm = medium_development / pixels,
         low_development_norm = low_development / pixels) %>% 
  mutate(slope_sum_norm = slope_sum / pixels,
         slope_count_norm = slope_count / pixels) %>% 
  mutate(impervious_count_norm = impervious_count / pixels,
         impervious_count_norm_historic = historic_impervious / pixels,
         ndvi_sum_norm = ndvi_sum / pixels) %>%
  mutate(development_change = (total_development_norm - total_development_norm_historic) / total_development_norm_historic,
         impervious_change = (impervious_count_norm - impervious_count_norm_historic) / impervious_count_norm_historic) %>%
  glimpse() 

blocks_complete %>% 
  st_drop_geometry() %>% 
  write_csv("remote_sensing.csv")




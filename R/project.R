########################################
## Wrapping datelines and other fun
########################################

## packages
library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(lwgeom)

## grab all coordinate reference systems
reference <- 
  crsuggest::crs_sf %>% 
  st_transform("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")

## grab coastlines
coastline <- 
  rnaturalearth::ne_coastline(returnclass = 'sf') %>% 
  rmapshaper::ms_simplify(0.09) %>%
  filter(!st_is_empty(geometry)) %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>% 
  st_transform("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")

## extract bounding boxes of each crs
bounds <-
  map_dfr(1:nrow(reference), function(x){ 

    s1 <- reference$geometry[[x]]
    s2 <- st_bbox(s1)
    s3 <- st_as_sfc(s2)
    s4 <- st_sf(s3)
    
    return(s4)
    
    }) %>% 
  rename(geometry = s3) %>% 
  st_set_crs("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")

## map it all
map <- 
  tm_shape(bounds) +
  tm_borders(col = '#000000', lwd = 0.1, alpha = 0.1) +
  tm_shape(coastline) +
  tm_lines(col = '#000000', lwd = 1, lty = 2) +
  tm_layout(main.title = "COORDINATE REFERENCE SYSTEMS",
            main.title.fontface = 'bold',
            frame.lwd = 0)

tmap_save(map, filename = "references.png", height = 8, dpi = 300) 

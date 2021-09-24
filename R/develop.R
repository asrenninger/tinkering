################################################
## How much land in New Jersey can be developed?
################################################

## packages
library(tidyverse)
library(sf)
library(tigris)
library(tmap)

## criteria
pad <- st_read("~/Downloads/PADUS2_1_Geopackage/PADUS2_1_Geopackage.gpkg", 
               layer = "PADUS2_1Combined_Fee_Designation_Easement")

sewer <- st_read("~/Downloads/Statewide_Sewer_Service_Area_for_New_Jersey/Statewide_Sewer_Service_Area_for_New_Jersey.shp")

pinelands <- st_read("~/Downloads/PinelandsOutline/PinelandsOutline.shp")
highlands <- st_read("~/Downloads/HighlandsOutline/HighlandsOutline.shp")

## state boudanries
blocks <- block_groups(state = "NJ", cb = TRUE, class = 'sf')

boundary <-
  blocks %>% 
  st_union() %>%
  st_combine()

## areas 
state_area <- st_area(boundary) 
units::set_units(state_area, mi^2)

protected <- 
  pad %>%
  st_transform(st_crs(boundary)) %>% 
  st_intersection(boundary) %>% 
  st_union() %>%
  st_combine()

protected_area <- st_area(protected)
units::set_units(protected_area, mi^2)

connected <-
  sewer %>% 
  st_transform(st_crs(boundary)) %>% 
  st_union() %>%
  st_combine()

connected_area <- st_area(connected)
units::set_units(connected_area, mi^2)

pinelands <- 
  st_read("~/Downloads/PinelandsOutline/PinelandsOutline.shp") %>% 
  st_transform(st_crs(boundary)) %>% 
  st_union() %>% 
  st_combine()

highlands <-
  st_read("~/Downloads/HighlandsOutline/HighlandsOutline.shp") %>%
  st_transform(st_crs(boundary)) %>% 
  st_union() %>% 
  st_combine()

difference <- 
  boundary %>% 
  st_difference(connected)

remove_lands <- 
  boundary %>%
  st_difference(pinelands) %>% 
  st_difference(highlands)

remove_area <- st_area(remove_lands)
units::set_units(remove_area, mi^2)

remove_lands <- 
  boundary %>%
  st_difference(pinelands) %>% 
  st_difference(highlands) %>%
  st_difference(protected) 

remove_area <- st_area(remove_lands)
units::set_units(remove_area, mi^2)

remove_lands <- 
  boundary %>%
  st_difference(pinelands) %>% 
  st_difference(highlands) %>%
  st_difference(protected) %>% 
  st_intersection(connected)

remove_area <- st_area(remove_lands)
units::set_units(remove_area, mi^2)

difference_area <- st_area(difference)
units::set_units(difference_area, mi^2)

intersection <-
  difference %>%
  st_intersection(protected)

intersection_area <- st_area(intersection)
units::set_units(intersection_area, ha)

## map it
tmap_save(tm_shape(boundary) +
            tm_borders(col = '#000000', alpha = 0.5, lty = 2, lwd = 0.5) + 
            tm_shape(connected) +
            tm_fill(col = '#773355', alpha = 0.75),
          filename = "test.png")
  

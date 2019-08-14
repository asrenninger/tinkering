library(tidyverse)
library(magrittr)
library(osmdata)

##

library(tmap)
library(sf)

##

phl <- getbb("Philadelphia, PA")

parks <- 
  rbind(opq(bbox = phl) %>%
          add_osm_feature(key = "leisure", value = "park") %>%
          osmdata_sf() %>%
          use_series("osm_multipolygons") %>%
          as_tibble() %>%
          select(name, geometry) %>%
          st_as_sf() %>%
          st_cast('POLYGON'),
        opq(bbox = phl) %>%
          add_osm_feature(key = "leisure", value = "park") %>%
          osmdata_sf() %>%
          use_series("osm_polygons") %>%
          as_tibble() %>%
          select(name, geometry) %>%
          st_as_sf()) %>%
  st_difference() %>%
  mutate(area = as.numeric(st_area(geometry))) %>%
  filter(area > 0)

##

library(ineq)

##

ineq(parks$area, type = "Gini")

##

cities <- c("New York, NY", "Philadelphia, PA", "Washington, DC", "Boston, MA")

spatial_data <- tibble()

for (i in 1:length(cities)) {
  
  where <- cities[i]
  boxed <- getbb(where)
  
  parks <- 
    rbind(opq(bbox = boxed) %>%
            add_osm_feature(key = "leisure", value = "park") %>%
            osmdata_sf() %>%
            use_series("osm_multipolygons") %>%
            as_tibble() %>%
            select(name, geometry) %>%
            st_as_sf() %>%
            st_cast('POLYGON'),
          opq(bbox = boxed) %>%
            add_osm_feature(key = "leisure", value = "park") %>%
            osmdata_sf() %>%
            use_series("osm_polygons") %>%
            as_tibble() %>%
            select(name, geometry) %>%
            st_as_sf()) %>%
    lwgeom::st_make_valid() %>%
    st_difference() %>%
    mutate(area = as.numeric(st_area(geometry))) %>%
    filter(area > 0) %>%
    as_tibble() %>%
    filter(name != "Chesapeake & Ohio Canal National Historical Park")
  
  corado <- ineq(parks$area, type = "Gini")
  
  parks <- mutate(parks, city = where, gini = corado)
  
  spatial_data <- bind_rows(spatial_data, parks)
  
}

##

library(glue)

##

spatial_data_sf <- 
  spatial_data %>%
  st_as_sf() %>%
  mutate(title = glue("{city} ({round(gini, 4)})"))

##

library(RColorBrewer)

brewer.pal(10, "BuGn")

##

library(magick)

##

fig <- image_graph(width = 800, height = 800, res = 300)

##

tm_shape(spatial_data_sf) +
  tm_fill("area", thres.poly = 0,
          palette = brewer.pal(11, "YlGn")[3:11], 
          style = 'fisher') +
  tm_facets("title", free.coords = TRUE, drop.units = TRUE,
            nrow = 2, ncol = 2, drop.empty.facets = TRUE) +
  tm_layout(legend.show = FALSE, 
            panel.show = TRUE,
            panel.label.bg.color = '#d8b365',
            panel.label.color = '#FAFBFB',
            panel.label.size = 0.5,
            fontface = 'bold', frame = FALSE,
            title.position = c("center", "center"), title.size = 5)

##

dev.off()

##

image_write(fig, "test.png")

##


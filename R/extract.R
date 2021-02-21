########################################
## Various voronois with osmextract
########################################

## packages 
library(osmextract)
library(sf)
library(tidyverse)
library(glue)

## simpler to use natural earch to get country boundaries
bounds <- 
  bind_rows(rnaturalearth::ne_countries(scale = 50, returnclass = 'sf') %>% 
              filter(str_detect(name_long, "United Kingdom")) %>%
              st_transform(4326), 
            rnaturalearth::ne_countries(scale = 50, returnclass = 'sf') %>% 
              filter(str_detect(name_long, "Ireland")) %>%
              st_transform(4326)) %>%
  st_as_sf() %>%
  st_union() %>% 
  st_combine()

## get and query the osm pbf file for the British Isles
britain <- 
  oe_get(
  "Britain and Ireland",
  layer = "points",
  quiet = FALSE,
  query = "SELECT geometry FROM 'points' WHERE hstore_get_value(other_tags, 'amenity') = 'fuel'"
)

## coordinates for the point pattern 
coordinates <- 
  britain
  st_coordinates() %>% 
  as_tibble()

## create point pattern
pointp <- spatstat::ppp(coordinates$X, coordinates$Y, window = spatstat::owin(xrange = c(st_bbox(bounds)$xmin, 
                                                                                         st_bbox(bounds)$xmax),
                                                                              yrange = c(st_bbox(bounds)$ymin, 
                                                                                         st_bbox(bounds)$ymax)))

## create the voronoi polygons and then plot on a black background
stations <- 
  ggplot(data = spatstat::dirichlet(pointp) %>% 
         st_as_sfc() %>%
         st_set_crs(4326) %>%
         st_intersection(bounds) %>%
         as_tibble() %>% 
         mutate(area = st_area(geometry)) %>%
         select(area, geometry) %>%
         st_as_sf()) +
  geom_sf(aes(), colour = '#ffffff', fill = NA, lwd = 0.05, show.legend = FALSE) + 
  labs(title = "Petrol Stations") + 
  coord_sf(crs = 27700) + 
  theme_black()

## repeat with restaurants
britain <- 
  oe_get(
    "Britain and Ireland",
    layer = "points",
    quiet = FALSE,
    query = "SELECT geometry FROM 'points' WHERE hstore_get_value(other_tags, 'amenity') = 'restaurant'"
  )

coordinates <- 
  britain %>%
  st_coordinates() %>% 
  as_tibble()

pointp <- spatstat::ppp(coordinates$X, coordinates$Y, window = spatstat::owin(xrange = c(st_bbox(bounds)$xmin, 
                                                                                         st_bbox(bounds)$xmax),
                                                                              yrange = c(st_bbox(bounds)$ymin, 
                                                                                         st_bbox(bounds)$ymax)))

restaurants <- 
  ggplot(data = spatstat::dirichlet(pointp) %>% 
           st_as_sfc() %>%
           st_set_crs(4326) %>%
           st_intersection(bounds) %>%
           as_tibble() %>% 
           mutate(area = st_area(geometry)) %>%
           select(area, geometry) %>%
           st_as_sf()) +
  geom_sf(aes(), colour = '#ffffff', fill = NA, lwd = 0.05, show.legend = FALSE) + 
  labs(title = "Restaurants") + 
  coord_sf(crs = 27700) + 
  theme_black()

## repeat with pubs
britain <- 
  oe_get(
    "Britain and Ireland",
    layer = "points",
    quiet = FALSE,
    query = "SELECT geometry FROM 'points' WHERE hstore_get_value(other_tags, 'amenity') = 'pub'"
  )

coordinates <- 
  britain %>%
  st_coordinates() %>% 
  as_tibble()

pointp <- spatstat::ppp(coordinates$X, coordinates$Y, window = spatstat::owin(xrange = c(st_bbox(bounds)$xmin, 
                                                                                         st_bbox(bounds)$xmax),
                                                                              yrange = c(st_bbox(bounds)$ymin, 
                                                                                         st_bbox(bounds)$ymax)))

pubs <- 
  ggplot(data = spatstat::dirichlet(pointp) %>% 
           st_as_sfc() %>%
           st_set_crs(4326) %>%
           st_intersection(bounds) %>%
           as_tibble() %>% 
           mutate(area = st_area(geometry)) %>%
           select(area, geometry) %>%
           st_as_sf()) +
  geom_sf(aes(), colour = '#ffffff', fill = NA, lwd = 0.05, show.legend = FALSE) + 
  labs(title = "Pubs") + 
  coord_sf(crs = 27700) + 
  theme_black()

## repeat with schools
britain <- 
  oe_get(
    "Britain and Ireland",
    layer = "points",
    quiet = FALSE,
    query = "SELECT geometry FROM 'points' WHERE hstore_get_value(other_tags, 'amenity') = 'school'"
  )

coordinates <- 
  britain %>%
  st_coordinates() %>% 
  as_tibble()

pointp <- spatstat::ppp(coordinates$X, coordinates$Y, window = spatstat::owin(xrange = c(st_bbox(bounds)$xmin, 
                                                                                         st_bbox(bounds)$xmax),
                                                                              yrange = c(st_bbox(bounds)$ymin, 
                                                                                         st_bbox(bounds)$ymax)))

schools <- 
  ggplot(data = spatstat::dirichlet(pointp) %>% 
           st_as_sfc() %>%
           st_set_crs(4326) %>%
           st_intersection(bounds) %>%
           as_tibble() %>% 
           mutate(area = st_area(geometry)) %>%
           select(area, geometry) %>%
           st_as_sf()) +
  geom_sf(aes(), colour = '#ffffff', fill = NA, lwd = 0.05, show.legend = FALSE) + 
  labs(title = "Schools") + 
  coord_sf(crs = 27700) + 
  theme_black()

## patch it all together
library(patchwork)
patched <- (stations + pubs) / (restaurants + schools)

## save it
ggsave(patched, filename = "voronois.png", height = 11, width = 8, dpi = 300)

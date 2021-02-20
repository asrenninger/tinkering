library(osmextract)
library(sf)
library(tidyverse)

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

ireland <- 
  oe_get(
  "Republic of Ireland",
  layer = "points",
  quiet = FALSE,
  query = "SELECT geometry FROM 'points' WHERE hstore_get_value(other_tags, 'amenity') = 'fuel'"
)

kingdom <- 
  oe_get(
    "United Kingdom",
    layer = "points",
    quiet = FALSE,
    query = "SELECT geometry FROM 'points' WHERE hstore_get_value(other_tags, 'amenity') = 'fuel'"
  )

coordinates <- 
  ireland %>%
  filter(!geometry %in% kingdom$geometry) %>%
  bind_rows(kingdom) %>%
  st_coordinates() %>% 
  as_tibble()

pointp <- spatstat::ppp(coordinates$X, coordinates$Y, window = spatstat::owin(xrange = c(st_bbox(bounds)$xmin, 
                                                                                         st_bbox(bounds)$xmax),
                                                                              yrange = c(st_bbox(bounds)$ymin, 
                                                                                         st_bbox(bounds)$ymax)))

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
  theme_black() +
  ggsave("petroleonoi.png", height = 8, width = 8, dpi = 300)

ireland <- 
  oe_get(
    "Republica of Ireland",
    layer = "points",
    quiet = FALSE,
    query = "SELECT geometry FROM 'points' WHERE hstore_get_value(other_tags, 'amenity') = 'restaurant'"
  )

kingdom <- 
  oe_get(
    "United Kingdom",
    layer = "points",
    quiet = FALSE,
    query = "SELECT geometry FROM 'points' WHERE hstore_get_value(other_tags, 'amenity') = 'restaurant'"
  )

coordinates <- 
  ireland %>%
  filter(!geometry %in% kingdom$geometry) %>%
  bind_rows(kingdom) %>%
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
  theme_black() +
  ggsave("restauronoi.png", height = 8, width = 8, dpi = 300)

ireland <- 
  oe_get(
    "Republic of Ireland",
    layer = "points",
    quiet = FALSE,
    query = "SELECT geometry FROM 'points' WHERE hstore_get_value(other_tags, 'amenity') = 'pub'"
  )

kingdom <- 
  oe_get(
    "United Kingdom",
    layer = "points",
    quiet = FALSE,
    query = "SELECT geometry FROM 'points' WHERE hstore_get_value(other_tags, 'amenity') = 'pub'"
  )

coordinates <- 
  ireland %>%
  filter(!geometry %in% kingdom$geometry) %>%
  bind_rows(kingdom) %>%
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
  theme_black() +
  ggsave("pubonoi.png", height = 8, width = 8, dpi = 300)

ireland <- 
  oe_get(
    "Republic of Ireland",
    layer = "points",
    quiet = FALSE,
    query = "SELECT geometry FROM 'points' WHERE hstore_get_value(other_tags, 'amenity') = 'school'"
  )

kingdom <- 
  oe_get(
    "United Kingdom",
    layer = "points",
    quiet = FALSE,
    query = "SELECT geometry FROM 'points' WHERE hstore_get_value(other_tags, 'amenity') = 'school'"
  )

coordinates <- 
  ireland %>%
  filter(!geometry %in% kingdom$geometry) %>%
  bind_rows(kingdom) %>%
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
  theme_black() +
  ggsave("educationonoi.png", height = 8, width = 8, dpi = 300)

library(patchwork)
patched <- (stations + pubs) / (restaurants + schools)

ggsave(patched, filename = "voronois.png", height = 11, width = 8, dpi = 300)

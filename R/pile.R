########################################
## Comparing geometries
########################################

## packages 
library(tidyverse)
library(sf)
library(tigris)

## get the data
contiguous <- unique(tigris::fips_codes$state_code)[1:51][-c(2, 12)]
counties <- 
  tigris::counties(cb = TRUE, resolution = "20m", class = 'sf') %>% 
  filter(STATEFP %in% contiguous) %>% 
  st_transform(2163)

## subtract the centroid, to pile them all up
counties <- st_geometry(st_transform(counties, 2163))
counties <- counties - st_centroid(counties)

## plot it
ggplot(counties) + 
  geom_sf(aes(), fill = '#000000', colour = NA, lwd = 0, alpha = 0.1) + 
  theme_void() +
  ggsave("counties.png", height = 8, width = 8, dpi = 300)

## break it down by state
state_centroids <- 
  counties %>% 
  mutate(centroid = st_centroid(geometry)) %>% 
  mutate(as_tibble(st_coordinates(centroid))) %>% 
  st_drop_geometry() %>%
  group_by(STATEFP) %>%
  summarise(X = mean(X),
            Y = mean(Y)) %>%
  ungroup() %>% 
  st_as_sf(coords = c("X", "Y"), crs = 2163) %>% 
  rename(centroid = geometry) %>% 
  as_tibble()

## plot it
counties %>% 
  left_join(state_centroids) %>% 
  mutate(geometry = (geometry - st_centroid(geometry)) + centroid) %>% 
  st_geometry() %>% 
  st_set_crs(2163) %>%
  ggplot() + 
  geom_sf(aes(), fill = '#000000', colour = NA, lwd = 0, alpha = 0.1) + 
  geom_sf(data = 
            states(cb = TRUE, class = 'sf') %>% 
            filter(STATEFP %in% contiguous) %>% 
            st_transform(2163), 
          aes(), lty = 2, size = 0.25, fill = NA, colour = '#000000') + 
  theme_void() +
  ggsave("countiesxstate.png", height = 8, width = 8, dpi = 300)
####################################
## Chrystaller
####################################

library(tidyverse)
library(sf)

## central place theory
rotate <- function(a) { matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2) }

square <- st_polygon(list(rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))))
grid <- st_make_grid(square, n = c(25, 25), square = FALSE)

neighbours <- 
  grid %>% 
  st_touches(sparse = TRUE) %>% 
  map(~length(.x)) %>% 
  reduce(c)

trimmed <- grid[neighbours > 2]
rotated <- (trimmed * rotate(pi/2))

coordinates <- rotated %>% st_set_precision(0.5) %>% st_centroid %>% st_coordinates() %>% as_tibble() 

trimmed <- rotated[coordinates$X != max(coordinates$X)]
tibbled <- trimmed %>% st_as_sf() %>% rownames_to_column(var = "geocode") %>% rename(geometry = x) %>% st_centroid()

tween <- 
  trimmed %>% 
  st_touches() %>% 
  as.data.frame() %>% 
  graph_from_data_frame() %>%
  betweenness() %>%
  as_tibble() %>%
  rownames_to_column(var = "row_id") %>%
  mutate(row_id = as.numeric(row_id))

trimmed %>% 
  st_touches() %>% 
  as.data.frame() %>% 
  clean_names() %>%
  glimpse() %>% 
  stplanr::od2line(., tibbled) %>% 
  left_join(tween) %>%
  select(value) %>%
  plot(add = TRUE)

edges <- 
  trimmed %>%
  st_cast("LINESTRING") %>%
  lwgeom::st_split(st_cast(trimmed, "POINT")) %>%
  st_collection_extract("LINESTRING")

tween_edges <- 
  edges %>% 
  st_touches() %>% 
  as.data.frame() %>% 
  graph_from_data_frame() %>%
  closeness() %>%
  as_tibble() %>%
  rownames_to_column(var = "row_id") %>%
  mutate(row_id = as.numeric(row_id))

ggplot() +
  geom_sf(data = edges %>%
            as_tibble() %>% 
            rownames_to_column(var = "row_id") %>%
            mutate(row_id = as.numeric(row_id)) %>%
            glimpse() %>% 
            left_join(tween_edges) %>%
            st_as_sf() %>%
            select(value), 
          aes(colour = value, lwd = value),
          show.legend = FALSE) +
  scale_colour_gradientn(colors = pal) + 
  scale_size_continuous(range = c(0.1, 1)) +
  theme_map() +
  ggsave("tessellation.png", height = 8, width = 8, dpi = 300)

ggplot() +
  geom_sf(data = trimmed, 
          aes(),
          fill = NA) + 
  geom_sf(data = trimmed %>% 
            st_touches() %>% 
            as.data.frame() %>% 
            clean_names() %>%
            glimpse() %>% 
            stplanr::od2line(., tibbled) %>% 
            left_join(tween) %>%
            select(value), 
          aes(colour = value, lwd = value), 
          show.legend = FALSE) +
  scale_colour_gradientn(colors = pal) + 
  scale_size_continuous(range = c(0.1, 1)) +
  theme_map() + 
  ggsave("chrystaller.png", height = 8, width = 8, dpi = 300)
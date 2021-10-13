## get populations
population <- 
  rnaturalearth::ne_countries(scale = 110, returnclass = 'sf') %>%
  mutate(area = units::drop_units(units::set_units(st_area(geometry), km^2))) %>% 
  st_drop_geometry() %>%
  arrange(desc(pop_est)) %>% 
  slice(1:9) %>% 
  transmute(name, density = round(pop_est / area)) %>% 
  arrange(desc(density)) %>% 
  rownames_to_column()

## make a grid
samples <- 
  rnaturalearth::ne_countries(scale = 110, returnclass = 'sf') %>% 
  st_transform("+proj=robin") %>%
  st_make_grid(cellsize =  10, n = c(3, 3)) %>%
  as_tibble() %>% 
  rownames_to_column()

## sample points to represent density
points <- purrr::map_df(1:9, ~as_tibble(st_union(st_sample(samples$geometry[.x], population$density[.x]))))

## points
xy_coordinates <- 
  samples %>% 
  mutate(points = points$geometry) %>% 
  select(-geometry) %>% 
  st_as_sf() %>% 
  select(-rowname) 

## borders
areal_units <- st_as_sf(samples)

## combined
plot(st_join(xy_coordinates, areal_units))

## plot it
map <- tm_shape(areal_units %>% 
           mutate(name = population$name)) +
  tm_borders() +
  tm_shape(areal_units %>% 
             mutate(name = population$name) %>% 
             st_centroid() %>% 
             as.data.frame() %>% 
             st_as_sf()) +
  tm_text("name",
          size = 1.5,
          fontface = 'bold',
          shadow = TRUE) +
  tm_shape(xy_coordinates %>% 
             st_join(areal_units) %>% 
             left_join(population) %>% 
             as.data.frame() %>% 
             st_as_sf()) + 
  tm_dots() +
  tm_layout(frame = FALSE)

tmap_save(map, filename = "densitogram.png", height = 8)

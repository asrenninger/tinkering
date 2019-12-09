library(tidyverse)
library(fs)

##

files <- dir_ls("data/strava")
length(files)

##

library(sf)

##

possible_bind <- possibly(rbind, otherwise = NULL) 

##

running_data <- 
  reduce(
    map(files, function(x) {
      st_read(x,layer = "track_points") %>%
        transmute(track_id = x,
                  track_seg_point_id = track_seg_point_id,
                  ele = ele,
                  time = time)
    }), 
    rbind
  )

##

glimpse(running_data)

##

st_write(running_data, "runs.geojson")

##

library(lubridate)

##

bounds <- 
  running_data %>%
  mutate(day = date(time)) %>%
  filter(day > as_date("2019-11-23")) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_buffer(0.02) %>%
  st_as_sf() %>%
  mutate(bounds = 1)

running_data %>%
  mutate(day = date(time)) %>%
  filter(day > as_date("2019-01-01")) %>%
  st_intersection(bounds) %>%
  drop_na(bounds) %>%
  ggplot() +
  geom_sf(aes(colour = ele), show.legend = FALSE) +
  scale_color_gradientn(colours = topo.colors(10)) +
  facet_wrap(~ day) +
  theme_void() +
  ggsave("test.png", height = 20, width = 20, dpi = 300)

##

running_data <- st_read("data/runs.geojson")

##

coords <- 
  running_data %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(running_data) %>%
  st_as_sf()

##

p <- 
  ggplot(coords %>%
           mutate(day = date(time)) %>%
           filter(day > as_date("2019-01-01")), 
         aes(X, Y, group = track_id)) +
  geom_path() +
  facet_wrap(~ day, scales = 'free') +
  theme_void() +
  ggsave("test.png", height = 30, width = 30, dpi = 300)

##

library(rnaturalearth)

##

cities <- 
  ne_download(scale = 10, 
              type = "populated_places", category = "cultural",
              returnclass = 'sf') %>%
  select(name_en)

plot(cities)

##

library(RANN)

##

neighbours <-
  nn2(st_coordinates(cities), st_coordinates(running_data), 
      k = 1, searchtype = "radius", radius = 10) %>%
  as.data.frame() %>%
  set_names(c("index", "distance"))

running_associated <-
  coords %>%
  bind_cols(neighbours) %>%
  st_as_sf()

running_associated <- 
  cities %>%
  rownames_to_column(var = "index") %>%
  mutate(index = as.numeric(index)) %>%
  st_drop_geometry() %>%
  right_join(running_associated)

running_associated <-
  running_associated %>%
  rename(city = name_en) %>%
  mutate(day = date(time)) %>%
  select(track_id, track_seg_point_id, city, X, Y, day, time, everything()) %>%
  st_as_sf()

##

library(gganimate)

##

anim_data <-
  running_associated %>%
  filter(city != "Ithaca") %>%
  filter(day > as_date("2019-01-01")) %>%
  mutate(marathon = if_else(day > as_date("2019-11-23"), 1, 0)) 
 
unique(anim_data$city)

##

anim_data %>%
  group_by(city) %>%
  mutate(geometry = st_normalize(geometry),
         ele = ele / max(ele)) %>%
  st_set_crs(4326) %>%
  st_transform(3395) %>%
  ggplot() +
  geom_sf(aes(colour = ele), show.legend = FALSE) +
  facet_wrap(~ city) + 
  coord_sf(crs = 3395) +
  scale_colour_gradientn(colours = topo.colors(10)) +
  theme_void() +
  ggsave("test.png", height = 8, width = 8, dpi = 300) +
  transition_manual(time)

##

anim_save(filename = "run.gif", path = "viz")

##

anim <- 
  ggplot(anim_data, 
       aes(X, Y, group = track_id)) +
  geom_path(aes(colour = factor(marathon), size = marathon, alpha = marathon),
            show.legend = FALSE) +
  facet_wrap(~ city, scales = 'free') +
  scale_size_continuous(range = c(1, 3)) +
  scale_alpha_continuous(range = c(0.5, 1)) +
  scale_colour_manual(values = c('#404040', '#00129c')) +
  theme_void() +
  theme(aspect.ratio = 1) +
  ggsave("projected.png", height = 6, width = 9, dpi = 100)
  transition_reveal(time) +
  ease_aes("cubic-in-out")

animate(anim, fps = 5, start_pause = 0, end_pause = 5,
        height = 600, width = 900)

anim_save(filename = "run.gif", path = "viz")

##

rectangle <- 
  anim_data %>%
  filter(city == "Philadelphia") %>%
  mapedit::editFeatures()

 rectangle %>%
   st_geometry_type() %>%
   as_tibble() %>%
   bind_cols(rectangle) %>%
   filter(value == "POLYGON") %>%
   select(geometry)

bbox <- 
  anim_data %>%
  filter(city == "Philadelphia") %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_buffer(0.001) %>%
  st_as_sf() %>%
  st_bbox()

padding <- (bbox[4] - bbox[2]) / 2

##

for (i in 1:length(unique(anim_data$city))) {
  
  bbox <- 
    anim_data %>%
    filter(city == unique(anim_data$city)[1]) %>%
    st_centroid() %>%
    st_buffer(padding) %>%
    st_bbox()
    
  m <- matrix(c(bbox[1], bbox[3], bbox[2], bbox[4]), 
              ncol = 2,
              byrow = TRUE)
  row.names(m) <- c("x", "y")
  colnames(m) <- c("min", "max")
  
  network <- 
    opq(bbox = m) %>%
    add_osm_feature(key = 'highway') %>%
    osmdata_sf() %>%
    use_series("osm_lines") %>%
    as_tibble() %>%
    select(name, geometry) %>%
    st_as_sf()
  
  networks <- cbind(network, networks)
  
}

plot(network)

##
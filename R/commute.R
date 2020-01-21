
library(geofacet)

##

states <- geofacet::us_state_grid1$code

##

library(lehdr)

##

ordest <- grab_lodes(state = states, year = 2015, lodes_type = "od", job_type = "JT01", 
                     segment = "S000", state_part = "aux", agg_geo = "tract")   

##

library(tidyverse)

##

write_csv(ordest, "lodes_data_aux.csv")

ordest <- read_csv("data/lodes_data_aux.csv")

##

head(ordest)

##

ordest$id <- paste(ordest$w_tract, ordest$h_tract, sep = " to ")

##

library(sf)

##

tracts <- st_read("data/tracts.shp")

##

tracts <- 
  tracts %>% 
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(tracts) %>%
  select(GEOID10, X, Y)

ordest %>%
  filter(state != "AK" & state != "HI") %>%
  group_by(state) %>%
  add_tally() %>%
  sample_n(n / 10) %>%
  ungroup() %>%
  group_by(id) %>%
  gather(location, GEOID10, h_tract:w_tract) %>%
  left_join(tracts) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
  st_transform(102003) %>%
  summarise(jobs = mean(`S000`)) %>%
  st_cast("MULTIPOINT") %>%
  st_cast("LINESTRING") %>%
  mutate(length = st_length(geometry)) %>%
  st_write("lodes_shape_7.shp")


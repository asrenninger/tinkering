########################################
## Mapping commutes in America
########################################

## packages
library(tidyverse)
library(tigris)
library(tidycensus)
library(sf)
library(tmap)
library(tmaptools)
library(fs)
library(glue)
library(vroom)
library(igraph)

## download tracts
us <- unique(fips_codes$state)[1:51]

tracts <-
  reduce(
    map(us, 
        function(x) {
          tracts(state = x, cb = TRUE, class = 'sf')
        }),
    rbind
  )

tracts <-
  tracts %>% 
  filter(!str_detect(STATEFP, "02|15")) %>%
  st_transform(2163) %>% 
  mutate(area = st_area(geometry)) %>%
  transmute(GEOID)

## download states
states <- 
  states(cb = TRUE, class = 'sf') %>%
  filter(!str_detect(STATEFP, "02|15|6[0-9]|7[0-9]")) %>%
  rmapshaper::ms_simplify(0.005) %>%
  select(NAME) %>% 
  st_transform(2163)

## download commutes
interest <-
  states(cb = TRUE, class = 'sf') %>%
  filter(!str_detect(STATEFP, "02|15|6[0-9]|7[0-9]")) %>%
  pull(STUSPS)

ordest_1 <- lehdr::grab_lodes(state = interest, year = 2015, lodes_type = "od", job_type = "JT01", 
                              segment = "S000", state_part = "aux", agg_geo = "tract")  

ordest_2 <- lehdr::grab_lodes(state = interest, year = 2015, lodes_type = "od", job_type = "JT01", 
                              segment = "S000", state_part = "main", agg_geo = "tract")   

## save the data
commutes <-
  bind_rows(ordest_1,
            ordest_2) %>%
  select(w_tract, h_tract, S000) %>%
  filter(h_tract %in% tracts$GEOID, 
         w_tract %in% tracts$GEOID) %>% 
  write_csv("commutes.csv")

## create origin-destination lines
lines <- stplanr::od2line(commutes %>%
                            filter(S000 > 10), 
                          st_centroid(tracts))

## save them
st_write(lines, "commutes_nationwide.gpkg")

## build a graph
graph <- graph_from_data_frame(transmute(commutes,
                                         from = h_tract,
                                         to = w_tract,
                                         weight = S000) %>%
                                 filter(weight > 5), directed = FALSE)

# find communities
graph <- set_vertex_attr(graph, "community", value = cluster_infomap(graph, e.weights = E(graph)$weight)$membership) 

# save it
tibble(GEOID = V(graph)$name, 
       community = V(graph)$community) %>% 
  left_join(tracts) %>% 
  st_as_sf() %>% 
  group_by(community) %>% 
  summarise() %>%
  st_write("communities_nationwide.gpkg")

## carve out acela corridor
acela <- "BAL|BBY|BOS|BWI|MET|NHV|NLC|NYP|NWK|PHL|PVD|STM|TRE|WAS|RTE|WIL"

stations <- 
  read_sf("data/Amtrak_Stations.geojson") %>% 
  filter(str_detect(stncode, acela)) %>% 
  st_transform(2163)

# filter tracts around stations
relevant <- 
  tracts %>% 
  st_join(st_buffer(stations, 10^4), largest = TRUE) %>%
  drop_na(stncode) %>%
  transmute(GEOID, state, station_code = stncode, station_name = stnname)

# filter for intersecting tracts
commutes_acela <-
  bind_rows(ordest_1,
            ordest_2) %>%
  filter(S000 > 5) %>%
  select(w_tract, h_tract, S000) %>%
  filter(h_tract %in% relevant$GEOID | w_tract %in% relevant$GEOID)

# create lines again
lines <- stplanr::od2line(commutes, 
                          st_centroid(tracts))

# save commutes
lines %>% filter(w_tract %in% relevant$GEOID) %>% st_write("full_in.geojson")
lines %>% filter(h_tract %in% relevant$GEOID) %>% st_write("full_out.geojson")

# save map data
stations %>% st_write("stations.geojson")
relevant %>% st_write("relevant.geojson")

# save context
states %>% st_write("states.geojson")


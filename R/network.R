library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)

##

counties <- read_csv("data/colorado.csv") %>% pull(County)

##

counties[8] <- "Broomfield County"
counties[17] <- "Denver County"

##

roads <- 
  reduce(
  map(counties, function(x) {
    roads(x, state = "CO", class = 'sf')
  }), 
  rbind
)

##

st_write(roads, "colorado.shp")

##

glimpse(roads)

##

duplicates <- 
  roads %>% 
  st_equals() %>%
  as_tibble() %>%
  rename(original = row.id,
         duplicate = col.id) %>%
  group_by(original) %>%
  mutate(grouping = max(duplicate)) %>%
  ungroup() %>%
  distinct(grouping)

##

roads %>% slice(duplicates$grouping) %>% st_write("clean.shp")

##

cleaned <- st_read("clean.shp")
  
##

containers <- 
  cleaned %>% 
  st_contains() %>%
  as_tibble() %>%
  rename(original = row.id,
         duplicate = col.id) %>%
  group_by(original) %>%
  mutate(grouping = max(duplicate)) %>%
  ungroup() %>%
  distinct(grouping)
  
##

roads %>% slice(duplicates$grouping) %>% slice(-containers$grouping) %>% st_write("test.shp")

##



  

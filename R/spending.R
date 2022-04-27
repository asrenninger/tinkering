## Mapping congressional spending

## packages
library(ggmap)
library(tigris)
library(sf)
library(tidyverse)
library(tmap)
library(tmaptools)

## load data from https://bipartisanpolicy.org/blog/congressionally-directed-spending-fy2022-dataset/
pork <- 
  readxl::read_xlsx("~/Downloads/FY2022 Congressionally Directed Spending - BPC.xlsx") %>%
  glimpse() 

## register Google API key
google_key <- jsonlite::fromJSON("secrets/google_key.json")[[1]]
register_google(key = google_key)

## geocode
pork_geocoded <- 
  pork %>% 
  left_join(tigris::fips_codes %>%
              select(state, state_name) %>%
              distinct(state, .keep_all = TRUE)) %>%
  mutate(address = glue("{recipient}, {state_name}")) %>%
  mutate_geocode(location = address, source = "google", output = "latlon") 

## background, shifted so Alaska and Hawaii don't ruin everything
states <- 
  states(cb = TRUE, resolution = "20m") %>%
  shift_geometry() %>%
  glimpse()

## map projects 
point_pork <- 
  pork_geocoded %>% 
  mutate(amount = parse_number(amount)) %>%
  drop_na(recipient, lon, lat) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(st_crs(states())) %>%
  shift_geometry() %>%
  st_intersection(states)

## population denominator
capita <- tidycensus::get_decennial(geography = "state", variables = "P001001", year = 2010) 

## map states for geocode misses 
state_pork <- 
  pork_geocoded %>%
  mutate(NAME = state_name,
         amount = parse_number(amount)) %>%
  left_join(states) %>%
  st_as_sf() %>%
  group_by(NAME) %>%
  summarise(total_value = sum(amount, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(capita) %>%
  mutate(value_per_capita = total_value / value)

## plot
tmapped <- 
  tm_shape(transmute(state_pork, "$ per capita (total)" = value_per_capita)) +
  tm_fill(col = "$ per capita (total)") + 
  tm_shape(transmute(point_pork, "$ (individual projects)" = amount)) +
  tm_bubbles(col = '#000000', size = "$ (individual projects)", scale = 3, alpha = 0.5) + 
  tm_shape(states) +
  tm_borders(lty = 2) +
  tm_layout(legend.outside = TRUE, 
            main.title = "Earmarks in FY2022",,
            main.title.fontface = 'bold',
            frame.lwd = 0)

## save
tmap_save(tmapped, filename = "spending.png", height = 12, width = 20, dpi = 300)


########################################
## Temperature change in june
########################################

## packages 
library(tidyverse)
library(janitor)
library(lubridate)
library(rvest)
library(glue)
library(ggmap)
library(sf)
library(tigris)
library(gganimate)

## register google API key
register_google(key = jsonlite::fromJSON("secrets/google_key.json")$google_key)

## scrape table from wikipedia and geocode address
rallies <-
  html("https://en.wikipedia.org/wiki/List_of_post-election_Donald_Trump_rallies") %>%
  html_nodes('table') %>%
  magrittr::extract2(8) %>%
  html_table() %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(address = glue("{venue}, {city}, {state}"),
         date_of_rally = str_remove_all(date_of_rally, "\\[.*?\\]"),
         date_of_rally = str_remove_all(date_of_rally, ",")) %>%
  separate(col = date_of_rally, into = c("weekday", "month", "day", "year"), sep = " ") %>%
  mutate(date = glue("{month} {day}, {year}")) %>%
  mutate(date = mdy(date)) %>%
  mutate_geocode(location = address, source = "google", output = "latlon")

## transform the address to US Atlas
rallies <- 
  rallies %>% 
  st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326) %>%
  st_transform(2163)

## load county backgroud
counties <- 
  counties(class = "sf", cb = TRUE, resolution = '5m') %>%
  filter(!str_detect(STATEFP, "15|02|60|66|69|72|78")) %>%
  select(GEOID) %>%
  st_transform(2163) 

## load state background
states <- 
  states(class = "sf", cb = TRUE, resolution = '5m') %>%
  filter(!str_detect(STATEFP, "15|02|60|66|69|72|78")) %>%
  select(NAME, GEOID) %>%
  st_transform(2163) 

## extract coordinates
coords <- 
  rallies %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(rallies) %>%
  select(-geometry)

## locate the white house
home <- ggmap::geocode("1600 Pennsylvania Avenue, Washington DC") %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(2163) %>% 
  st_coordinates() %>% 
  as_tibble()

## isolate the campaign in earnest (stalling during the pandemic)
campaign <- 
  coords %>% 
  filter(date > as_date('2020-09-01')) %>% 
  transmute(date, X, Y, location = "Campaign Trail")

## combine for a complete set of days with/without travel
combined <- 
  tibble(date = seq(ymd('2020-09-01'), ymd('2020-11-03'), by = '1 day'),
       X = home$X,
       Y = home$Y,
       location = "White House") %>%
  filter(!date %in% campaign$date) %>% 
  bind_rows(campaign) %>% 
  arrange(date)

## animate it
anim <- 
  ggplot() +
  geom_sf(data = states,
          aes(), colour = 'white', lwd = 1, alpha = 0.25) +
  geom_path(data = combined, 
            aes(x = X, y = Y), colour = '#000000', linetype = 2, alpha = 0.5) +
  geom_point(data = combined, 
             aes(x = X, y = Y, colour = location)) +
  scale_colour_brewer(palette = 'Set1', guide = 'none') + 
  transition_manual(date, cumulative = TRUE) +
  labs(title = "TRUMP CAMPAIGN RALLIES", subtitle = "{lubridate::wday(current_frame, label = TRUE, abbr = FALSE)}, {lubridate::month(current_frame, label = TRUE, abbr = FALSE)} {lubridate::day(current_frame)}") +
  theme_map()

## save it
anim_save("rallies.gif", animation = anim, 
          height = 800, width = 1000, fps = 5,
          start_pause = 0, end_pause = 2)

## summary statistics
campaign %>%
  st_as_sf(coords = c("X", "Y"), crs = 2163) %>%
  st_intersection(states) %>% 
  st_drop_geometry() %>%
  group_by(NAME) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) 

combined %>% 
  group_by(location) %>%
  summarise(n = n())

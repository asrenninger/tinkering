xl_data <- "data/county-to-county-2013-2017-ins-outs-nets-gross.xlsx"

##

library(readxl)
library(tidyverse)
library(janitor)

##

excel_sheets(path = xl_data)
read_excel(path = xl_data, sheet = 1, skip = 1)

##

metrics <- tibble()

for (i in 1:length(excel_sheets(path = xl_data))) {
  
  index <- i
  
  new_metrics <- 
    read_excel(path = xl_data, sheet = index, skip = 1) %>%
    clean_names() %>%
    select(state_code_of_geography_a, fips_county_code_of_geography_a, 
           state_u_s_island_area_foreign_region_code_of_geography_b, fips_county_code_of_geography_b,
           net_migration_from_geography_b_to_geography_a1) %>%
    filter(!str_detect(net_migration_from_geography_b_to_geography_a1, "Estimate")) %>%
    mutate(net_migration_from_geography_b_to_geography_a1 = as.numeric(net_migration_from_geography_b_to_geography_a1)) %>%
    mutate(state = excel_sheets(path = xl_data)[i])
  
  metrics <- bind_rows(metrics, new_metrics)
  
}

glimpse(metrics)

##

library(glue)

##

metrics_tidy <-
  metrics %>%
  mutate(start = paste(state_code_of_geography_a, fips_county_code_of_geography_a, sep = ""),
         end = paste(state_u_s_island_area_foreign_region_code_of_geography_b, fips_county_code_of_geography_b, sep = "")) %>%
  mutate(start = substring(start, 2),
         end = substring(end, 2)) %>%
  mutate(flow = glue("{start} to {end}")) %>%
  select(-state_code_of_geography_a, -fips_county_code_of_geography_a,
         -state_u_s_island_area_foreign_region_code_of_geography_b, -fips_county_code_of_geography_b) %>%
  rename(net_migration = net_migration_from_geography_b_to_geography_a1) %>% 
  select(flow, start, end, state, net_migration) %>%
  gather(position, location, start:end)

##

library(tigris)
library(sf)

##

counties <- 
  counties(cb = TRUE, class = 'sf') %>%
  clean_names() %>%
  select(statefp, countyfp, geoid, name) %>% 
  filter(!str_detect(statefp, "15|02|60|66|69|72|78")) %>%
  st_set_crs(4269)

counties <- st_transform(counties, crs = 102003)

##

library(tidycensus)

##

population <- 
  get_decennial(geography = "county", variables = "P001001") %>%
  clean_names() %>%
  rename(population = value)
  

##

coords <- 
  counties %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(counties) %>%
  left_join(population) %>%
  select(geoid, statefp, countyfp, population, X, Y) %>%
  rename(location = geoid)

##

metrics_spatial <-
  metrics_tidy %>%
  left_join(coords) %>%
  drop_na()

glimpse(metrics_spatial)

##

library(viridis)

##

states <-
  counties %>%
  group_by(statefp) %>%
  summarise()

##

pal <- read_csv("https://github.com/asrenninger/palettes/raw/master/orred.txt", col_names = FALSE) %>% pull(X1)

##

p <- 
  metrics_spatial %>%
  mutate(direction = case_when(net_migration > 0 ~ "out",
                               net_migration <= 0 ~ "in")) %>%
  filter(abs(net_migration) > 100) %>%
  filter(state != "Alaska" & state != "Hawaii" & state !=  "Puerto Rico" & state != "Wyoming") %>%
  filter(net_migration < 0) %>%
  mutate(pct = ntile(abs(net_migration), 100)) %>%
  ggplot() +
  geom_sf(data = states,
          aes(), fill = '#adadad', colour = '#dcdcdc') +
  geom_line(aes(x = X, y = Y, group = flow, colour = pct), 
            alpha = 0.5, size = 0.2, 
            show.legend = FALSE) +
  geom_point(aes(x = X, y = Y, colour = pct), 
             alpha = 0.5, size = 0.1, 
             show.legend = FALSE) +
  facet_wrap(~ state, ncol = 8) +
  coord_sf(crs = 102003) +
  scale_colour_gradientn(colours = pal) +
  theme_map() + 
  ggsave("test.png", height = 16, width = 24, dpi = 300)


  

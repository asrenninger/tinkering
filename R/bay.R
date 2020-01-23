library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)

##

counties <- c("Sonoma", "Napa", "Solano", "Marin", "Contra Costa", "Alameda", "Santa Clara", "San Mateo", "San Francisco")

##

bay <- 
  reduce(
    map(counties, function(x) {
      get_acs(county = x, state = "CA", geography = 'tract', 
              variables = "B01003_001",
              class = 'sf')
    }), 
    rbind
  )

##

tracts <- 
  reduce(
    map(counties, function(x){
      tracts("CA", x, class = 'sf')
    }),
    rbind
  )

##

water <- 
  reduce(
    map(counties, function(x){
      area_water("CA", x, class = 'sf')
    }),
    rbind
  ) %>% 
  st_union() %>%
  st_combine()

##

background <-
  tracts %>%
  group_by(STATEFP, COUNTYFP) %>%
  summarise() %>%
  st_difference(water)

##

plot(background)

##

dots <- 
  bay %>% 
  select(GEOID, estimate) %>% 
  left_join(tracts) %>% 
  st_as_sf() %>% 
  st_difference(water) %>%
  select(estimate) 

##

sampled <- 
  st_sample(dots, size = (dots$estimate %/% 1000), type = "random") %>% 
  st_cast("POINT") %>%
  st_coordinates() %>%                                    
  as_tibble()

##

write_csv(sampled, "dots_1000.csv")

##




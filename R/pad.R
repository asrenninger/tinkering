## Plotting all protected areas

# packages
library(sf)
library(tmap)
library(tidyverse)
library(tigris)

# data
pad <- st_read("data/PADUS2_1_Geopackage/PADUS2_1_Geopackage.gpkg", 
               layer = "PADUS2_1Combined_Fee_Designation_Easement")

# filtering  
contiguous <- unique(tigris::fips_codes$state)[1:51][-c(2, 12)]

states <- 
  states(cb = TRUE, class = 'sf') %>%
  mutate(state_area = units::set_units(st_area(geometry), ha)) %>%
  filter(STUSPS %in% contiguous) %>%
  st_transform(2163)

# map it
tmap_save(
  pad %>% 
    rownames_to_column(var = "id") %>% 
    select(id) %>%
    st_transform(2163) %>%
    st_join(states) %>% 
    drop_na(GEOID) %>%
    group_by(id) %>% 
    slice(1) %>% 
    ungroup() %>% 
    st_geometry() %>%
    tm_shape() +
    tm_polygons(col = "#000000", fill = NA, lwd = 0.0001) +
    tm_layout(frame = FALSE),
  filename = "protected_lines.png", height = 20, units = "in", dpi = 300)

# aggregate it
centroid <- 
  pad %>% 
  transmute(protected_area = units::set_units(st_area(SHAPE), ha)) %>% 
  rownames_to_column(var = 'id') %>%
  st_centroid()

# joining
joined <- st_join(st_transform(states, 2163), st_transform(centroid, 2163))
difference <- st_difference(st_transform(states, 2163), st_transform(st_combine(st_union(pad)), 2163))

# plotting
tmap_save(
  joined %>% 
    st_drop_geometry() %>% 
    group_by(NAME, state_area) %>% 
    summarise(protected_area = sum(protected_area)) %>%
    mutate(`Percent Protected` = 1 - units::drop_units((state_area - protected_area) / state_area)) %>%
    mutate(`Percent Protected` = if_else(`Percent Protected` > 0.98, 0.98, `Percent Protected`)) %>% 
    drop_na() %>%
    left_join(states) %>% 
    filter(STUSPS %in% contiguous) %>%
    st_as_sf() %>% 
    select(`Percent Protected`) %>% 
    st_transform(2163) %>%
    tm_shape() +
    tm_fill(col = "Percent Protected", pal = RColorBrewer::brewer.pal(n = 9, name = 'OrRd')) +
    tm_borders(col = "#000000", lty = 2) +
    tm_layout(frame = FALSE),
  filename = "protected_areas.png", height = 8, dpi = 300)

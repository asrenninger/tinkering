## Plotting all protected areas

# packages
library(sf)
library(tmap)
library(tidyverse)
library(tigris)

# data
pad <- 
  st_read("~/Downloads/PADUS2_1_Geopackage/PADUS2_1_Geopackage.gpkg", 
          layer = "PADUS2_1Combined_Fee_Designation_Easement") %>%
  st_transform(2163)

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

## fl
fl <- 
  states %>% 
  filter(STUSPS == "FL") %>% 
  st_union() %>% 
  st_combine()

pad_fl <- st_intersection(pad, fl)

# map it
tmap_save(
  pad_fl %>% 
    st_transform(3857) %>% 
    st_geometry() %>%
    tm_shape() +
    tm_fill(col = "#000000") +
    tm_shape(counties(cb = TRUE, class = 'sf') %>% 
               filter(STATEFP == "12") %>% 
               st_transform(3857)) +
    tm_borders(col = '#4a4a4a', lwd = 2, lty = 3) + 
    tm_layout(frame = FALSE),
  filename = "protected_fl_redux.png", height = 10, units = "in", dpi = 300)

# md
md <- 
  states %>% 
  filter(STUSPS == "MD") %>% 
  st_union() %>% 
  st_combine()

pad_md <- st_intersection(pad, md)

# map it
tmap_save(
  pad_md %>% 
    st_geometry() %>%
    tm_shape() +
    tm_fill(col = "#000000") +
    tm_shape(counties(cb = TRUE, class = 'sf') %>% 
               filter(STATEFP == "24")) +
    tm_borders(col = '#4a4a4a', lwd = 2, lty = 3) + 
    tm_layout(frame = FALSE),
  filename = "protected_md_redux.png", height = 10, units = "in", dpi = 300)

# nj
nj <- 
  states %>% 
  filter(STUSPS == "NJ") %>% 
  st_union() %>% 
  st_combine() %>%
  st_transform(3857)

pad_nj <- st_intersection(st_transform(pad, 3857), nj)

# map it
tmap_save(
  pad_nj %>% 
    st_geometry() %>%
    tm_shape() +
    tm_fill(col = "#000000") +
    tm_shape(counties(cb = TRUE, class = 'sf') %>% 
               filter(STATEFP == "34")) +
    tm_borders(col = '#4a4a4a', lwd = 2, lty = 3) + 
    tm_layout(frame = FALSE),
  filename = "protected_nj_redux.png", height = 10, units = "in", dpi = 300)

## all together now
blades <- states %>% group_by(STUSPS) %>% group_split() %>% map(~st_combine(st_union(.x)))

state_areas <- 
  reduce(map(blades,
             function(blade){
               return(st_combine(st_union(st_intersection(pad, blade))))
             }
             
  ), 
  rbind)

counties_fl <- 
  tigris::fips_codes %>% 
  filter(state == "FL") %>% 
  pull(county) %>% 
  str_remove_all(" County")

download.file(url = "http://ww10.doh.state.fl.us/pub/bos/Inventory/FloridaWaterManagementInventory/MiamiDade/miamidade-public.zip",
              destfile = "florida/Desoto.zip")

purrr::map(counties_fl[60:length(counties_fl)], 
           function(x) {
             download.file(url = glue::glue("http://ww10.doh.state.fl.us/pub/bos/Inventory/FloridaWaterManagementInventory/{x}/{str_to_lower(str_remove_all(x, pattern = ' '))}-public.zip"),
                           destfile = glue::glue("florida/{x}.zip"))}
)

purrr::map(fs::dir_ls("florida"), ~unzip(.x, exdir = "florida/extracted"))

parcels <- reduce(purrr::map(fs::dir_ls("florida/extracted", recurse = TRUE, regexp = ".shp$")[1:3], function(x) {
  st_read(x) %>%
    select(WW)}), rbind)

parcels %>% drop_na(WW) %>% plot()



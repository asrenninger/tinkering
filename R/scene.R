########################################
## Scenic or not
########################################

## packages
library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)

## data
votes <- 
  read_delim("http://scenicornot.datasciencelab.co.uk/votes.tsv", delim = '\t') %>%
  janitor::clean_names() %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(27700) %>%
  select(-id)

## context
rnaturalearth::ne_download(scale = 10, type = 'states', category = 'cultural')
states <- rnaturalearth::ne_load(scale = 10, type = 'states', category = 'cultural', returnclass = 'sf')

britain <- 
  states %>% 
  st_transform(27700) %>%
  filter(admin == "United Kingdom") %>%
  filter(geonunit != "Northern Ireland") %>% 
  select(admin, geonunit) 

## regular tesselation
grid <- 
  britain %>%
  st_union() %>%
  st_combine() %>%
  st_make_grid(cellsize = 5000, square = FALSE) %>%
  st_as_sf() %>% 
  rownames_to_column(var = "id") %>%
  rename(geometry = x) 

grid <- 
  grid %>%
  st_join(votes) %>%
  st_drop_geometry() %>% 
  group_by(id) %>% 
  summarise(average = mean(average, na.rm = TRUE)) %>%
  left_join(grid) %>%
  st_as_sf() 

## plot
tmap_save(tm_shape(grid[!is.nan(grid$average), ]) +
          tm_fill(size = NA, col = "average", palette = scico::scico(9, palette = 'hawaii')) +
          tm_shape(st_combine(st_union(britain))) +
          tm_borders(col = '#ffffff') +
          tm_layout(main.title = "Beauty in Britain",
                    title = 'Rated at \"Scenic or Not\"',
                    title.fontface = 'bold',
                    legend.position = c(0.05, 0.2),
                    frame.lwd = 0),
        filename = "scenic.png", height = 8, dpi = 300)

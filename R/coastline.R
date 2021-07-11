########################################
## Having fun with Mandelbrot
########################################

## get states
rnaturalearth::ne_download(scale = 10, type = 'states', category = 'cultural')
states <- rnaturalearth::ne_load(scale = 10, type = 'states', category = 'cultural', returnclass = 'sf')

## load packages
library(tidyverse)
library(glue)
library(sf)
library(tmap)
library(tmaptools)
library(magick)
library(fs)

## select only the island of britain
britain <- 
  states %>% 
  st_transform(27700) %>%
  filter(admin == "United Kingdom") %>%
  select(admin, geonunit) %>% 
  filter(geonunit != "Northern Ireland") %>%
  st_union() %>%
  st_combine() %>%
  st_cast("POLYGON") %>%
  st_as_sf() %>% 
  rownames_to_column(var = "id") %>%
  rename(geometry = x) %>%
  mutate(area = st_area(geometry)) %>% 
  arrange(desc(area)) %>% 
  slice(1)

## iterate through simplifications of the island
map(1:99,
    function(x){
      
      # simplify
      new_britain <- rmapshaper::ms_simplify(input = st_geometry(britain), keep = 1 / x)
      # measure
      new_length <- round(units::drop_units(units::set_units(st_length(st_cast(st_combine(new_britain), "MULTILINESTRING")), km)))
      
      # map
      map <- 
        tm_shape(st_buffer(st_geometry(britain), 10000)) +
        tm_borders(col = '#ffffff', lwd = 1, lty = 1) +
        tm_shape(st_geometry(new_britain)) +
        tm_borders(col = '#000000', lwd = 1, lty = 1) +
        tm_layout(main.title = "How Long Is the Coast of Britain?",
                  title = glue("{new_length} km"),
                  title.fontface = 'bold',
                  title.position = c(0.6, 0.7),
                  frame.lwd = 0)
      
      # generate a label that will keep them all in order
      lab <- str_pad(x, width = 2, side = 'left', pad = "0")
      # save
      tmap_save(map, filename = glue("viz/coastline/britain_{lab}.png"), height = 10, width = 5, dpi = 300)
      
    })

## create the gif
dir_ls("viz/coastline", pattern = ".*png") %>% 
  image_read() %>% 
  image_join() %>% 
  image_animate(fps = 4) %>% 
  image_write("coastline.gif")


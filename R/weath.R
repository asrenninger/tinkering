########################################
## Temperature change in june
########################################

## packages 
library(stars)
library(sf)
library(raster)
library(tidyverse)
library(tmap)
library(glue)
library(fs)
library(tigris)
library(tmap)
library(tmaptools)
library(magick)

## frame
states <- states(cb = TRUE, class = 'sf') %>%
  filter(!str_detect(STATEFP, "02|15|6[0-9]|7[0-9]")) %>%
  rmapshaper::ms_simplify(0.005) %>%
  select(NAME) %>% 
  st_transform(2163)

plot(states)

## download data
map(str_pad(1:30, side = 'left', width = 2, pad = "0"), 
    function(x) {
      
      download.file(glue("https://ftp.cpc.ncep.noaa.gov/GIS/GRADS_GIS/GeoTIFF/TEMP/us_tmax/us.tmax_nohads_ll_202106{x}_float.tif"),
                    destfile = glue("data/weather/202106{x}.tif"))
    
      })

## establish breaks
brks <- seq(20, 40, by = (20 / 10))
bnds <- replace(brks, brks == 20, "-")
bnds <- replace(bnds, bnds == 40, "+")
labs <- paste(bnds[1:10], bnds[2:11], sep = " to ") %>%
  if_else(str_detect(., "\\-|\\+"), str_remove_all(., "to "), .)

## map it
map(str_pad(1:30, side = 'left', width = 2, pad = "0"),
    function(x, aoi = states){
      
      # pull the file
      file <- glue("data/weather/202106{x}.tif")
      
      # grab contours
      temperature <- raster(file)
      contours <-   temperature %>% 
        rasterToContour(nlevels = 100) %>% 
        st_as_sf() %>% 
        st_transform(2163) %>% 
        st_intersection(aoi)
      
      # transform the data to match the states
      temperature <- 
        temperature %>% 
        st_as_stars() %>% 
        st_transform(2163)
      
      # map it
      map <- 
        tm_shape(temperature[states] %>% 
                   st_as_sf() %>% 
                   st_intersection(states) %>% 
                   set_names(c("temperature", "NAME", "geometry"))) +
        tm_fill(col = 'temperature', pal = scico::scico(10, palette = 'hawaii', direction = -1),
                breaks = brks, labels = labs, title = 'High Temperature') +
        tm_shape(states) +
        tm_borders(col = '#ffffff', lwd = 0.75, lty = 2) +
        tm_shape(contours %>%
                   mutate(threshold = as.numeric(as.character(level))) %>%
                   filter(threshold >= 30)) +
        tm_lines(col = '#ffffff', lwd = 0.25) +
        tm_layout(main.title = "Heat Waves",
                  title = glue("June {parse_number(x)}"),
                  title.fontface = 'bold',
                  frame.lwd = 0)
      
      # save it
      tmap_save(map, filename = glue("viz/weather/temperature_{x}.png"), height = 10, width = 14, dpi = 300)
      
    })

## generate animation
dir_ls("viz/weather", pattern = ".*png") %>% 
  image_read() %>% 
  image_join() %>% 
  image_animate(fps = 2.5) %>% 
  image_write("weather.gif")





#################################
## G-Econ Data
#################################

## packages
library(readxl)
library(tidyverse)
library(sf)

## files
download.file("https://gecon.yale.edu/sites/default/files/files/Gecon40_post_final.xls",
              destfile = "gecon.xls")

## wrangling
coastline <- 
  rnaturalearth::ne_coastline(returnclass = 'sf') %>% 
  rmapshaper::ms_simplify(0.09) %>%
  filter(!st_is_empty(geometry)) %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>% 
  st_transform("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")

world <- 
  readxl::read_xls("gecon.xls") %>%
  select(LONGITUDE, LAT, PPP2005_40) %>% 
  raster::rasterFromXYZ() %>% 
  stars::st_as_stars() %>% 
  st_set_crs(4326) %>% 
  st_as_sf() %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>% 
  st_transform("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")

## plotting
ggplot() +
  geom_sf(data = world,
          aes(fill = factor(cut(PPP2005_40, quantile(world$PPP2005_40, probs = c(0, 0.3, 0.5, 0.7, 0.8, 0.9, 0.95, 0.99, 1))))), 
          colour = '#ffffff', size = 0) +
  geom_sf(data = coastline,
          aes(), fill = NA, colour = '#ffffff') +
  scale_fill_manual(values = scico(n = 9, palette = 'hawaii'),
                    labels = paste(round(quantile(world$PPP2005_40, probs = c(0, 0.3, 0.5, 0.7, 0.8, 0.9, 0.95, 0.99, 1)), 2)),
                    name = 'PPP (2005)',
                    guide = guide_legend(direction = "horizontal",
                                         keyheight = unit(2, units = "mm"),
                                         keywidth = unit(10, units = "mm"),
                                         title.position = 'top',
                                         label.position = 'bottom',
                                         title.hjust = 0.5,
                                         label.hjust = 0,
                                         nrow = 1,
                                         byrow = TRUE),
                    na.translate = FALSE) +
  labs(title = "Global Economic Heft",
       subtitle = "Purchasing Power Parity in 2005",
       caption = "Data from the Yale G-Econ Project") + 
  theme_void() +
  theme(plot.title = element_text(colour = '#000000', face = 'bold', hjust = 0),
        plot.subtitle = element_text(colour = '#000000', hjust = 0), 
        legend.position = 'bottom',
        legend.title = element_text(face = 'bold'))

ggsave(plot = last_plot(), filename = "gdppp.png", height = 12, width = 20, dpi = 300)

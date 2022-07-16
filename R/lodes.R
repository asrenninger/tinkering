###################################
## ICDR
###################################

## packages
library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)

## download
times <- map(unique(tigris::fips_codes$state)[1:51],
             tictoc::tic()
             ~lehdr::grab_lodes(state = .x, year = 2015, lodes_type = 'od', job_type = 'JT00', segment = 'S000', agg_geo = 'BG', 
                                state_part = "aux", 
                                download_dir = "~/Desktop/R/git/networks/data/lodes/",
                                use_cache = TRUE)
             })

## load
lodes <- map_dfr(fs::dir_ls("~/Desktop/R/git/networks/data/lodes/"), 
                 ~vroom::vroom(.x, 
                               col_select = c("w_geocode", "h_geocode", "S000"),
                               col_types = cols(w_geocode = col_character(),
                                                h_geocode = col_character(),
                                                S000 = col_integer())) %>% 
                   filter(S000 > 1))

## block groups for country
blocks <-
  reduce(
    map(unique(tigris::fips_codes$state)[1:51], 
        ~tigris::block_groups(state = .x, cb = TRUE, year = 2015)), 
    rbind)

## move alaska and hawaii
blocks <- tigris::shift_geometry(blocks, geoid_column = "STATEFP")
centroids <- st_centroid(blocks) 

plot(st_geometry(centroids))

## generate origin-destination shape
lines <- od::od_to_sf(lodes %>% 
                        mutate(w_geocode = str_sub(w_geocode, 1, 12),
                               h_geocode = str_sub(h_geocode, 1, 12)) %>%
                        group_by(w_geocode, h_geocode) %>% 
                        summarise(flow = sum(S000)), 
                      select(centroids, GEOID))

## dissolve the background
border <- 
  blocks %>% 
  st_union() %>% 
  st_combine()

## map it
commutes <- 
  tm_shape(border) +
  tm_borders(col = '#7c7c7c', lwd = 0.5, lty = 1) + 
  tm_shape(lines) +
  tm_lines(col = '#000000', lwd = "flow", alpha = 0.5, scale = 2) +
  tm_layout(title = "COMMUTES",
            frame = FALSE,
            legend.position = c("RIGHT", "BOTTOM"))

tictoc::tic()
tmap_save(commutes, height = 10, dpi = 300)
tictoc::toc()
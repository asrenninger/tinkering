########################################
## Rainfall rescue
########################################

## packages 
library(vroom)
library(fs)
library(tidyverse)
library(sf)

## natural earth boundaries
bounds <- 
  bind_rows(rnaturalearth::ne_countries(scale = 50, returnclass = 'sf') %>% 
              filter(str_detect(name_long, "United Kingdom")) %>%
              st_transform(27700), 
            rnaturalearth::ne_countries(scale = 50, returnclass = 'sf') %>% 
              filter(str_detect(name_long, "Ireland")) %>%
              st_transform(27700)) %>%
  st_as_sf() %>%
  st_union() %>% 
  st_combine()

## download files
files <- dir_ls("data/ed-hawkins-rainfall-rescue-fbef1fe/DATA", regexp = "\\<[[:upper:]][[:upper:]]*\\>.csv", recurse = TRUE)

stations <- 
  map_df(files[1:200], function(x) { 
    name <- vroom(x, col_names = FALSE, skip = 0, n_max = 1, 
                  progress = FALSE,
                  col_types = cols(X1 = col_character()),
                  trim_ws = TRUE) %>% 
      pull(X1)
    
    vroom(x, col_names = FALSE, skip = 1, n_max = 1,
          progress = FALSE,
          col_types = cols(X1 = col_character()),
          trim_ws = TRUE) %>% 
      select(X2, X4, X6, X8) %>% 
      set_names(c("id", "lon", "lat", "elevation")) %>% 
      mutate(name = name) 
  })

for (i in 801:length(files)) {
  x <- files[i]
  
  name <- vroom(x, col_names = FALSE, skip = 0, n_max = 1, 
                progress = FALSE,
                col_types = cols(X1 = col_character()),
                trim_ws = TRUE) %>% 
    pull(X1)
  
  attempt <- try(vroom(x, col_names = FALSE, skip = 1, n_max = 1,
                       progress = FALSE,
                       col_types = cols(X1 = col_character()),
                       trim_ws = TRUE) %>% 
                   select(X2, X4, X6, X8) %>% 
                   set_names(c("id", "lon", "lat", "elevation")) %>% 
                   mutate(name = name))
  if("try-error" %in% class(attempt)) {
    next
  } else {
    temp <- 
      vroom(x, col_names = FALSE, skip = 1, n_max = 1,
            progress = FALSE,
            col_types = cols(X1 = col_character()),
            trim_ws = TRUE) %>% 
      select(X2, X4, X6, X8) %>% 
      set_names(c("id", "lon", "lat", "elevation")) %>% 
      mutate(name = name) 
  }
  
  
  
  stations <- bind_rows(stations, temp)
  
}

stations <- 
  bind_rows(stations, 
            map_df(files[801:1000], function(x) { 
              name <- vroom(x, col_names = FALSE, skip = 0, n_max = 1, 
                            progress = FALSE,
                            col_types = cols(X1 = col_character()),
                            trim_ws = TRUE) %>% 
                pull(X1)
              
              vroom(x, col_names = FALSE, skip = 1, n_max = 1,
                    progress = FALSE,
                    col_types = cols(X1 = col_character()),
                    trim_ws = TRUE) %>% 
                select(X2, X4, X6, X8) %>% 
                set_names(c("id", "lon", "lat", "elevation")) %>% 
                mutate(name = name) 
            }))

rainfall <- tibble()

for (i in 1:length(files)) {
  x <- files[i]
  
  name <- vroom(x, col_names = FALSE, skip = 0, n_max = 1, 
                progress = FALSE,
                col_types = cols(X1 = col_character()),
                trim_ws = TRUE) %>% 
    pull(X1)
  
  attempt <- try(vroom(x, col_names = TRUE, skip = 4, n_max = 13,
                       progress = FALSE,
                       trim_ws = TRUE) %>%
                   janitor::clean_names() %>%
                   pivot_longer(!x1, names_to = "year", values_to = "rainfall") %>%
                   rename(month = x1) %>%
                   mutate(station = name,
                          year = parse_number(str_sub(year, 2, 5))))
  if("try-error" %in% class(attempt)) {
    next
  } else {
    temp <- 
      vroom(x, col_names = TRUE, skip = 4, n_max = 13,
            progress = FALSE,
            trim_ws = TRUE) %>%
      janitor::clean_names() %>%
      pivot_longer(!x1, names_to = "year", values_to = "rainfall") %>%
      rename(month = x1) %>%
      mutate(station = name,
             year = parse_number(str_sub(year, 2, 5)))
  }
  
  
  
  rainfall <- bind_rows(rainfall, temp)
  
}

stations <- read_csv("stations.csv")
rainfall <- read_csv("rainfall.csv")

## process it
test <- 
  rainfall %>% 
  filter(month == "Total",
         rainfall >= 0,
         year > 1750,
         year < 1960) %>% 
  drop_na() %>% 
  mutate(rainfall = scales::rescale(rainfall, to = c(0, 2))) %>%
  group_by(station) %>%
  mutate(year = year - min(year)) %>%
  ungroup() %>% 
  mutate(year = scales::rescale(year, to = c(0, 4))) %>%
  ungroup() %>% 
  rename(name = station) %>%
  left_join(stations) %>%
  mutate(x = year + lon,
         y = rainfall + lat)

test <- 
  test %>% 
  drop_na(x, y) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  st_transform(27700) %>%
  st_coordinates() %>% 
  as_tibble() %>%
  bind_cols(drop_na(test, x, y))

cols <- read_csv("https://raw.githubusercontent.com/asrenninger/palettes/master/cool.txt", col_names = FALSE) %>% pull(X1)

## plot it
plot <- 
  ggplot() +
  geom_sf(data = bounds,
          aes(),
          fill = NA, colour = '#ffffff', alpha = 0.5, linetype = 2, lwd = 0.1) +
  geom_line(data = 
              test %>%
              group_by(name) %>%
              add_tally() %>% 
              filter(n > 1) %>%
              ungroup() %>% 
              group_by(name) %>% 
              mutate(change = (rainfall - lag(rainfall)) / lag(rainfall)) %>%
              ungroup() %>% 
              replace_na(list(change = 0)),
            aes(x = X, y = Y, group = name, colour = abs(change)),
            size = 0.1, 
            alpha = 0.5,
            inherit.aes = FALSE) +
  scale_colour_gradientn(colours = cols, guide = 'none',
                         limits = c(0, 0.5), oob = scales::squish) + 
  # transition_manual(name) +
  # labs(title = "{current_frame}", subtitle = "Rainfall Rescue Readings") +
  labs(title = "All stations",
       subtitle = "Rainfall Rescue Readings") +
  theme_bm_legend() +
  ggsave("rainfall_geolocated.png", height = 14.25, width = 10, dpi = 300)

# anim_save(filename = "rainfall_animated.gif", animation = plot, fps = 4,
#           height = 14.35, width = 10, units = "in", res = 300)



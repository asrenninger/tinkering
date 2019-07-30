## Start with spatial tools

library(rnaturalearth)
library(sf)

## Grab a map of the earth

countries <- 
  ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(54030)

## Clean that data

library(tidyverse)
library(janitor)

## Import data and create sf objects

mod_mod <- 
  read_csv("~/Desktop/R/git/tinkering/data/modelskiModernV2.csv") %>%
  clean_names() %>%
  mutate(x = longitude, y = latitude) %>%
  drop_na(x, y) %>%
  st_as_sf(coords = c("x", "y")) %>%
  st_set_crs(4326)

mod_anc <- 
  read_csv("~/Desktop/R/git/tinkering/data/modelskiAncientV2_replaced.csv") %>%
  clean_names() %>%
  mutate(longitude = as.numeric(longitude), 
         latitude = as.numeric(latitude)) %>% 
  mutate(x = longitude, y = latitude) %>%
  drop_na(x, y) %>%
  st_as_sf(coords = c("x", "y")) %>%
  st_set_crs(4326)

chand <- 
  read_csv("~/Desktop/R/git/tinkering/data/chandlerV2_replaced.csv") %>%
  clean_names() %>%
  mutate(longitude = as.numeric(longitude), 
         latitude = as.numeric(latitude)) %>% 
  mutate(x = longitude, y = latitude) %>%
  drop_na(x, y) %>%
  st_as_sf(coords = c("x", "y")) %>%
  st_set_crs(4326)

## Suss out its form

glimpse(chand)
glimpse(mod_mod)
glimpse(mod_anc)

names(chand) == names(mod_anc)
names(mod_mod)

## bind it all together

binding <- 
  bind_rows(
    mutate(mod_mod  %>%
             st_transform(54030) %>%
             st_coordinates() %>%
             as_tibble(),
           set = "modelski modern"),
    mutate(mod_anc %>%
             st_transform(54030) %>%
             st_coordinates() %>%
             as_tibble(),
           set = "modelski ancient"),
    mutate(chand %>%
             st_transform(54030) %>%
             st_coordinates() %>%
             as_tibble(),
           set = "chandler"))

## plot a comparison of all sources

ggplot() +
  geom_sf(data = countries, aes(), fill = '#696969', colour = '#ffffff', size = 0.1) +
  geom_point(data = binding, aes(X, Y), colour = 'black', alpha = 0.5) +
  scale_size_continuous(range = c(0.1, 3)) +
  facet_wrap(~ set, nrow = 1) +
  theme_map() +
  ggsave("comparison.png", height = 3, width = 10, dpi = 300)

## Transform from wide to long format

names(chand)

gathered <- 
  chand %>%
  gather(year, population, bc_2250:ad_1975) %>%
  mutate(year = str_replace_all(year, pattern = "ad_", replacement = "")) %>%
  mutate(year = str_replace_all(year, pattern = "bc_", replacement = "-")) %>%
  mutate(year = as.numeric(year))

## Project it in the Robinson projection

gathered <- st_transform(gathered, 54030)

coordinates <-
  gathered %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(gathered)

## Animate cities over the centuries

library(gganimate)

## Store an animation

animate <-
  ggplot(coordinates %>%
           drop_na(population)) + 
  geom_sf(data = countries,
          aes(),
          fill = '#696969', colour = '#FFFFFF', size = 0.1) +
  geom_point(aes(x = X, y = Y, size = population, colour = year), alpha = 0.5) +
  coord_sf(datum = 54030) + 
  scale_size_continuous(range = c(1, 50),
                        guide = FALSE) +
  scale_colour_gradient2(low = '#8e0152', mid = '#f7f7f7', high = '#276419',
                         midpoint = 0,
                         breaks = c(-1000, 0, 1000),
                         guide = guide_continuous) +
  labs(title = "{round(frame_time, 0)}", subtitle = "MILLENNIA URBANA") +
  transition_time(year) +
  ease_aes('linear') + 
  shadow_wake(0.05, size = 3, alpha = TRUE, wrap = FALSE, 
              falloff = 'sine-in', exclude_phase = 'enter') + 
  exit_recolour(colour = '#f7f7f7') +
  theme_map()

anim_save("population.gif", animation = animate, 
          height = 600, width = 800, nframes = 100, fps = 5,
          start_pause = 2, end_pause = 20)

## Create snapshots of each continent

world <- ne_countries(scale = "medium", returnclass = "sf")

## A (likely) way of coverting an sf object into a long list of points

poly <- bind_cols(world %>%
                    st_cast("POLYGON") %>%
                    st_cast("MULTIPOINT") %>%
                    st_cast("POINT"), 
                  world %>%
                    st_cast("POLYGON") %>%
                    st_cast("MULTIPOINT") %>%
                    st_coordinates() %>%
                    as_tibble())

## Bring in the data

chand <- read_csv("~/Desktop/R/git/tinkering/data/chandlerV2_replaced.csv") %>%
  clean_names() %>%
  mutate(longitude = as.numeric(longitude), 
         latitude = as.numeric(latitude)) %>% 
  mutate(x = longitude, y = latitude) %>%
  drop_na(x, y) %>%
  st_as_sf(coords = c("x", "y")) %>%
  st_set_crs(4326)

cities_shaped <- 
  chand %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(chand)

cities <- 
  chand %>%
  gather(year, population, bc_2250:ad_1975) %>%
  mutate(year = str_replace_all(year, pattern = "ad_", replacement = "")) %>%
  mutate(year = str_replace_all(year, pattern = "bc_", replacement = "-")) %>%
  mutate(year = as.numeric(year))

cities_shaped <-
  cities %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(cities) %>%
  drop_na(population)

## Create focal points for each image

centroids <- 
  world %>%
  group_by(continent) %>%
  summarise() %>%
  filter(!str_detect(continent, "Antarctica|Seven")) %>%
  st_centroid()

coords <-
  centroids %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(centroids)

## Then create a blank repository for those pictures

plots <- list()

##

library(glue)

## Iterate through each continent and generate an image

for (i in 1:nrow(centroids)) {
  
  plot <-
    ggplot() +
    geom_polygon(data = poly, 
                 aes(x = X, y = Y, group = L1),
                 fill = '#696969', colour = '#FFFFFF', size = 0.1) +
    geom_point(data = cities_shaped, 
               aes(x = X, y = Y, size = population, colour = year), show.legend = FALSE) +
    scale_size_continuous(range = c(0.5, 5)) +
    scale_colour_gradient2(low = '#8e0152', mid = '#f7f7f7', high = '#276419',
                           midpoint = 0) +
    coord_map("ortho", orientation = c(coords$Y[i], coords$X[i], 0)) +
    labs(title = "6,000 years of cities", subtitle = glue("{centroids$continent[i]}"), 
         x = "city", y = "number") +
    theme_map()
  
  plots[[glue("plot{i}")]] <- plot
   
}

## Arrange them on a canvas

library(gridExtra)
library(grid)

## Grob it all together

blank <- grid.rect(gp = gpar(col = 'transparent', fill = 'transparent'))

lay <- rbind(c(1, 1, 1, 2, 2, 2),
             c(1, 1, 1, 2, 2, 2),
             c(1, 1, 1, 2, 2, 2),
             c(3, 3, 3, 4, 4, 4),
             c(3, 3, 3, 4, 4, 4),
             c(3, 3, 3, 4, 4, 4),
             c(5, 5, 5, 6, 6, 6),
             c(5, 5, 5, 6, 6, 6),
             c(5, 5, 5, 6, 6, 6)) 

agg <- grobTree(rectGrob(gp = gpar(fill = 'transparent', lwd = 0)), 
                grid.arrange(grobs = plots, layout_matrix = lay))

getwd()
setwd("/Users/andrewrenninger/Desktop/R/git/tinkering/viz")

ggsave(agg, filename = "aggregate.png", height = 10, width = 10, dpi = 300)

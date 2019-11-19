library(sf)

##

roads <- st_read("data/londonroads.geojson")
roads <- roads %>% st_cast("LINESTRING")

roads %>% mapview::mapview()

##

library(sfnetworks)

##

network <- sfn_asnetwork(roads)
graph <- sfn_network2graph(network)

##

library(tidyverse)
library(igraph)
library(ggraph)

##

simplified <- simplify(graph, remove.multiple = TRUE, remove.loops = TRUE)

##

plot(simplified,
     vertex.size = .1,
     vertex.label = '')

##

diameter(graph, directed = FALSE)

##

roads$tween <- edge_betweenness(graph, e = E(graph), directed = FALSE)

##

grouped <-
  rbind(roads %>%
          filter(!is.na(name)) %>%
          group_by(name) %>%
          summarise(aggretween = sum(tween)),
        roads %>%
          filter(is.na(name)) %>%
          transmute(name = name,
                    aggretween = tween)) %>%
  st_as_sf()

##

library(magrittr)
library(classInt)

##

natural_tween <- 
  grouped %>%
  use_series(aggretween) %>%
  classIntervals(n = 9, style = 'jenks') %>%
  use_series(brks)

##

library(scales)

##

lab <- 
  c(round(rescale(natural_tween, to = c(0, 10)), 2)) %>% 
  as_tibble() %>% 
  pull()

##

pal <- read_csv("https://github.com/asrenninger/palettes/raw/master/turbo.txt", col_names = FALSE) %>% pull(X1)

##

theme_bm_legend <- function () {
  theme_void() + 
    theme(plot.background = element_rect(fill = 'black', colour = 'black'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_text(colour = 'grey50'),
          legend.text = element_text(colour = 'white'),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'white', size = 15),
          panel.grid.major = element_line(size = NA), 
          panel.grid.minor = element_line(size = NA),
          plot.margin = margin(10, 10, 10, 10),
          legend.position = c(0.85, 0.05)
    )
  
}

##

guide_discrete <-
  guide_legend(direction = "horizontal",
               keyheight = unit(1, units = "mm"),
               keywidth = unit(100 / length(lab), units = "mm"),
               title.position = 'top',
               label.position = 'bottom',
               title.hjust = 0.5,
               label.hjust = 1,
               nrow = 1,
               byrow = TRUE)

##

p_tween <- 
  ggplot(grouped %>%
           mutate(group = factor(cut(as.numeric(aggretween), c(natural_tween))))) +
  geom_sf(aes(colour = group, fill = group),
          size = 0.5) +
  scale_fill_manual(values = pal,
                    labels = c(lab[2:9], 10),
                    na.translate = FALSE,
                    name = "betweenness (rescaled)",
                    guide = guide_discrete) +
  scale_color_manual(values = pal,
                     labels = c(lab[2:9], 10),
                     na.translate = FALSE,
                     name = "betweenness (rescaled)",
                     guide = guide_discrete) +
  labs(title = "through-movement", subtitle = "BETWEENNESS CENTRALITY") +
  theme_bm_legend()

##

ggsave(p_tween, filename = "tween.png", height = 11.75, width = 13, dpi = 300)

##

speeds <- read_csv("data/movement-speeds-hourly-london-2019-1.csv")

##

glimpse(speeds)

##

library(lubridate)

##

roadspeeds <-
  speeds %>%
  mutate(day = wday(utc_timestamp, label = TRUE, abbr = TRUE)) %>%
  filter(hour < 23 & hour > 5) %>%
  group_by(day, osm_way_id) %>%
  summarise(speed = mean(speed_mph_mean)) %>%
  rename(osm_id = osm_way_id)

##

joined <- 
  left_join(roadspeeds, roads) %>%
  mutate(name = as.character(name)) %>%
  drop_na(geometry, speed) %>%
  st_as_sf() %>%
  group_by(osm_id, name) %>%
  summarise(speed = mean(speed, na.rm = TRUE)) %>%
  ungroup()

##

grouped <-
  rbind(joined %>%
          filter(!is.na(name)) %>%
          group_by(name) %>%
          summarise(aggretween = mean(speed)),
        joined %>%
          filter(is.na(name)) %>%
          transmute(name = name,
                    aggretween = speed)) %>%
  st_as_sf()

##

natural_tween <- 
  grouped %>%
  use_series(aggretween) %>%
  classIntervals(n = 9, style = 'jenks') %>%
  use_series(brks)

##

lab <- 
  c(round(natural_tween, 2)) %>% 
  as_tibble() %>% 
  pull()

##

p_speed <- 
  ggplot(grouped %>%
           mutate(group = factor(cut(as.numeric(aggretween), c(natural_tween))))) +
  geom_sf(aes(colour = group, fill = group),
          size = 0.5) +
  scale_fill_manual(values = pal,
                    labels = c(lab[2:9], 10),
                    na.translate = FALSE,
                    name = "speed (km/h)",
                    guide = guide_discrete) +
  scale_color_manual(values = pal,
                     labels = c(lab[2:9], 10),
                     na.translate = FALSE,
                     name = "speed (km/h)",
                     guide = guide_discrete) +
  labs(title = "through-movement", subtitle = "OBSERVED SPEED") +
  theme_bm_legend()

##

ggsave(p_speed, filename = "speed.png", height = 11.75, width = 13, dpi = 300)

##

roads <- st_read("data/greaterlondonroads.geojson")

##

roads <- st_transform(roads, 27700)
roads <- st_cast(roads, "LINESTRING")

##

library(sfnetworks)

##

network <- sfn_asnetwork(roads)
graph <- sfn_network2graph(network)

##

library(tidyverse)
library(igraph)
library(ggraph)

##

simplified <- simplify(graph, remove.multiple = TRUE, remove.loops = TRUE)

##

plot(simplified,
     vertex.size = .1,
     vertex.label = '')

##

diameter(graph, directed = FALSE)

##

roads$tween <- edge_betweenness(graph, e = E(graph), directed = FALSE)

##

grouped <-
  rbind(roads %>%
          filter(!is.na(name)) %>%
          group_by(name) %>%
          summarise(aggretween = sum(tween)),
        roads %>%
          filter(is.na(name)) %>%
          transmute(name = name,
                    aggretween = tween)) %>%
  st_as_sf()

##

library(magrittr)
library(classInt)

##

natural_tween <- 
  grouped %>%
  use_series(aggretween) %>%
  classIntervals(n = 9, style = 'jenks') %>%
  use_series(brks)

##

library(scales)

##

lab <- 
  c(round(rescale(natural_tween, to = c(0, 10)), 2)) %>% 
  as_tibble() %>% 
  pull()

##

p_tween <- 
  ggplot(grouped %>%
           mutate(group = factor(cut(as.numeric(aggretween), c(natural_tween))))) +
  geom_sf(aes(colour = group, fill = group),
          size = 0.09) +
  scale_fill_manual(values = pal,
                    labels = c(lab[2:9], 10),
                    na.translate = FALSE,
                    name = "betweenness (rescaled)",
                    guide = guide_discrete) +
  scale_color_manual(values = pal,
                     labels = c(lab[2:9], 10),
                     na.translate = FALSE,
                     name = "betweenness (rescaled)",
                     guide = guide_discrete) +
  labs(title = "through-movement", subtitle = "BETWEENNESS CENTRALITY") +
  theme_bm_legend()

##

ggsave(p_tween, filename = "tween_gla.png", height = 11.7, width = 14, dpi = 300)

##

joined <- 
  left_join(roadspeeds, roads) %>%
  mutate(name = as.character(name)) %>%
  drop_na(geometry, speed) %>%
  st_as_sf() %>%
  group_by(osm_id, name) %>%
  summarise(speed = mean(speed, na.rm = TRUE)) %>%
  ungroup()

##

grouped <-
  rbind(joined %>%
          filter(!is.na(name)) %>%
          group_by(name) %>%
          summarise(aggretween = mean(speed)),
        joined %>%
          filter(is.na(name)) %>%
          transmute(name = name,
                    aggretween = speed)) %>%
  st_as_sf()

##

natural_tween <- 
  grouped %>%
  use_series(aggretween) %>%
  classIntervals(n = 9, style = 'jenks') %>%
  use_series(brks)

##

lab <- 
  c(round(natural_tween, 2)) %>% 
  as_tibble() %>% 
  pull()

##

p_speed <- 
  ggplot(grouped %>%
           mutate(group = factor(cut(as.numeric(aggretween), c(natural_tween))))) +
  geom_sf(aes(colour = group, fill = group),
          size = 0.09) +
  scale_fill_manual(values = pal,
                    labels = c(lab[1:8], max(lab)),
                    na.translate = FALSE,
                    name = "speed (km/h)",
                    guide = guide_discrete) +
  scale_color_manual(values = pal,
                     labels = c(lab[1:8], max(lab)),
                     na.translate = FALSE,
                     name = "speed (km/h)",
                     guide = guide_discrete) +
  labs(title = "through-movement", subtitle = "OBSERVED SPEED") +
  theme_bm_legend()

##

ggsave(p_speed, filename = "speed_gla.png", height = 11.7, width = 14, dpi = 300) 





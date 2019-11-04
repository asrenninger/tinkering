##

library(tigris)
library(tidyverse)
library(sf)

##

counties <- c("Sonoma", "Napa", "Solano", "Marin", "Contra Costa", "Alameda", "Santa Clara", "San Mateo", "San Francisco")

##

roads <- 
  reduce(
    map(counties, function(x) {
      roads(x, state = "CA", class = 'sf')
    }), 
    rbind
  )

##

bay <- st_read("data/bay.shp")

##

duplicates <- 
  bay %>% 
  st_equals() %>%
  as_tibble() %>%
  rename(original = row.id,
         duplicate = col.id) %>%
  group_by(original) %>%
  mutate(grouping = max(duplicate)) %>%
  ungroup() %>%
  distinct(grouping)

##

roads <- 
  bay %>% 
  slice(duplicates$grouping)

##

containers <- 
  roads %>% 
  st_contains() %>%
  as_tibble() %>%
  rename(original = row.id,
         duplicate = col.id) %>%
  filter(original != duplicate) 

##

roads <-
  roads %>%
  slice(-unique(containers$duplicate))

##

library(sfnetworks)

##

network <- sfn_asnetwork(roads)
graph <- sfn_network2graph(network)

##

library(igraph)
library(ggraph)

##

ggraph(graph, layout = 'kk') + 
  geom_edge_fan(aes(alpha = stat(index)), show.legend = FALSE) + 
  geom_node_point(aes(), size = 0.1, colour = '#ffffff', alpha = 0.5) +
  theme_bm_legend()

diameter(graph, directed = FALSE)

##

roads$tween <- edge_betweenness(graph, e = E(graph), directed = FALSE)

##

grouped <-
  rbind(roads %>%
          filter(!is.na(FULLNAME)) %>%
          group_by(FULLNAME) %>%
          summarise(aggretween = sum(tween)),
        roads %>%
          filter(is.na(FULLNAME)) %>%
          transmute(FULLNAME = FULLNAME,
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
          plot.margin = margin(10, 10, 10, 10)
    )
  
}

##

p_tween <- 
  ggplot(grouped %>%
           mutate(group = factor(cut(as.numeric(aggretween), c(natural_tween))))) +
  geom_sf(aes(colour = group),
          size = 0.05,
          show.legend = FALSE) +
  scale_colour_manual(values = pal) +
  theme_bm_legend()

##

ggsave(p_tween, filename = "tween.png", height = 18, width = 18, dpi = 300)







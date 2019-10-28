library(tidyverse)
library(sf)

##

colorado <- 
  st_read("data/colorado.shp") %>%
  st_centroid() %>%
  st_transform(3501) %>%
  st_coordinates() %>%
  as_tibble()

##

glimpse(colorado)

##

library("dbscan")

##

clusters <- dbscan(colorado, eps = 1610, minPts = 100, weights = NULL, borderPoints = TRUE)

##

length(clusters$cluster)
unique(clusters$cluster)

clusters$cluster %>%
  enframe(name = NULL) %>%
  group_by(value) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

##

theme_map <- function () {
  theme_void() + 
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major = element_line(size = NA), 
          panel.grid.minor = element_line(size = NA),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15),
          plot.caption = element_text(face = 'bold', colour = 'black'),
          legend.position = 'bottom',
          legend.title = element_text(face = 'bold', colour = 'black'),
          legend.text = element_text(face = 'bold', colour = 'grey50'),
          strip.text = element_text(face = 'bold', colour = 'black'),
          plot.margin = margin(10, 10, 10, 10)
    )
  
}

##

pal <- read_csv("https://github.com/asrenninger/palettes/raw/master/turbo.txt", col_names = FALSE) %>% pull(X1)
fun <- colorRampPalette(pal)

##

num <- length(unique(clusters$cluster))

##

ggplot(colorado, 
         aes(X, Y)) +
  geom_point(aes(colour = clusters$cluster), 
             size = 0.00005,
             show.legend = FALSE) +
  coord_equal() +
  scale_colour_gradientn(colours = c(pal[1], sample(fun(num)[2:num]))) +
  theme_map() +
  ggsave("test.png", height = 20, width = 20, dpi = 300)

##


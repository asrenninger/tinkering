library(tidyverse)
library(sf)

##

colorado <- 
  st_read("data/Colorado.geojson") %>%
  st_centroid() %>%
  st_transform(102003) %>%
  st_coordinates() %>%
  as_tibble()

##

glimpse(colorado)

##

library("dbscan")

##

clusters <- dbscan(colorado, eps = 500, minPts = 1000, weights = NULL, borderPoints = TRUE)

##

length(clusters$cluster)
unique(clusters$cluster)

##

ggplot(colorado, 
       aes(X, Y)) +
  geom_point(aes(colour = factor(clusters$cluster)), 
             size = 0.1,
             show.legend = FALSE)


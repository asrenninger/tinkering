####################################
## Lloyd's Algorithm
####################################

library(tidyverse)
library(sf)
library(gganimate)

## create a bounding box
square <- st_polygon(list(rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))))

## generatea regular tesselation from it
bounds <- 
  square %>% 
  st_make_grid(n = c(25, 25), square = FALSE) %>% 
  st_union() %>% 
  st_combine()

## cast to polygon
bounds <- st_cast(new_bounds, 'POLYGON')

## build a data frame of coordinates for spatstat
framed <-
  bounds %>% 
  st_coordinates() %>% 
  as_tibble()

## generate points from a gaussian kernel so they start close to the middle and need to sort themselves out
points <- tibble(X = rnorm(n = 25 * 25, mean = 0, sd = 0.1), Y = rnorm(n = 25 * 25, mean = 0, sd = 0.1))

## generate point pattern from these points
pointp <- spatstat::ppp(points$X, points$Y, window = spatstat::owin(poly = tibble(x = rev(framed$X), y = rev(framed$Y))))

## create the voronoi tesselation from this pattern
dirich <- 
  spatstat::dirichlet(pointp) %>% 
  st_as_sfc() %>% 
  st_as_sf() %>%
  mutate(iteration = 1) %>%
  rename(geometry = x) %>%
  select(iteration, geometry)

## plot to be sure it worked
plot(dirich)

## iterate through with Lloyd's Algorithm
for (i in 1:500) {
  
  ## create a temporary data frame
  temporary <- filter(dirich, iteration == i)
  
  ## calculate the centroids of those voronoi polygons
  temporary_points <- 
    temporary %>% 
    st_centroid() %>% 
    st_coordinates %>% 
    as_tibble()
  
  ## create a new point pattern
  temporary_pointp <- spatstat::ppp(temporary_points$X, temporary_points$Y, 
                                    window = spatstat::owin(poly = tibble(x = rev(framed$X), y = rev(framed$Y))))  
  
  ## calculate a new voronoi tesselation
  temporary_dirich <- 
    spatstat::dirichlet(temporary_pointp) %>% 
    st_as_sfc() %>% 
    st_as_sf() %>%
    mutate(iteration = i + 1) %>%
    rename(geometry = x) %>%
    select(iteration, geometry)
  
  ## bind it to the original so that we can see the iterations 
  dirich <- rbind(dirich, temporary_dirich)
  
}

## calculate the area of the ideal cell for comparison
regular <- 
  bounds %>%
  st_make_grid(n = c(25, 25), square = FALSE) %>% 
  magrittr::extract2(1) %>% 
  st_area()

## animate it
anim <- 
  ggplot(new_dirich %>% 
           mutate(area = st_area(geometry),
                  diff = (area - regular) / regular)) + 
  geom_sf(aes(fill = factor(ntile(diff, 9))), colour = '#ffffff', show.legend = FALSE) +
  scale_fill_manual(values = scico::scico(9, palette = 'hawaii')) +
  transition_manual(iteration) +
  theme_void()

anim_save("test.gif", animation = anim, fps = 25, start_pause = 5, end_pause = 25, height = 800, width = 800)
########################################
## 30 Days of Maps
########################################

## LINES

library(vroom)
library(sf)
library(fs)

##

files <- dir_ls("data/all", recurse = TRUE)
files <- files[str_detect(files, "routes.geojson")]

##

routes <- 
  reduce(
  map(files, function(x){
    read_sf(x) %>%
      mutate(city = str_sub(x, 38) %>%
               str_extract("[^/]+"))
  }),
  rbind
)

##

library(tmap)
library(tmaptools)
library(glue)

##

map(unique(routes$city), function(x){
  map <- 
    tm_shape(filter(routes, city == x) %>%
               mutate(route_type = factor(route_type))) + 
    tm_lines(
      lwd = 0.1) +
    tm_scale_bar(breaks = c(5),
                 position = c("right","bottom"),
                 size = 0.25) + 
    tm_layout(main.title = glue("{x}"),
              main.title.size = 0.5,
              frame.lwd = 0)
  
  tmap_save(map, glue("miscellany/animations/routes/map_{x}.png"), width = 600, height = 600, unit = 'px')
  
})

##

library(magick)

##

list.files(path = 'miscellany/animations/routes', pattern = "*.png", full.names = TRUE) %>% 
  image_read() %>% 
  image_join() %>% 
  image_animate(fps = 0.5) %>% 
  image_write(glue("combined.gif"))

##

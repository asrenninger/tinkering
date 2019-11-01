library(tigris)
library(sf)

##

options(tigris_use_cache = TRUE)

##

counties <- counties(cb = TRUE, resolution = "500k", class = 'sf')

##

options(scipen = 999)

library(tidyverse)
library(scales)
library(magrittr)
library(classInt)
library(janitor)

##

background <-
  counties %>%
  filter(!str_detect(STATEFP, "15|02|60|66|69|72|78")) %>%
  mutate(dissolve = 1) %>%
  group_by(dissolve) %>%
  summarise()

plot(background)

##

library(raster)

##

band <- raster("data/FBUY/data/FBUY.tif")

plot(band)

##

scissors <- st_transform(background, st_crs(band))

##

extents <- extent(scissors)

##

cropped <- crop(band, extents)
clipped <- mask(cropped, scissors)

##

points <- 
  clipped %>%
  as('SpatialPointsDataFrame') %>%
  st_as_sf(coords = c("x", "y"), remove = FALSE)

##

transformed<- st_transform(points, st_crs(tracts))

##

plot(transformed)

##

library(mapview)

##

transformed %>% 
  mutate(percentile = ntile(LC08_L1TP_014032_20190720_20190731_01_T1_B10, 100)) %>%
  filter(percentile > 95) %>%
  mapview(zcol = "LC08_L1TP_014032_20190720_20190731_01_T1_B10")

##

coordinated <- 
  transformed %>%
  st_coordinates() %>%
  cbind(transformed)

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

guide_continuous <- 
  guide_colorbar(direction = "horizontal",
                 barwidth = unit(50, units = "mm"),
                 barheight = unit(2, units = "mm"),
                 draw.ulim = FALSE,
                 title.position = 'top',
                 label.position = 'bottom',
                 title.hjust = 0.5,
                 label.hjust = 0.5)

##

pal <- read_csv("https://github.com/asrenninger/palettes/raw/master/grered.txt", col_names = FALSE) %>% pull(X1)
fun <- colorRampPalette(pal)

##

hoods <- st_read("https://github.com/azavea/geo-data/raw/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson")

hoods %>%
  mapview()

##

labels <-
  hoods %>%
  st_centroid() %>%
  st_coordinates() %>%
  cbind(hoods) %>%
  st_drop_geometry() %>%
  dplyr::select(X, Y, listname)

##

ggplot() +
  geom_point(data = 
               coordinated %>%
               mutate(percentile = ntile(LC08_L1TP_014032_20190720_20190731_01_T1_B10, 100)),
             aes(x = X, y = Y, colour = percentile),
             size = 0.5) +
  geom_sf(data = water,
          aes(), fill = 'white', 
          colour = NA, size = 0) +
  geom_text(data = labels %>%
              filter(str_detect(listname, "Grays Ferry|Cobbs Creek|Upper Kensington")),
            aes(x = X, y = Y, label = listname),
            colour = 'grey30', fontface = 'bold', alpha = 0.75, size = 5) +
  scale_colour_gradientn(colours = rev(fun(9)),
                         name = "radiance (percentiles)",
                         breaks = c(25, 50, 75), 
                         guide = guide_continuous) +
  labs(title = "20 july 2019",
       subtitle = "surface temperature",
       caption = "source: USGS, processing by author") +
  theme_map() +
  ggsave("temperature.png", height = 8, width = 6, dpi = 300)

##

library(RStoolbox)

##

mtl <- readMeta("data/images/LC08_L1TP_014032_20190720_20190731_01_T1_MTL.txt")

##

bias <- subset(mtl$CALRAD, rownames(mtl$CALRAD) == "B10_dn")

##

radiance <- bias$gain * clipped + bias$offset

##

calibration <- subset(mtl$CALBT, rownames(mtl$CALBT) == "B10_dn")

##

temperature <- (((calibration$K2 / log((calibration$K1 / radiance) + 1)) - 273) * 1.8) + 32

##

plot(temperature)

##


points <- 
  temperature %>%
  as('SpatialPointsDataFrame') %>%
  st_as_sf(coords = c("x", "y"), remove = FALSE)

##

transformed <- st_transform(points, st_crs(tracts))

##

plot(transformed)

##

library(mapview)

##

coordinated <- 
  transformed %>%
  st_coordinates() %>%
  cbind(transformed)

##

ggplot() +
  geom_point(data = coordinated,
             aes(x = X, y = Y, colour = layer),
             size = 0.5) +
  geom_sf(data = water,
          aes(), fill = 'white', 
          colour = NA, size = 0) +
  geom_text(data = labels %>%
              filter(str_detect(listname, "Grays Ferry|Cobbs Creek|Upper Kensington")),
            aes(x = X, y = Y, label = listname),
            colour = 'grey30', fontface = 'bold', alpha = 0.75, size = 5) +
  scale_colour_gradientn(colours = rev(fun(9)),
                         name = "temperature (fahrenheit)",
                         breaks = c(72, 86, 100), 
                         guide = guide_continuous) +
  labs(title = "20 july 2019",
       subtitle = "surface temperature",
       caption = "source: USGS, processing by author") +
  theme_map() +
  ggsave("temperature.png", height = 8, width = 6, dpi = 300)

##

ggplot() +
  geom_sf(data = background,
          aes(),
          fill = 'grey90', size = 0, colour = NA) +
  geom_point(data = coordinated %>%
               mutate(percentile = ntile(layer, 100)) %>%
               filter(percentile > 95),
             aes(x = X, y = Y, colour = layer),
             size = 0.5) +
  geom_sf(data = water,
          aes(), fill = 'white', 
          colour = NA, size = 0) +
  #  geom_text(data = labels %>%
  #              filter(str_detect(listname, "Grays Ferry|Cobbs Creek|Upper Kensington")),
  #            aes(x = X, y = Y, label = listname),
  #            colour = 'grey30', fontface = 'bold', alpha = 0.75, size = 5) +
  scale_colour_gradientn(colours = rev(fun(9)),
                         name = "temperature (fahrenheit)",
                         #      breaks = c(72, 86, 100), 
                         guide = guide_continuous) +
  labs(title = "20 july 2019",
       subtitle = "hot spots",
       caption = "source: USGS, processing by author") +
  theme_map() +
  ggsave("temperature.png", height = 8, width = 6, dpi = 300)

##

min(coordinated$layer)
max(coordinated$layer)
median(coordinated$layer)

##

library(glue)

##

files <- glue("data/images/LC08_L1TP_014032_20190720_20190731_01_T1_B{1:7}.TIF")
length(files)

##

philadelphia <- stack(files)

##

plot(philadelphia)

##

cropped <- crop(philadelphia, extents)
clipped <- mask(cropped, scissors)

##

names(clipped) <- c('ultra-blue', 'blue', 'green', 'red', 'NIR', 'SWIR1', 'SWIR2')

##

plot(clipped)

##

ggRGB(clipped, r = 4, g = 3, b = 2, stretch = "lin") +
  labs(title = "natural colour",
       subtitle = "PHILADELPHIA") +
  theme_map()

##

ggRGB(clipped, r = 5, g = 4, b = 3, stretch = "lin")+
  labs(title = "false colour",
       subtitle = "PHILADELPHIA") +
  theme_map()

##

ggRGB(clipped, r = 5, g = 6, b = 7, stretch = "lin")+
  labs(title = "false colour",
       subtitle = "PHILADELPHIA") +
  theme_map()

##

nd <- function(img, a, b) {
  ba <- img[[a]]
  bb <- img[[b]]
  i <- (ba - bb) / (ba + bb)
  return(i)
}

##

ndvi <- nd(clipped, 5, 4)

##

ndvi %>%
  as('SpatialPointsDataFrame') %>%
  st_as_sf(coords = c("x", "y"), remove = FALSE) %>%
  mutate(percentile = ntile(layer, 100)) %>%
  filter(percentile > 5 & percentile < 95) %>%
  plot()

##

ndbi <- nd(clipped, 6, 5)

##

ndbi %>%
  as('SpatialPointsDataFrame') %>%
  st_as_sf(coords = c("x", "y"), remove = FALSE) %>%
  mutate(percentile = ntile(layer, 100)) %>%
  filter(percentile > 5 & percentile < 95) %>%
  plot()

##

ndwi <- nd(clipped, 3, 4)

##

ndwi %>%
  as('SpatialPointsDataFrame') %>%
  st_as_sf(coords = c("x", "y"), remove = FALSE) %>%
  mutate(percentile = ntile(layer, 100)) %>%
  filter(percentile > 5 & percentile < 95) %>%
  plot()

##

matrix(rbind(cbind(-Inf, 0.3, NA), cbind(1, Inf, NA)), ncol = 3)

##

veg <- reclassify(ndvi, matrix(rbind(cbind(-Inf, 0.3, NA), cbind(1, Inf, NA)), ncol = 3))
cem <- reclassify(ndbi, matrix(rbind(cbind(-Inf, -0.2, NA), cbind(1, Inf, NA)), ncol = 3))
wat <- reclassify(ndwi, matrix(rbind(cbind(-Inf, 0, NA), cbind(1, Inf, NA)), ncol = 3))

plot(veg, main = 'possible vegetation')
plot(cem, main = 'possible payment')
plot(wat, main = 'possible water')

##

built <- 
  ndbi %>%
  as('SpatialPointsDataFrame') %>%
  st_as_sf(coords = c("x", "y"), remove = FALSE)

##

library(hexbin)

##

coordinated %>%
  st_drop_geometry() %>%
  mutate(built = built$layer,
         temperature = layer) %>%
  mutate(trim = ntile(built, 100)) %>%
  filter(built > -0.5 & built < 1) %>%
  ggplot(aes(built, temperature)) +
  geom_hex(bins = 100) +
  geom_smooth(colour = 'grey50', linetype = 2, method = lm, se = FALSE, fullrange = TRUE) +
  scale_fill_gradientn(colours = rev(fun(9)),
                       name = "count",
                       guide = guide_continuous) +
  labs(title = "correlation 0.76 | p < 0.05",
       subtitle = "temperature and built index") + 
  theme_map() +
  ggsave("correlation.png", height = 6, width = 8, dpi = 300)

##

correlating <- 
  coordinated %>%
  st_drop_geometry() %>%
  mutate(built = built$layer,
         temperature = layer) %>%
  mutate(trim = ntile(built, 100)) %>%
  filter(built > -0.5 & built < 1)

##

cor.test(correlating$temperature, correlating$built)

##

plot(philadelphia)

##

vectorised <- getValues(clipped)

str(vectorised)

##

kmncluster <- kmeans(na.omit(vectorised), centers = 10, iter.max = 500, nstart = 5, algorithm="Lloyd")

##

rasterImage(vectorised)

##

knr <- setValues(kmncluster$cluster)

##

colours <- c("#fef65b","#ff0000", "#daa520","#0000ff","#0000ff","#00ff00","#cbbeb5", "#c3ff5b", "#ff7373", "#00ff00", "#808080")

##

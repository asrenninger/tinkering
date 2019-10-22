
library(tidyverse)
library(sf)

counties <- st_read("https://github.com/asrenninger/twentysixteen/raw/master/data-out/counties.geojson",
                    crs = 102003)

##

crosswalk <- read_csv("https://github.com/asrenninger/twentysixteen/raw/master/data-out/crosswalk.csv")
elections <- read_csv("https://github.com/asrenninger/twentysixteen/raw/master/data-out/votes.csv")

##

results <- 
  elections %>%
  filter(year == 2016) %>%
  transmute(GEOID = GEOID,
            clinton_trump = democrat_raw - republican_raw,
            margin = abs(clinton_trump),
            winner = if_else(clinton_trump > 0, "clinton", "trump"))

##

glimpse(results)

##

prepared <- 
  crosswalk %>%
  left_join(results) %>%
  left_join(counties) %>%
  drop_na() %>%
  st_as_sf()

##

plot(prepared)

##

simplified <- rmapshaper::ms_simplify(prepared, keep = 0.05)
plot(simplified)

##

library(cartogram)

##

gram <- cartogram_dorling(mutate(prepared, w = margin / 1694621), weight = "w", m_weight = 0.005, itermax = 100)

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
          strip.text = element_text(face = 'bold', colour = 'black'),
          plot.margin = margin(5, 5, 5, 5)
    )
  
}

##

ggplot(gram) +
  geom_sf(aes(fill = winner), 
          colour = '#ffffff', size = 0.1,
          show.legend = FALSE) +
  scale_fill_manual(values = c('#0015BC', '#FF0000')) +
  theme_map() + 
  ggsave("test.png", height = 5, width = 6, dpi = 300)

##

library(transformr)
library(tweenr)

##

gram_start <- 
  bind_cols(st_normalize(simplified) %>%
              st_cast('POINT'), 
            st_normalize(simplified) %>%
              st_cast('POINT') %>%
              st_coordinates() %>%
              as_tibble())
##

library(scales)

##

ggplot(gram_start, 
       aes(x = X, y = Y, group = GEOID)) +
  geom_polygon(aes(fill = clinton_trump), show.legend = FALSE) +
  scale_fill_gradient2(high = '#0015BC', low = '#FF0000', mid = '#ffffff',
                       midpoint = 0, limits = c(-10000, 10000), oob = squish) +
  theme_map()

##

gram_end <- 
  bind_cols(st_normalize(gram) %>%
              st_cast('POINT'), 
            st_normalize(gram) %>%
              st_cast('POINT') %>%
              st_coordinates() %>%
              as_tibble())

##

ggplot(gram_end, 
       aes(x = X, y = Y, group = GEOID)) +
  geom_polygon(aes(fill = clinton_trump), show.legend = FALSE) +
  scale_fill_gradient2(high = '#0015BC', low = '#FF0000', mid = '#ffffff',
                       midpoint = 0, limits = c(-10000, 10000), oob = squish) +
  theme_map()

##

morph <- tween_polygon(st_drop_geometry(transmute(gram_start, x = X, y = Y, id = GEOID)), 
                       st_drop_geometry(transmute(gram_end, x = X, y = Y, id = GEOID)),
                       ease = 'cubic-in-out',
                       nframes = 20,
                       id = id)

##

glimpse(morph)

##

sample <- prepared %>% filter(state == "PA") %>% pull(GEOID)

##

timing <- 
  prepared %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  transmute(order = ntile(round(X, 0), 100)) %>%
  bind_cols(prepared)

##

toy_data <- 
  morph %>%
  filter(id %in% sample) %>%
  rename(GEOID = id) %>%
  left_join(timing) %>% 
  mutate(.frame = .frame + order)

##

ggplot(toy_data) + 
  geom_polygon(aes(x, y, group = GEOID, fill = winner),
               colour = '#ffffff', size = 0.1,
               show.legend = FALSE) +
  scale_fill_manual(values = c('#0015BC', '#FF0000')) +
  coord_map() + 
  transition_manual(.frame) +
  ease_aes('cubic-in-out') +
  theme_map()

##

library(gganimate)

##

animate <-
  ggplot(everything) + 
  geom_polygon(aes(x, y, group = GEOID, fill = winner),
               colour = '#ffffff', size = 0.1,
               show.legend = FALSE) +
  scale_fill_manual(values = c('#0015BC', '#FF0000')) +
  coord_map() + 
  transition_manual(.frame) +
  ease_aes('cubic-in-out') +
  enter_appear(early = TRUE) +
  exit_disappear(early = FALSE) +
  theme_map()

anim_save("updated.gif", animation = animate, 
          height = 600, width = 700,
          start_pause = 10, end_pause = 10)



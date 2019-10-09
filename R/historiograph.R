library(tidyverse)
library(janitor)

## https://www.census.gov/dataviz/visualizations/007/508.php
##

pal <- read_csv("https://github.com/asrenninger/palettes/raw/master/turbo.txt", col_names = FALSE) %>% pull(X1)

##

theme_ver <- function () {
  theme_minimal() +
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.x = element_line(size = 0.1, colour = 'grey50'),
          panel.grid.major.y = element_line(size = 0.1, colour = 'grey50'),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_line(size = 0.5, colour = 'black'),
          axis.line.y = element_blank(),
          axis.ticks.x = element_line(size = 0.5, colour = 'black'),
          axis.ticks.y = element_line(size = 0.1, colour = 'grey50'),
          axis.text.x = element_text(face = 'bold'),
          axis.text.y = element_blank(),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black'),
          strip.text = element_text(face = 'bold', colour = 'black'),
          plot.margin = margin(20, 20, 20, 20)
    )
}

##

messy <- read_csv("https://github.com/asrenninger/tinkering/raw/master/data/cities.csv",
                  skip = 1)

tidy <- 
  messy %>%
  clean_names() %>%
  select(-x0) %>%
  mutate(metro_area = str_remove_all(city_and_state, ",.*")) %>%
  select(metro_area, everything()) %>%
  gather(year, rank, x1790_rank:x2010_rank) %>%
  mutate(year = str_remove_all(year, "_rank")) %>%
  mutate(year = as.numeric(str_remove_all(year, "x"))) %>%
  filter(rank != 0)
  
##

allyears <- read_csv("https://github.com/asrenninger/tinkering/raw/master/data/cities_historiograph.csv")

##

allyears <- 
  allyears %>%
  group_by(metro_area) %>%
  filter(year == min(year)) %>%
  rename(first = rank,
         started = year) %>%
  select(metro_area, first, started) %>%
  right_join(allyears) %>%
  filter(year == max(year)) %>%
  rename(final = rank,
         finished = year) %>%
  select(metro_area, first, final, started, finished) %>%
  right_join(allyears) %>%
  ungroup() %>%
  mutate(metro_area = str_remove_all(metro_area, pattern = ",.*"))


allyears <- 
  tidy %>%
  group_by(metro_area) %>%
  filter(year == min(year)) %>%
  rename(first = rank,
         started = year) %>%
  select(metro_area, first, started) %>%
  right_join(tidy) %>%
  filter(year == max(year)) %>%
  rename(final = rank,
         finished = year) %>%
  select(metro_area, first, final, started, finished) %>%
  right_join(tidy) %>%
  ungroup()

##

ggplot() +
  geom_line(data = allyears,
            aes(x = year, y = rank, colour = final, group = metro_area)) +
  geom_text(data = 
              allyears %>%
              group_by(metro_area) %>%
              filter(year == max(year)),
            aes(x = year, y = rank, colour = final, label = metro_area),
            fontface = 'bold', hjust = 0) +
  geom_text(data = 
              allyears %>%
              group_by(metro_area) %>%
              filter(year == min(year)),
            aes(x = year, y = rank, colour = final, label = metro_area),
            fontface = 'bold', hjust = 1) +
  scale_colour_gradientn(colours = pal,
                         guide = 'none') +
  xlim(1780, 2020) +
  ylim(0, 20) + 
  scale_y_reverse() +
  theme_ver()

##

library(gganimate)

##

min(allyears$year)
max(allyears$year)

##

animation <- 
  ggplot(allyears, aes(year, rank, colour = final, group = metro_area)) +
  geom_line(alpha = 0.5, size = 1) + 
  geom_segment(aes(xend = finished + (((finished / 2010)^1000) * 20), yend = rank), 
               linetype = 2, colour = 'grey') + 
  geom_text(aes(x = finished + (((finished / 2010)^1000) * 20), colour = final, label = metro_area), 
            hjust = 0,
            fontface = 'bold') + 
  geom_segment(aes(xend = started - (((1790 / started)^1000) * 20), yend = first,
                   x = started, y = first), 
               linetype = 2, colour = 'grey') +
  geom_text(aes(x = started - (((1790 / started)^1000) * 20), y = first, colour = first, label = metro_area), 
            hjust = 1,
            fontface = 'bold') + 
  transition_reveal(year) + 
  coord_cartesian(clip = 'off') + 
  ggtitle(label = "{round({frame_along}, 0)}", subtitle = "RANKED SIZE") + 
  labs(caption = "names are positioned at the decades wherein that city joins or leaves the rankings") +
  ylab("") +
  xlim(1760, 2040) +
  ylim(0, 20) + 
  scale_colour_gradientn(colours = rev(pal), 
                         guide = 'none') +
  scale_y_reverse() +
  theme_ver()

##

animate(animation, fps = 5, start_pause = 5, end_pause = 20,
        height = 500, width = 900)

anim_save(filename = "race.gif", path = "viz")

##

read_csv("data/urbanism.csv", skip = 1) %>%
  clean_names() %>%
  select(-x1) %>%
  gather(year, urbanisation, x2010:x1790) %>%
  mutate(state_territory = str_remove_all(state_territory, "\\[.*\\]"),
         year = as.numeric(str_remove_all(year, "x")),
         urbanisation = as.numeric(str_remove_all(urbanisation, "%"))) %>%
  ggplot(aes(year, urbanisation)) +
  geom_line(data = . %>% filter(state_territory != "United States"),
            aes(group = state_territory),
            colour = 'grey40', alpha = 0.25, size = 0.5) +
  geom_line(data = . %>% filter(state_territory == "United States"),
            colour = pal[9], size = 2) +
  labs(title = "states behind the national average",
       subtitle = "PROPORTION URBAN",
       caption = "") +
  theme_hor() +
  ggsave("urbanisation.png", height = 5, width = 6, dpi = 300)
  
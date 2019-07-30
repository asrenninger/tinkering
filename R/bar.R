library(tidyverse)
library(testit)

##

has_error(read_csv("https://www.ncdc.noaa.gov/cag/statewide/time-series/1-tavg-12-12-1900-2019.csv?base_prd=true&begbaseyear=1900&endbaseyear=1910", skip = 4))

##

library(tidycensus)

##

states <- 
  fips_codes %>% 
  filter(!str_detect(state_code, "02|15|60|66|69|72|78|11")) %>% 
  select(state_name) %>%
  distinct() %>%
  slice(1:48) %>%
  arrange()

##

library(glue)

##

data <- reduce(
  map(1:48, function(x) {
    mutate(read_csv(glue("https://www.ncdc.noaa.gov/cag/statewide/time-series/{x}-tavg-12-12-1900-2019.csv?base_prd=true&begbaseyear=1900&endbaseyear=1910"),
             skip = 4),
           state = states[x, ])
  }), 
  bind_rows
)

##

data <- reduce(
  map(1:3, function(x) {
    glue("https://www.ncdc.noaa.gov/cag/statewide/time-series/{x}-tavg-12-12-1900-2019.csv?base_prd=true&begbaseyear=1900&endbaseyear=1910") %>%
      read_csv(skip = 4) %>% 
      mutate(state = states[x, ])
  }), 
  bind_rows
)

##

glimpse(data)

##

library(janitor)
library(lubridate)

##

cleaned <- 
  data %>% 
  clean_names() %>%
  mutate(date = str_sub(date, 1, 4) %>% 
           str_c("01-01", sep = "-") %>%
           ymd()) %>%
  group_by(state) %>%
  mutate(avg = mean(anomaly)) %>%
  ungroup() %>%
  mutate(dev = mean(anomaly))

joined <- 
  fips_codes %>%
  select(state, state_name) %>%
  rename(abbrev = state, state = state_name) %>%
  group_by(state) %>%
  slice(1) %>%
  right_join(cleaned)


##

library(RColorBrewer)

##

col_strip <- brewer.pal(11, "RdBu")

##

library(geofacet)

##

theme_map <- function () {
  theme_void() + 
    theme(plot.background = element_rect(fill = 'white', colour = 'white'),
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
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15),
          plot.margin = margin(5, 5, 5, 5, "mm"),
          strip.text = element_blank(),
          strip.background = element_blank(),
          panel.grid.major = element_line(size = NA), 
          panel.grid.minor = element_line(size = NA),
          panel.spacing = unit(1, "mm")
    )
  
}

##

ggplot(joined %>%
         mutate(lab = str_replace(state, pattern = " ", replacement = "\n"))) +
  geom_tile(aes(x = date, y = 1, fill = anomaly), 
            show.legend = FALSE) +
  geom_text(aes(x = as_date(1900), y = 1, label = abbrev, colour = avg),
            hjust = 1,
            vjust = 0,
            size = 6,
            show.legend = FALSE) +
  scale_x_date(date_breaks = "6 years",
               date_labels = "%Y",
               expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(colors = rev(col_strip)) +
  scale_color_gradientn(colors = rev(col_strip)) +
  facet_geo(~ state, grid = "us_state_grid1") +
  theme_map() +
  ggsave("test.png", height = 6, width = 10, dpi = 300)


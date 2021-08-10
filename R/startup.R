########################################
## Unicorns
########################################

## packages 
library(tidyverse)
library(sf)
library(rvest)
library(ggmap)
library(ggshadow)
library(ggnewscale)

## unicorns
wiki <- read_html("https://en.wikipedia.org/wiki/List_of_unicorn_startup_companies")
elem <- html_nodes(wiki,"#mw-content-text > div.mw-parser-output > table:nth-child(17)")

unicorns <-
  rvest::html_table(elem) %>% 
  magrittr::extract2(1) %>%
  tibble::as_tibble() %>%
  janitor::clean_names()

links <- 
  reduce(map(1:nrow(unicorns), function(x){
    rvest::html_node(elem, glue("tbody > tr:nth-child({x+1}) > td > a")) %>% 
      rvest::html_attr(name = "href") } 
  ), 
  c)

unicorns_linked <- 
  unicorns %>% 
  mutate(reference = links) %>%
  mutate(link = case_when(str_sub(reference, 1, 1) == "/" ~ str_c("https://en.wikipedia.org", reference),
                          TRUE ~ reference)) %>%
  mutate(link = case_when(str_detect(link, "redlink") ~ "none", 
                          TRUE ~ link)) %>%
  mutate(link = na_if(link, "none")) %>%
  drop_na()

unicorns_filtered <- filter(unicorns_linked, str_detect(link, "wikipedia"))

unicorns_info <-
  map_df(1:nrow(unicorns_filtered), possibly(function(x){ 
    link <- unicorns_filtered$link[x]
    wiki <- read_html(link)
    elem <- rvest::html_nodes(wiki,"#mw-content-text > div.mw-parser-output > table.infobox.vcard")
    tibl <- 
      elem %>% 
      rvest::html_table() %>% 
      magrittr::extract2(1) %>% 
      as_tibble() %>% 
      set_names(c("field", "value")) %>% 
      mutate(company = unicorns_filtered$company[x])
    return(tibl)
  }, 
  tibble("field" = NA, "value" = NA, company = unicorns_filtered$company[x])
  )
  )

unicorns_geocoded <- 
  unicorns_info %>% 
  filter(field == "Headquarters") %>%
  select(-field) %>% 
  rename(headquarters = value) %>% 
  mutate_geocode(location = headquarters, source = "google", output = "latlon")

unicorns_geocoded %>% drop_na(lon, lat) %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview::mapview()

## decacorns
wiki <- read_html("https://en.wikipedia.org/wiki/List_of_unicorn_startup_companies")
elem <- rvest::html_nodes(wiki,"#mw-content-text > div.mw-parser-output > table:nth-child(20)")

decacorns <- 
  rvest::html_table(elem) %>% 
  magrittr::extract2(1) %>%
  tibble::as_tibble() %>%
  janitor::clean_names()

links <- 
  reduce(map(1:nrow(decacorns), function(x){
    rvest::html_node(elem, glue("tbody > tr:nth-child({x+1}) > td > a")) %>% 
      rvest::html_attr(name = "href") } 
  ), 
  c)

decacorns_linked <- 
  decacorns %>% 
  mutate(reference = links) %>%
  mutate(link = case_when(str_sub(reference, 1, 1) == "/" ~ str_c("https://en.wikipedia.org", reference),
                          TRUE ~ reference)) %>%
  mutate(link = case_when(str_detect(link, "redlink") ~ "none", 
                          TRUE ~ link)) %>%
  mutate(link = na_if(link, "none")) %>%
  drop_na()

decacorns_filtered <- filter(decacorns_linked, str_detect(link, "wikipedia"))

decacorns_info <-
  map_df(1:nrow(decacorns_filtered), possibly(function(x){ 
    link <- decacorns_filtered$link[x]
    wiki <- read_html(link)
    elem <- rvest::html_nodes(wiki,"#mw-content-text > div.mw-parser-output > table.infobox.vcard")
    tibl <- 
      elem %>% 
      rvest::html_table() %>% 
      magrittr::extract2(1) %>% 
      as_tibble() %>% 
      set_names(c("field", "value")) %>% 
      mutate(company = decacorns_filtered$company[x])
    return(tibl)
  }, 
  tibble("field" = NA, "value" = NA, company = decacorns_filtered$company[x])
  )
  )

unicorns_geocoded <-
  unicorns_info %>%
  filter(field == "Headquarters") %>%
  mutate(value = str_remove_all(value, "\\n")) %>%
  mutate(value = str_remove_all(value, "\\[.*?\\]")) %>%
  mutate(value = str_remove_all(value, "\\(.*")) %>%
  mutate(value = str_remove_all(value, "and.*")) %>%
  mutate(value = str_remove_all(value, ".mw.*")) %>%
  select(-field) %>% 
  rename(city = value) %>% 
  mutate(headquarters = glue("{company}, {city}")) %>%
  mutate_geocode(location = headquarters, source = "google", output = "latlon")

decacorns_geocoded <- 
  decacorns_info %>% 
  filter(field == "Headquarters") %>%
  mutate(value = str_remove_all(value, "\\n")) %>%
  mutate(value = str_remove_all(value, "\\[.*?\\]")) %>%
  mutate(value = str_remove_all(value, "\\(.*")) %>%
  mutate(value = str_remove_all(value, "and.*")) %>%
  select(-field) %>% 
  rename(city = value) %>% 
  mutate(headquarters = glue("{company}, {city}")) %>%
  mutate_geocode(location = headquarters, source = "google", output = "latlon")

bind_rows(decacorns_geocoded, unicorns_geocoded) %>% write_csv("startups.csv")

info <- 
  bind_rows(unicorns %>% 
              transmute(company, valuation = valuation_us_billion),
            decacorns %>% 
              transmute(company, valuation = last_valuation_us_b))

## plot it
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
          legend.position = 'bottom',
          plot.margin = margin(10, 10, 10, 10),
    )
  
}

bind_rows(decacorns_geocoded, unicorns_geocoded) %>% 
  drop_na(lon, lat) %>%
  left_join(info) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs") %>% 
  ggplot() +
  geom_sf(data = coastline,
          aes(),
          colour = '#c7c7c7', size = 0.1, linetype = 3) +
  geom_glowpoint(aes(geometry = geometry, size =  parse_number(valuation)), 
                 alpha = .8,
                 color = "#6bb857",
                 shadowcolour = "#0062ff",
                 shadowalpha = .1,
                 stat = "sf_coordinates",
                 show.legend = FALSE) +
  scale_size(range = c(.1, 1.5)) +
  new_scale("size") +
  geom_glowpoint(aes(geometry = geometry, size = parse_number(valuation)), 
                 alpha = .6,
                 shadowalpha = .05,
                 color = "#ffffff",
                 stat = "sf_coordinates",
                 show.legend = FALSE) +
  scale_size(range = c(0.1, 0.7)) +
  labs(title = 'Technology \"Unicorns\"', 
       subtitle = "Exited or valued above $1 billion") + 
  theme_bm_legend() +
  ggsave(filename = "startups.png", height = 6, width = 10.37, dpi = 300)


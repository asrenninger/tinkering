
library(readxl)

##

russell <- read_xlsx("data/russell_3000.xlsx")

##

library(tidyverse)
library(janitor)
library(glue)

##

russell_clean <- 
  russell %>%
  clean_names() %>%
  mutate(location = glue("{name} corporate headquarters")) %>%
  mutate(location = str_replace_all(location, pattern = " Class A | Class C | Class B | A | ", replacement = " ")) %>%
  mutate(location = str_remove_all(location, pattern = "Corp |Inc |[:alpha:] Co |Ltd |PLC ")) %>% 
  mutate(location = str_remove_all(location, pattern = "Registered Shs Series -1-"))

##

library(ggmap)

##

register_google(key = "YOURKEY")

##

russell_geocoded <-
  russell_clean %>%
  mutate_geocode(location = location, source = "google", output = "latlon")

##

russell_geocoded %>%
  drop_na(lat, lon) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  mapview::mapview(zcol = "sector")

##

russell_symbols <-
  russell_geocoded %>%
  mutate(ticker = str_remove_all(ticker, pattern = "\\.[:alpha:]"))

##

library(tidyquant)

##

aapl_prices <- tq_get(russell_symbols$ticker[1], get = "stock.prices", from = " 1990-01-01")
msft_prices <- tq_get(russell_symbols$ticker[2], get = "stock.prices", from = " 1990-01-01")
amzn_prices <- tq_get(russell_symbols$ticker[3], get = "stock.prices", from = " 1990-01-01")

##

grab_stocks <- 
  function(x){
    ticker <- russell_symbols$ticker[x]
    tq_get(ticker, 
           get = "stock.prices", from = "1990-01-01") %>%
      mutate(ticker = ticker)
  }

##

possible_get <- possibly(grab_stocks, otherwise = NULL)

##

trading_data <- 
  reduce(
    map(1:nrow(russell_symbols), function(x) {
      possible_get(x)
    }), 
    rbind
  )

##

files <- c("russell_1000.csv", "russell_2000.csv", "russell_3000.csv", "russell_misses.csv")

##

trading_data <- 
  reduce(
    map(1:4, function(x) {
      glue("data/{files[x]}") %>%
      read_csv()
    }), 
    rbind
  )

##

library(lubridate)

##

trading_series <-
  trading_data %>%
  mutate(date_round = floor_date(date, "month")) %>%
  group_by(date_round, ticker) %>% 
  summarise(close = mean(close, na.rm = TRUE))

##

library(tigris)
library(sf)

##

states <- states(class = 'sf')

geodata <- 
  read_csv("data/russell_geocoded.csv") %>%
  mutate(ticker = str_remove_all(ticker, pattern = "\\.[:alpha:]")) %>%
  drop_na(lon, lat) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4236) %>%
  st_transform(st_crs(states)) %>%
  st_join(states) %>%
  st_drop_geometry()

##

library(geofacet)

##

trading_series %>%
  group_by(ticker) %>%
  add_tally() %>%
  ungroup() %>%
  filter(n == 30) %>%
  filter(!str_detect(ticker, "YRCW|ABEO")) %>%
  arrange(desc(close)) %>%
  distinct(ticker)

ggplot() +
  geom_line(data = 
              trading_series %>%
              group_by(ticker) %>%
              add_tally() %>%
              ungroup() %>%
              filter(n == 30) %>%
              group_by(ticker) %>%
              mutate(height = max(close)) %>%
              filter(height < 1500) %>%
              mutate(z_score = scale(close)),
            aes(date_round, z_score, group = ticker)) +
  geom_line(data = trading_series %>%
              group_by(ticker) %>%
              add_tally() %>%
              ungroup() %>%
              filter(n == 30) %>%
              group_by(ticker) %>%
              mutate(height = max(close)) %>%
              filter(height < 1500) %>%
              mutate(z_score = scale(close)) %>%
              filter(ticker == "AAPL"),
            aes(date_round, z_score, group = ticker), colour = '#183713')

ggplot(trading_series %>%
         group_by(ticker) %>%
         add_tally() %>%
         ungroup() %>%
         filter(n == 30) %>%
         group_by(ticker) %>%
         mutate(height = max(close)) %>%
         filter(height < 1500) %>%
         mutate(z_score = scale(close))%>%
         left_join(select(geodata, ticker, REGION, NAME)) %>%
         drop_na(NAME) %>%
         group_by(NAME, date_round) %>%
         summarise(avg = mean(z_score)),
       aes(date_round, avg)) +
  geom_line() +
  facet_geo(~ NAME) +
  theme_hor() +
  ggsave("statebystate.png", height = 8, width = 11, dpi = 300)

ggplot(trading_series %>%
         group_by(ticker) %>%
         add_tally() %>%
         ungroup() %>%
         filter(n == 30) %>%
         group_by(ticker) %>%
         mutate(height = max(close)) %>%
         filter(height < 1500) %>%
         mutate(z_score = scale(close))%>%
         ungroup() %>%
         left_join(select(geodata, ticker, REGION, NAME)),
       aes(date_round, z_score)) +
  geom_line() +
  facet_geo(~ NAME) +
  theme_hor()
  ggsave("statebystate.png", height = 8, width = 11, dpi = 300)

##

plot(states)

##




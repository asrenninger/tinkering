
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

ggplot(trading_series %>%
         filter(date_round > as_date("2009-01-01")) %>%
         left_join(select(geodata, ticker, REGION, NAME)) %>%
         drop_na(NAME) %>%
         group_by(NAME, date_round) %>%
         summarise(avg = mean(close),
                   dif = var(close)),
       aes(date_round, avg)) +
  geom_line() +
  geom_ribbon(aes(ymax = avg + dif / 2, ymin = avg - dif / 2)) +
  facet_geo(~ NAME, scales = 'free')

##

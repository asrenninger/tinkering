########################################
## dow jones
########################################

## packages 
library(tidyverse)
library(sf)
library(rvest)
library(ggmap)

## dow jones
url <- "https://en.wikipedia.org/wiki/Historical_components_of_the_Dow_Jones_Industrial_Average"

## scraping
wiki <- read_html(url)
elem <- html_nodes(wiki,".mw-headline")

heads <- map_chr(elem, ~html_text(.x))
heads <- heads[1:57]
heads <- lubridate::as_date(heads, format = "%B %d, %Y")

elem <- html_nodes(wiki,".wikitable")

texts <- map_chr(elem, ~html_text(.x))
texts <- texts[1:57]

full <- tibble(year = seq(last(heads), first(heads), by = "day"))

str_remove_all(texts, "\nDropped from Average\n.*") |> pluck(1)

combined <-
  tibble(year = heads,
         companies = texts) |>
  mutate(companies = str_remove_all(companies, "\nDropped from Average\n.*")) |>
  right_join(full) |>
  arrange(desc(year)) |>
  fill(companies, .direction = "up") |>
  separate_rows(companies, sep = "\n\n") |>
  separate_rows(companies, sep = "\n") |>
  mutate(companies = str_remove_all(companies, "\\((.*?)\\)")) |>
  mutate(companies = str_remove_all(companies, " ↑")) |>
  mutate(companies = str_remove_all(companies, " $")) |>
  mutate(companies = str_remove_all(companies, "\\[(.*?)\\]"))

combined |>
  group_by(companies) |>
  summarise(n = n()) |>
  arrange(desc(n)) |>
  filter(companies != "") |>
  slice(1:50) |>
  mutate(companies = str_remove(companies, " Company| Corporation|, Inc.")) |>
  mutate(companies = str_remove(companies, ", Ltd.")) |>
  ggplot(aes(reorder(companies, n), n)) +
  geom_bar(stat = 'identity') +
  labs(x = "", y = "", title = "Days on the Dow Jones Industrial Average") +
  coord_flip() +
  theme_minimal()



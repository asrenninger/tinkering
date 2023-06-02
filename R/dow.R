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

elem <- html_nodes(wiki,".wikitable")

texts <- map_chr(elem, ~html_text(.x))
texts <- texts[1:57]

combined <-
  tibble(year = heads,
         companies = texts) |>
  separate_rows(companies, sep = "\n\n") |>
  separate_rows(companies, sep = "\n") |>
  mutate(companies = str_remove_all(companies, "\\((.*?)\\)")) |>
  mutate(companies = str_remove_all(companies, " ↑")) |>
  mutate(companies = str_remove_all(companies, " $")) |>
  mutate(companies = str_remove_all(companies, "\\[(.*?)\\]"))



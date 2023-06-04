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
  mutate(companies = str_remove_all(companies, " †")) |>
  mutate(companies = str_remove_all(companies, " $")) |>
  mutate(companies = str_remove_all(companies, "\\[(.*?)\\]"))

dow <- 
  combined |>
  group_by(companies) |>
  summarise(n = n(), 
            reign = mean(year)) |>
  mutate(reign = lubridate::year(reign), 
         reign = round(reign, -1)) |>
  arrange(desc(n)) |>
  filter(companies != "") |>
  slice(1:50) |>
  mutate(companies = str_remove(companies, " Company| Corporation|, Inc.")) |>
  mutate(companies = str_remove(companies, ", Ltd.")) |>
  mutate(companies = str_remove(companies, "The ")) |>
  mutate(companies = str_remove(companies, " &")) |>  
  ggplot(aes(reorder(companies, n), n, fill = factor(reign))) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = scico::scico(palette = 'hawaii', n = length(seq(1910, 2000, 10))), name = 'peak decade of reign\n(average of years)') + 
  labs(x = "", y = "", title = "Days on the Dow Jones Industrial Average") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(face = 'bold', size = 18),
        legend.position = c(0.8, 0.3),
        legend.title = element_text(face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.1, colour = '#000000'),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())

dow

links <- map_chr(elem[1], ~html_attr(.x, name = "href"))

links <- 
  tibble(label = elem |> html_nodes("a") |> html_text(),
       link = elem |> html_nodes("a") |> html_attr("href")) |> 
  filter(!str_detect(label, "(Preferred)"),
         !str_detect(label, "[1-9]")) |>
  mutate(link = glue::glue("https://en.wikipedia.org{link}"))

info <- 
  map(1:nrow(links), safely(function(x){ 
    link <- links$link[x]
    wiki <- read_html(link)
    elem <- rvest::html_nodes(wiki,"#mw-content-text > div.mw-parser-output > table.infobox.vcard")
    tibl <- 
      elem |>
      rvest::html_table() |> 
      magrittr::extract2(1) |> 
      as_tibble() |> 
      set_names(c("field", "value")) |>
      mutate(company = links$label[x])
    return(tibl)
  }))

info <- map_dfr(discard(info, ~!is.null(.$error)), "result") 
info |>
  filter(field == "Headquarters") |>
  rename(hq = value) |>
  select(company, hq)



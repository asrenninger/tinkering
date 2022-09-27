###############################################
## scraping conflicts of interest in parliament
###############################################

## packages
library(tidyverse)
library(sf)
library(rvest)

## start with the list of mps 
# page
interest <- read_html("https://publications.parliament.uk/pa/cm/cmregmem/220905/contents.htm")
# only the text
all_text <- html_nodes(interest,"#mainTextBlock > p")

# iterate through text blocks and grab links
links <- 
  reduce(
    map(4:length(all_text), 
        function(x){
          html_nodes(interest, glue::glue("#mainTextBlock > p:nth-child({x}) > a")) |>
            # href = link
            rvest::html_attr(name = 'href')
        }),
    c)

# top links are just an index, delineated by # sign
links <- links[!str_detect(links, "\\#")]

## iterated over the links with the below function
all_conflicts <- map_dfr(links, scrape_conflicts)
all_conflicts

## look at the data
# filter for those with a property
with_conflict <- 
  conflicts_plot <- 
  all_conflicts |>
  filter(str_detect(category, "Land and property portfolio")) |>
  group_by(name) |>
  summarise(n = n()) |>
  ungroup() 

# get all mps and join in the conflicted ones
both_conflict <- 
  all_conflicts |>
  distinct(name) |>
  left_join(with_conflict) |>
  replace_na(list(n = 0)) |>
  mutate(conflict = if_else(n > 0, "conflicted", "not conflicted"))

# what is the ratio
both_conflict |>
  group_by(conflict) |>
  summarise(n = n())

177 / 394

# plot it
conflicts_plot <- 
  all_conflicts |>
  filter(str_detect(category, "Land and property portfolio")) |>
  group_by(name) |>
  summarise(n = n()) |>
  ungroup() |>
  arrange(desc(n)) |>
  ggplot(aes(reorder(str_trim(name), n), n, fill = n)) +
  geom_bar(colour = '#ffffff', size = 0.1, stat = 'identity', show.legend = FALSE) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 6, name = 'Greens')) +
  coord_flip() + 
  labs(x = "", y = "", 
       title = "Real Estate Conflicts in the Commons",
       subtitle = "Count of Registered Properties, with 177 in 394 MPs owning") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(size = 0.25, colour = '#000000'),
        plot.title = element_text(face = 'bold'),
        plot.subtitle = element_text(size = 12, colour = '#4a4a4a'),
        plot.background = element_rect('#ffffff'))

ggsave(filename = "conflicts.png", height = 20, width = 10, dpi = 300)

## here is where all the magic happens
scrape_conflicts <-
  function(endpoint){
    
    # find the mps conflicts page by taking the root and pasting the link
    link <- glue::glue("https://publications.parliament.uk/pa/cm/cmregmem/220905/{endpoint}")
    # read the page
    mp <- read_html(link)
    
    # grab the name, which is labelled in the css
    name  <- html_nodes(mp, ".RegisterOfInterestsMemberHeader")
    spacer <- html_nodes(mp, ".spacer")
    message(html_text(name))
    
    # remove a bunch of annoying parts of the tree
    # html_nodes(mp,"#mainTextBlock > p")
    xml2::xml_remove(name)
    # html_nodes(mp,"#mainTextBlock > p")
    xml2::xml_remove(spacer)
    # html_nodes(mp,"#mainTextBlock > p")
    
    # this function will help us process the results
    split_at <- 
      function(x, pos, lab) { 
        split_x <- split(x, cumsum(seq_along(x) %in% pos))
        names(split_x) <- lab
        return(split_x)
      } 
    
    # grab all text
    texts <- html_nodes(mp,"#mainTextBlock > p") |> html_text()
    # find just the bold text (which demarcates the class of conflicted)
    interests <- html_nodes(mp,"#mainTextBlock > p > strong") |> html_text()
    # split the text vector into a list with each item a vector of conflicts by class
    conflicts <- split_at(texts, match(interests, texts), interests)
    
    # turn it into a data frame
    mpdf <- 
      purrr::imap_dfr(conflicts, 
                      function(x, y){
                        tibble::tibble(category = str_remove_all(y, "[0-9]\\. |\\(.*\\) "),
                                       conflict = x) |>
                          filter(!conflict %in% y)
                      }) |>
      mutate(mp = html_text(name)) |>
      separate(mp, into = c("name", "district"), sep = " \\(") |>
      mutate(district = str_remove(district, "\\)"))
    
    # kick it out
    return(mpdf)
    
  }


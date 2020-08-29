###############################################
## Scraping and mapping from Chernick et al. 

datapasta::tribble_paste()

tib <- tibble::tribble(
  ~City.State.Less.Severe.Scenario.More.Severe.Scenario,
  "Chattanooga TN 4.9% 8.6%",
  "Knoxville TN 4.9% 8.1%",
  "Memphis TN 5.1% 8.5%",
  "Nashville TN 4.9% 8.1%",
  "Arlington TX 3.8% 7.5%",
  "Austin TX 3.2% 6.3%",
  "Corpus Christi TX 3.3% 6.7%",
  "Dallas TX 3.3% 6.6%",
  "El Paso TX 3.2% 6.9%",
  "Ft. Worth TX 3.6% 7.2%",
  "Garland TX 3.0% 6.5%",
  "Houston TX 3.4% 6.6%",
  "Lubbock TX 3.9% 7.9%",
  "San Antonio TX 3.5% 7.0%",
  "Provo UT 3.6% 6.4%",
  "Salt Lake City UT 3.8% 7.1%",
  "Chesapeake VA 5.8% 8.6%",
  "Norfolk VA 6.1% 9.4%",
  "Richmond VA 5.4% 8.3%",
  "Virginia Beach VA 5.8% 8.8%",
  "Burlington VT 5.6% 8.9%",
  "Rutland VT 5.8% 8.3%",
  "Seattle WA 6.7% 11.5%",
  "Spokane WA 6.3% 10.3%",
  "Tacoma WA 6.3% 10.6%",
  "Madison WI 4.2% 6.6%",
  "Milwaukee WI 5.7% 8.8%",
  "Charleston WV 7.2% 12.4%",
  "Huntington WV 6.4% 11.1%",
  "Casper WY 2.2% 3.9%",
  "Cheyenne WY 3.2% 6.3%"
)

tib <- tibble::tribble(
  ~City.State.Less.Severe.Scenario.More.Severe.Scenario,
  "Billings MT 6.0% 9.3%",
  "Missoula MT 5.8% 8.5%",
  "Charlotte NC 5.3% 9.3%",
  "Durham NC 4.0% 6.6%",
  "Greensboro NC 5.0% 8.0%",
  "Raleigh NC 5.1% 8.3%",
  "Bismarck ND 3.5% 8.3%",
  "Fargo ND 3.3% 8.3%",
  "Lincoln NE 4.5% 7.5%",
  "Omaha NE 4.7% 7.8%",
  "Manchester NH 3.2% 5.4%",
  "Nashua NH 2.6% 4.5%",
  "Albuquerque NM 3.1% 5.5%",
  "Las Cruces NM 4.1% 7.3%",
  "Las Vegas NV 7.1% 11.5%",
  "Reno NV 6.5% 10.5%",
  "Buffalo NY 13.7% 19.7%",
  "New York NY 9.4% 13.9%",
  "Rochester NY 13.8% 19.9%",
  "Syracuse NY 13.5% 19.3%",
  "Yonkers NY 10.1% 15.3%",
  "Akron OH 6.4% 10.0%",
  "Cincinnati OH 6.2% 9.5%",
  "Cleveland OH 6.4% 10.4%",
  "Columbus OH 5.6% 8.8%",
  "Dayton OH 6.3% 10.0%",
  "Toledo OH 6.5% 10.3%",
  "Oklahoma OK 7.1% 11.1%",
  "Tulsa OK 6.1% 9.9%",
  "Eugene OR 3.5% 7.1%",
  "Portland OR 4.2% 8.2%",
  "Salem OR 3.1% 7.0%",
  "Philadelphia PA 8.0% 12.0%",
  "Pittsburgh PA 7.0% 10.6%",
  "Providence RI 6.1% 8.1%",
  "Warwick RI 3.2% 4.8%",
  "Charleston SC 7.6% 12.6%",
  "Columbia SC 5.5% 9.0%",
  "Rapid City SD 2.1% 3.9%",
  "Sioux Falls SD 2.1% 3.9%"
)

tib <- tibble::tribble(
  ~City.State.Less.Severe.Scenario.More.Severe.Scenario,
  "Anchorage AK 2.0% 3.6%",
  "Fairbanks AK 2.3% 4.1%",
  "Birmingham AL 4.8% 8.0%",
  "Mobile AL 4.4% 7.7%",
  "Montgomery AL 4.8% 8.8%",
  "Ft. Smith AR 7.0% 9.4%",
  "Little Rock AR 6.4% 9.1%",
  "Mesa AZ 6.1% 9.2%",
  "Phoenix AZ 5.6% 8.6%",
  "Tucson AZ 5.5% 8.4%",
  "Anaheim CA 5.1% 9.1%",
  "Bakersfield CA 4.5% 7.9%",
  "Fremont CA 4.6% 8.2%",
  "Fresno CA 4.8% 8.5%",
  "Huntington Beach CA 4.5% 8.0%",
  "Long Beach CA 4.5% 8.2%",
  "Los Angeles CA 4.7% 8.5%",
  "Modesto CA 5.0% 8.9%",
  "Oakland CA 4.3% 7.8%",
  "Riverside CA 4.9% 8.8%",
  "Sacramento CA 5.1% 9.0%",
  "San Diego CA 4.6% 8.2%",
  "San Francisco CA 5.5% 9.7%",
  "San Jose CA 4.4% 8.1%",
  "Santa Ana CA 4.6% 8.1%",
  "Stockton CA 4.8% 8.6%",
  "Aurora CO 5.3% 9.0%",
  "Colorado Springs CO 6.2% 10.4%",
  "Denver CO 5.8% 10.3%",
  "Bridgeport CT 2.1% 4.3%",
  "Hartford CT 2.8% 5.6%",
  "New Haven CT 2.5% 4.9%",
  "Washington DC 5.6% 8.8%",
  "Dover DE 5.7% 9.6%",
  "Wilmington DE 5.7% 9.8%",
  "Ft. Lauderdale FL 7.1% 11.7%",
  "Hialeah FL 6.8% 11.0%",
  "Jacksonville FL 8.4% 12.6%",
  "Miami FL 6.6% 10.9%"
)

tib <- tibble::tribble(
  ~City.State.Less.Severe.Scenario.More.Severe.Scenario,
  "Orlando FL 8.3% 13.1%",
  "St. Petersburg FL 6.6% 10.4%",
  "Tallahassee FL 8.3% 13.0%",
  "Tampa FL 6.8% 10.3%",
  "Atlanta GA 4.3% 7.9%",
  "Columbus GA 3.9% 7.3%",
  "Cedar Rapids IA 4.0% 7.1%",
  "Des Moines IA 4.3% 7.5%",
  "Boise ID 5.7% 8.9%",
  "Nampa ID 6.7% 9.9%",
  "Aurora IL 6.9% 10.0%",
  "Chicago IL 7.3% 11.5%",
  "Ft. Wayne IN 6.7% 10.6%",
  "Gary IN 5.6% 9.5%",
  "Indianapolis IN 6.6% 11.3%",
  "Kansas City KS 8.9% 13.2%",
  "Topeka KS 6.9% 10.5%",
  "Wichita KS 9.3% 13.9%",
  "Lexington KY 6.8% 10.4%",
  "Louisville KY 7.2% 10.9%",
  "Baton Rouge LA 8.3% 12.5%",
  "New Orleans LA 7.7% 12.3%",
  "Shreveport LA 11.1% 16.0%",
  "Boston MA 2.2% 4.2%",
  "Springfield MA 1.3% 3.2%",
  "Worcester MA 1.4% 3.2%",
  "Baltimore MD 4.6% 7.3%",
  "Frederick MD 4.7% 7.4%",
  "Lewiston ME 5.9% 9.0%",
  "Portland ME 3.1% 5.5%",
  "Detroit MI 9.5% 14.2%",
  "Flint MI 8.6% 13.5%",
  "Grand Rapids MI 8.2% 12.0%",
  "Warren MI 8.3% 12.3%",
  "Minneapolis MN 3.9% 7.5%",
  "St. Paul MN 3.4% 6.6%",
  "Kansas City MO 7.1% 11.3%",
  "St. Louis MO 6.3% 10.0%",
  "Gulfport MS 5.4% 9.6%",
  "Jackson MS 5.1% 7.6%"
)

##

uno <- 
  tib %>%
  set_names(c("all")) %>%
  mutate(state = str_extract(all, pattern = "[A-Z][A-Z]")) %>%
  separate(all, sep = " [A-Z][A-Z] ", into = c("city", "stats")) %>%
  separate(stats, sep = " ", into = c("mild", "severe")) %>%
  select(city, state, mild, severe)

dos <- 
  tib %>%
  set_names(c("all")) %>%
  mutate(state = str_extract(all, pattern = "[A-Z][A-Z]")) %>%
  separate(all, sep = " [A-Z][A-Z] ", into = c("city", "stats")) %>%
  separate(stats, sep = " ", into = c("mild", "severe")) %>%
  select(city, state, mild, severe)

tre <- 
  tib %>%
  set_names(c("all")) %>%
  mutate(state = str_extract(all, pattern = "[A-Z][A-Z]")) %>%
  separate(all, sep = " [A-Z][A-Z] ", into = c("city", "stats")) %>%
  separate(stats, sep = " ", into = c("mild", "severe")) %>%
  select(city, state, mild, severe)

tro <- 
  tib %>%
  set_names(c("all")) %>%
  mutate(state = str_extract(all, pattern = "[A-Z][A-Z]")) %>%
  separate(all, sep = " [A-Z][A-Z] ", into = c("city", "stats")) %>%
  separate(stats, sep = " ", into = c("mild", "severe")) %>%
  select(city, state, mild, severe)

##

target <- 
  bind_rows(uno, dos, tre, tro) %>%
  mutate(mild = str_remove(mild, "%"),
         severe = str_remove(mild, "%")) %>%
  mutate(mild = as.numeric(mild),
         severe = as.numeric(severe)) %>%
  write_csv("shortfalls.csv")

##

library(tidycensus)
library(tigris)

options(tigris_use_cache = TRUE)

##

cities <- core_based_statistical_areas(cb = TRUE, class = 'sf')
people <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", variables = "B00001_001", year = 2018)

joined <- 
  people %>%
  select(-NAME) %>%
  left_join(cities) %>%
  st_as_sf() %>%
  rename(population = estimate)

##

library(ggmap)
library(sf)

##

target <-
  target %>%
  mutate(address = paste(city, state, sep = ", ")) %>%
  mutate_geocode(location = address, source = "google", output = "latlon")

target %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(4269) %>%
  st_join(joined) %>%
  select(city, state, population, mild, severe) %>%
  st_write("test.geojson")

##

pal <- read_csv("https://raw.githubusercontent.com/asrenninger/palettes/master/grered.txt", col_names = FALSE) %>% pull(X1)

##

proj_cities <- read_sf("data/projected-geography-cities.geojson")
proj_states <- read_sf("data/projected-geography.geojson")

##

library(tmap)  
library(tmaptools)

##

map <- tm_shape(proj_states) +
  tm_polygons(col = "#cfcfcf", border.col = '#ffffff') +
  tm_shape(proj_cities %>%
             rename(`budget shortfall (%)` = severe)) +
  tm_bubbles(col = "budget shortfall (%)", size = "population", pal = rev(pal), scale = 4, border.col = '#cfcfcf') +
  tm_layout(main.title = "FISCAL DISTRESS", 
            main.title.fontface = 'bold',
            legend.position = c("right","bottom"),
            legend.bg.color = "white",
            legend.width = 0.25,
            legend.height = 0.25,
            frame.lwd = 0)

tmap_save(map, "viz/distress.png", height = 12, width = 16, dpi = 300, units = "in")
 
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
  tm_layout(main.title = "LOCAL FISCAL DISTRESS", 
            main.title.fontface = 'bold',
            legend.position = c("right","bottom"),
            legend.bg.color = "white",
            legend.width = 0.25,
            legend.height = 0.25,
            frame.lwd = 0)

tmap_save(map, "~/Desktop/local.png", height = 12, width = 16, dpi = 300, units = "in")

## 

library(janitor)

##

budgets <- read_csv("~/Desktop/states.csv") %>%
  clean_names() %>%
  select(-x1) %>%
  mutate(fy_2020 = readr::parse_number(fy_2020),
         fy_2021 = readr::parse_number(fy_2021)) 

##

states <- 
  proj_states %>%
  transmute(state = name) %>%
  left_join(budgets) %>%
  select(state, fy_2020, fy_2021, geometry) %>%
  st_as_sf()

##

map <- tm_shape(states %>%
                  rename(`budget drop (%)` = fy_2021)) +
  tm_fill(col = "budget drop (%)", border.col = '#ffffff', pal = rev(pal)) +
  tm_shape(proj_cities %>%
             rename(`budget shortfall (%)` = severe)) +
  tm_bubbles(col = "budget shortfall (%)", size = "population", pal = rev(pal), scale = 4, border.col = '#cfcfcf') +
  tm_layout(main.title = "FISCAL DISTRESS", 
            legend.outside = TRUE,
            main.title.fontface = 'bold',
            legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.width = 0.25,
            legend.height = 0.25,
            frame.lwd = 0)

tmap_save(map, "~/Desktop/aggregate.png", height = 12, width = 16, dpi = 300, units = "in")

map <- tm_shape(states %>%
                  rename(`budget drop (%)` = fy_2021)) +
  tm_fill(col = "budget drop (%)", border.col = '#ffffff', pal = rev(pal)) +
  tm_layout(main.title = "STATE FISCAL DISTRESS", 
            #legend.outside = TRUE,
            main.title.fontface = 'bold',
            legend.position = c("right","bottom"),
            legend.bg.color = "white",
            legend.width = 0.25,
            legend.height = 0.25,
            frame.lwd = 0)

tmap_save(map, "~/Desktop/state.png", height = 12, width = 16, dpi = 300, units = "in")

##

map <- tm_shape(states %>%
                  rename(`budget drop (%)` = fy_2021)) +
  tm_fill(col = "budget drop (%)", border.col = '#ffffff', pal = rev(pal)) +
  tm_shape(proj_cities %>%
             rename(`budget shortfall (%)` = severe)) +
  tm_bubbles(col = "budget shortfall (%)", size = "population", pal = rev(pal), scale = 4, border.col = '#cfcfcf') +
  tm_layout(main.title = "STATE AND LOCAL FISCAL DISTRESS", 
            legend.show = FALSE, 
            main.title.fontface = 'bold',
            main.title.position = c("center", "top"),
            frame.lwd = 0)

tmap_save(map, "~/Desktop/artistic.png", height = 12, width = 16, dpi = 300, units = "in")

##

datapasta::tribble_paste()

budgets <- tibble::tribble(
  ~x, ~state, ~debt, ~joblessness, ~rainday, ~shortfall, ~rating, ~yield, ~comment, 
  1,           "Idaho", 2.8, 5.6, 11.5, -20.3, "Aa1*.(S)./.AA+.(S)", 11,                                "Strong financial position, revenue growth of 8% in fiscal year 2020",
  2L,        "Wyoming",       3,     7.6,     96.6,     -28.5,              "AA (S)",    0L,                                                   "Largest rainy day fund as % of government revenue",
  3L,   "South Dakota",     1.5,     7.2,     10.4,     -10.2,  "Aaa* (S) / AAA (S)",   13L,                       "Good reserves and strong budget management including fiscal-year 2020 surplus",
  4L,           "Utah",     2.3,     5.1,      9.2,     -10.3,   "Aaa (S) / AAA (S)",    5L,                                                         "Strong historical job and population growth",
  5L,       "Nebraska",     0.7,     6.7,      7.6,     -12.6,   "Aa1 (S) / AAA (S)",   49L,                                "Lowest overall debt as % of GDP paired with solid financial position",
  6L,   "North Dakota",     2.3,     6.1,      5.4,     -35.9,  "Aa1* (S) / AA+ (S)",   28L,                                 "Strong reserves, energy dependence could lead to prolonged recovery",
  7L,      "Tennessee",     2.1,     9.7,      6.1,     -10.8,   "Aaa (S) / AAA (S)",   -2L,                                                               "Good operations, well-funded pensions",
  8L,           "Iowa",       2,       8,     10.1,     -11.4,  "Aaa* (S) / AAA (S)",   22L,                                                                                "Very low debt burden",
  9L,       "Virginia",     5.1,     8.4,      3.7,       -13,   "Aaa (S) / AAA (S)",   -4L,                                 "Resilient economy bolstered by second-largest federal employee base",
  10L,      "Minnesota",     4.7,     8.6,     10.5,      -9.6,   "Aa1 (S) / AAA (N)",    7L,                                               "Heading into recession with healthy financial metrics",
  11L,     "Washington",     5.7,     9.8,      7.1,     -13.6,   "Aaa (S) / AA+ (S)",   11L,            "Above-average debt, but well-funded pensions, strong demographics, and diverse employers",
  12L,        "Georgia",     6.9,     7.6,       11,       -10,   "Aaa (S) / AAA (S)",    3L,                                                      "Proactive budget management and solid reserves",
  13L, "North Carolina",     7.3,     7.6,      5.3,     -10.1,   "Aaa (S) / AAA (S)",   -8L,                                              "Growing economy prerecession with well-funded pensions",
  14L,        "Indiana",     4.1,    11.2,      8.8,       -16,  "Aaa* (S) / AAA (S)",   22L,                                                                                          "No GO debt",
  15L,          "Texas",     9.5,     8.6,     21.1,     -14.7,   "Aaa (S) / AAA (S)",   23L,                                         "No. 1 in population and job growth in 2019; stable finances",
  16L,        "Florida",     3.8,    10.4,      4.5,     -18.8,   "Aaa (S) / AAA (S)",   -2L,                                                             "Heavy reliance on sales-tax performance",
  17L,         "Oregon",     6.2,    11.2,     12.7,      -8.1,   "Aa1 (S) / AA+ (S)",    3L,                                     "Improved reserves will help offset revenue losses; debt is high",
  18L,       "Delaware",    18.9,    12.5,      5.5,     -10.6,   "Aaa (S) / AAA (S)",   -3L,                                           "Good reserves though highly levered by tax-supported debt",
  19L,       "Arkansas",     6.7,       8,      2.7,     -11.3,    "Aa1 (S) / AA (S)",    5L,                                                                                "Low debt, low wealth",
  20L,  "New Hampshire",     4.9,    11.8,      7.7,      -9.5,    "Aa1 (S) / AA (S)",    2L,                                   "Good economy, large federal stimulus compared with size of budget",
  21L,      "Wisconsin",     4.4,     8.5,      3.6,      -9.9,    "Aa1 (S) / AA (S)",   -2L,                       "Among strongest pensions in country, reserves at highest point in two decades",
  22L,       "Oklahoma",     3.2,     6.6,     11.5,     -22.2,   "Aa2* (S) / AA (N)",   52L,                                                     "Positive financial trends reversed by oil crash",
  23L,        "Montana",     7.2,     7.1,      2.5,     -15.4,    "Aa1 (S) / AA (S)",    4L,                                                                          "Good reserves and low debt",
  24L, "South Carolina",    10.5,     8.7,      6.5,       -16,   "Aaa (S) / AA+ (S)",    7L,                                                        "Strong reserves through poor pension funding",
  25L,       "Missouri",     4.6,     7.9,      6.8,     -22.5,   "Aaa (S) / AAA (S)",    3L,                                           "FY21 budget includes $4 billion in unapproved federal aid",
  26L,       "Colorado",     3.5,    10.5,      8.9,     -11.5,   "Aa1* (S) / AA (S)",   21L,        "Conservative budgeting, recent population growth, good GDP growth heading into the recession",
  27L,       "Maryland",    15.2,       8,      4.9,      -7.6,   "Aaa (S) / AAA (S)",    5L,                                              "Low reserves and strong economy heading into recession",
  28L,  "Massachusetts",      19,    17.4,     10.4,      -7.7,    "Aa1 (S) / AA (S)",    6L, "High debt and highest current jobless rate, but improved reserves and a historically strong economy",
  29L,           "Ohio",     5.9,    10.9,        8,      -8.7,   "Aa1 (S) / AA+ (S)",    9L,                             "Strong budgetary practices, above-average unemployment even prepandemic",
  30L,     "California",    10.7,    14.9,       15,     -14.9,   "Aa2 (S) / AA- (S)",    1L,                           "Much stronger financial position heading into this recession than in 2009",
  31L,       "New York",     7.9,    15.7,      2.8,     -25.5,   "Aa1 (N) / AA+ (S)",   -1L,                        "Estimated budget gap is large and continues to escalate, manageable pensions",
  32L,       "Michigan",     9.7,    14.8,       11,     -18.6,    "Aa1 (S) / AA (N)",   14L,                          "Volatile economy during downturns, supported by recent buildup in reserves",
  33L,        "Alabama",      11,     7.5,      9.8,      -8.3,    "Aa1 (S) / AA (S)",   28L,                                                         "Good fund balances, large pension liability",
  34L,         "Alaska",      17,    12.4,     46.8,     -65.9,   "Aa3 (N) / AA- (N)",   13L,                                                                 "Economy driven by natural resources",
  35L,          "Maine",    11.6,     6.6,      8.3,     -19.7,    "Aa2 (S) / AA (S)",   10L,                                              "Adequate finances, lagging behind prerecession economy",
  36L,        "Vermont",    19.4,     9.4,       14,     -14.1,   "Aa1 (S) / AA+ (S)",   -6L,                         "High unfunded pension and health-care liabilities, strong budget management",
  37L,     "New Mexico",    11.1,     8.3,     14.5,     -11.2,    "Aa2 (S) / AA (N)",    2L,                         "Economic concentration in oil-and-gas industries, strong financial reserves",
  38L,        "Arizona",     2.9,      10,      4.4,     -13.5,   "Aa1* (S) / AA (S)",   -3L,                                                            "Struggling operations with weak reserves",
  39L,         "Nevada",     4.2,      15,      6.9,     -13.4,   "Aa1 (N) / AA+ (N)",    7L,                                        "Employment is highly concentrated in leisure and hospitality",
  40L,         "Hawaii",    26.8,    13.9,      4.8,     -12.2,   "Aa2 (S) / AA+ (N)",   24L,                      "Tourism-based economy as leisure-and-hospitality account for 19% of employment",
  41L,         "Kansas",    10.1,     7.5,        0,     -17.3,  "Aa2* (S) / AA- (S)",   41L,                               "Positive financial trends following years of declines due to tax cuts",
  42L,    "Mississippi",     9.9,     8.7,      6.3,     -14.5,    "Aa2 (S) / AA (S)",   12L,                                                                  "Low wealth, poorly funded pensions",
  43L,  "West Virginia",    14.1,    10.4,     16.4,     -28.2,   "Aa2 (S) / AA- (S)",    2L,                                       "Weak economy and high unemployment pressured by elevated debt",
  44L,      "Louisiana",    10.6,     9.7,      4.1,     -36.4,   "Aa3 (S) / AA- (S)",   20L,                                  "Weak economy; however, strong budget performance prior to pandemic",
  45L,    "Connecticut",      33,     9.8,       13,       -12,      "A1 (S) / A (S)",   47L,                  "The state with the highest overall debt burden on a per capita and a per GDP basis",
  46L,   "Rhode Island",    11.7,    12.4,      5.2,     -12.5,    "Aa2 (S) / AA (S)",   11L,                                          "Very low reserves accompanied by large pension obligations",
  47L,   "Pennsylvania",    12.4,      13,      0.1,      -5.6,    "Aa3 (S) / A+ (S)",   36L,                                          "Weak reserves and sensitive economy with high unemployment",
  48L,       "Kentucky",    22.4,     4.3,      1.1,     -15.6,    "Aa3* (S) / A (S)",   72L,                                                                    "Large and poorly funded pensions",
  49L,     "New Jersey",      31,    16.6,      1.1,     -25.4,     "A3 (N) / A- (N)",   74L,                                                "Could borrow over $4 billion to plug budget deficits",
  50L,       "Illinois",    30.3,    14.6,        0,     -13.1, "Baa3 (N) / BBB- (N)",  223L,                              "The only state that has borrowed from the Municipal Liquidity Facility"
)

##

states <- 
  proj_states %>%
  transmute(state = name) %>%
  left_join(budgets) %>%
  select(state, shortfall, geometry) %>%
  st_as_sf()

## 

map <- tm_shape(states %>%
                  rename(`budget shortfall (%)` = shortfall)) +
  tm_fill(col = "budget shortfall (%)", border.col = '#ffffff', pal = pal, style = 'jenks') +
  tm_shape(proj_cities %>%
             rename(`budget shortfall (%)` = severe)) +
  tm_bubbles(col = "budget shortfall (%)", size = "population", pal = rev(pal), scale = 4, border.col = '#cfcfcf') +
  tm_layout(main.title = "FISCAL DISTRESS", 
            legend.outside = TRUE,
            main.title.fontface = 'bold',
            legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.width = 0.25,
            legend.height = 0.25,
            frame.lwd = 0)

tmap_save(map, "~/Desktop/aggregate.png", height = 12, width = 16, dpi = 300, units = "in")

map <- tm_shape(states %>%
                  rename(`budget shortfall (%)` = shortfall)) +
  tm_fill(col = "budget shortfall (%)", border.col = '#ffffff', pal = pal, style = 'jenks') +
  tm_layout(main.title = "STATE FISCAL DISTRESS", 
            #legend.outside = TRUE,
            main.title.fontface = 'bold',
            legend.position = c("right","bottom"),
            legend.bg.color = "white",
            legend.width = 0.25,
            legend.height = 0.25,
            frame.lwd = 0)

tmap_save(map, "~/Desktop/state.png", height = 12, width = 16, dpi = 300, units = "in")


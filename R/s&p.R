library(tidyverse)
library(janitor)
library(lubridate)

##

datapasta::tribble_paste()

##

returns <- tibble::tribble(
  ~Year, ~Total.Return,
   2019,         31.49,
   2018,         -4.38,
   2017,         21.83,
   2016,         11.96,
   2015,          1.38,
   2014,         13.69,
   2013,         32.39,
   2012,            16,
   2011,          2.11,
   2010,         15.06,
   2009,         26.46,
   2008,           -37,
   2007,          5.49,
   2006,         15.79,
   2005,          4.91,
   2004,         10.88,
   2003,         28.68,
   2002,         -22.1,
   2001,        -11.89,
   2000,          -9.1,
   1999,         21.04,
   1998,         28.58,
   1997,         33.36,
   1996,         22.96,
   1995,         37.58,
   1994,          1.32,
   1993,         10.08,
   1992,          7.62,
   1991,         30.47,
   1990,          -3.1,
   1989,         31.69,
   1988,         16.61,
   1987,          5.25,
   1986,         18.67,
   1985,         31.73,
   1984,          6.27,
   1983,         22.56,
   1982,         21.55,
   1981,         -4.91,
   1980,         32.42,
   1979,         18.44,
   1978,          6.56,
   1977,         -7.18,
   1976,         23.84,
   1975,          37.2,
   1974,        -26.47,
   1973,        -14.66,
   1972,         18.98,
   1971,         14.31,
   1970,          4.01,
   1969,          -8.5,
   1968,         11.06,
   1967,         23.98,
   1966,        -10.06,
   1965,         12.45,
   1964,         16.48,
   1963,          22.8,
   1962,         -8.73,
   1961,         26.89,
   1960,          0.47,
   1959,         11.96,
   1958,         43.36,
   1957,        -10.78,
   1956,          6.56,
   1955,         31.56,
   1954,         52.62,
   1953,         -0.99,
   1952,         18.37,
   1951,         24.02,
   1950,         31.71,
   1949,         18.79,
   1948,           5.5,
   1947,          5.71,
   1946,         -8.07,
   1945,         36.44,
   1944,         19.75,
   1943,          25.9,
   1942,         20.34,
   1941,        -11.59,
   1940,         -9.78,
   1939,         -0.41,
   1938,         31.12,
   1937,        -35.03,
   1936,         33.92,
   1935,         47.67,
   1934,         -1.44,
   1933,         53.99,
   1932,         -8.19,
   1931,        -43.34,
   1930,         -24.9,
   1929,         -8.42,
   1928,         43.61,
   1927,         37.49,
   1926,         11.62
  ) %>%
  clean_names() %>%
  mutate(valence = if_else(total_return > 0, "pos", "neg"))

##

test <- returns

##

library(RColorBrewer)

##

pal <- brewer.pal(n = 10, name = 'RdYlBu')

##

return <- returns %>% 
  arrange(desc(year)) %>% 
  slice(1) %>% 
  pull(total_return)

average <- mean(returns$total_return)

##

xd <- tibble(density(test$total_return)[c("x", "y")])

##

p1 <- ggplot(xd, aes(x, y)) + 
  geom_area(data = subset(xd, x < 0), fill = pal[1]) +
  geom_area(data = subset(xd, x > 0), fill = pal[10]) +
  geom_vline(aes(xintercept = mean(returns$total_return)),
             linetype = 2, colour = '#f1f1f1') +
  geom_vline(aes(xintercept = returns %>% 
                   arrange(desc(year)) %>% 
                   slice(1) %>% 
                   pull(total_return)),
             linetype = 2, colour = '#f1f1f1') +
  annotate("text", 
           x = average - 10, 
           y = 0.018, size = 5, color = "gray20",
           label = "AVERAGE",
           fontface = 'bold') +
  annotate("text", 
           x = return + 10, 
           y = 0.018, size = 5, color = "gray20",
           label = "THIS YEAR",
           fontface = 'bold') +
  geom_line() +
  ylab("frequency") +
  xlab("return") +
  theme_minimal()

p1 

p2 <-
  ggplot(returns, aes(year, total_return)) +
  geom_bar(aes(fill = valence), colour = NA,
           stat = 'identity') +
  scale_fill_manual(values = c(pal[1], pal[10]),
                    guide = 'none') +
  ylab("return") +
  xlab("year") +
  theme_minimal()
  
p2

##

library(patchwork)

##

p3 <- p1 / p2

p4 <- p3 + plot_annotation(title = 'S&P 500 since inception')

ggsave(plot = p4, "test.png", width = 11, height = 8, dpi = 300)

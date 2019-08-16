test <- data.table::fread("~/Downloads/mappm102001.csv", skip = 5)

glimpse(test)

library(viridis)

ggplot(test %>%
         filter(pm102001 != "MISSING") %>%
         mutate(pm102001 = as.numeric(pm102001)), (aes(x, y))) +
  geom_point(aes(colour = pm102001), size = 0.01) +
  scale_colour_viridis()

read_csv("~/Downloads/mapno22017.csv", skip = 5) %>%
  set_names(c("code", "x", "y", "variable")) %>%
  filter(variable != "MISSING") %>%
  mutate(variable = as.numeric(variable)) %>%
  ggplot(aes(x, y)) +
  geom_point(aes(colour = variable), size = 0.01) +
  scale_colour_viridis()
  
"https://uk-air.defra.gov.uk/datastore/pcm/mapno22016.csv"
"https://uk-air.defra.gov.uk/datastore/pcm/mapno22017.csv"

## 

library(glue)

##

glue("~/Downloads/mapno22016.csv") %>%
  read_csv(skip = 5) 

test <- 
  reduce(
    map(2007:2017, function(x) {
      glue("~/Downloads/mapno2{x}.csv") %>%
        read_csv(skip = 5) %>%
        set_names(c("code", "X", "Y", "NO2")) %>%
        filter(NO2 != "MISSING") %>%
        mutate(year = x)
    }), 
    bind_rows
  )

test %>% pull(year) %>% unique()

p <-
  ggplot(test, 
         aes(x = X, y = Y)) + 
  geom_point(aes(color = NO2), size = 0.1) +
  scale_fill_viridis() +
  theme_map()


##

library(gganimate)

##

my_anim <- 
  p + 
  transition_states(year,
                    transition_length = 1,
                    state_length = 1)+
  enter_fade() + 
  exit_shrink() +
  labs(subtitle= "{closest_state}")

animate(my_anim, height = 650, width = 350)

anim_save("test.gif", animation = my_anim, 
          height = 650, width = 350,
          path = "~/Desktop/")



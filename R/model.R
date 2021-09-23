###############################
## modelling
###############################

## packages
library(tidyverse)
library(tidymodels)
library(tigris)
library(sf)

## block groups
blocks <- 
  block_groups(state = "MD", cb = TRUE, class = 'sf') %>% 
  st_transform(26985) %>% 
  select(GEOID) %>%
  mutate(area = st_area(geometry),
         area = units::set_units(area, ha))

## datasets
area_character <- 
  read_csv("data/area_characteristics.csv") %>% 
  mutate(GEOID = as.character(GEOID))

area_protected <- 
  read_csv("data/protected_lands.csv") %>% 
  mutate(GEOID = as.character(GEOID))

area_landcover <- 
  read_csv("data/remote_sensing.csv") %>% 
  mutate(GEOID = as.character(GEOID))

## features
vars <- c("GEOID", "geometry_count_perha", "area_mean", "isoperi_mean", "straightness", "orientation_order", "ndvi_mean")

# from remote sensing
raster_variables <- 
  area_character %>% 
  left_join(blocks) %>% 
  mutate(geometry_count_perha = geometry_count / units::drop_units(area)) %>%
  left_join(area_protected) %>%
  left_join(area_landcover) %>% 
  select(all_of(vars))
  
vars <- c("GEOID", "density_gap", "sign", "hhi_med", "pct_bach", "pct_white", "popden", "pct_unit_renter", "pct_unit_vacant", "dist_popcen", "dist_station_cat", "pct_unit_recent10", "res_acres", "hu_acres_2019", "avgage")

# from data collection
vector_variables <- 
  model_bg %>% 
  as_tibble() %>% 
  janitor::clean_names() %>%
  rename(GEOID = geoid) %>%
  mutate(density_gap = maxhu_acre_clt - hu_acres_2019,
         sign = if_else(density_gap < 0, -1, 1)) %>%
  select(all_of(vars)) 

## testing the model
mod_linear <- 
  lm(density_gap ~ .,
   data = 
     vector_variables %>% 
     left_join(raster_variables) %>%
     # remove what is not significant
     select(-GEOID, -sign, -straightness, -pct_bach, -pct_unit_renter, -pct_unit_vacant) %>%
     mutate(density_gap = log(abs(density_gap))))

summary(mod_linear)

## preparing the data
set.seed(42)

# hold out one county for testing
holdout <- 
  vector_variables %>% 
  pull(GEOID) %>% 
  str_sub(1, 5) %>%
  unique() %>%
  sample(1)

# check out which county it is
blocks %>% 
  transmute(holdout = factor(if_else(str_sub(GEOID, 1, 5) == holdout, 1, 0))) %>% 
  plot()
  
crosswalk <-
  vector_variables %>% 
  left_join(raster_variables) %>%
  filter(str_sub(GEOID, 1, 5) != holdout) %>%
  drop_na() %>% 
  transmute(GEOID, 
            density_gap = log(abs(density_gap))) %>%
  left_join(blocks) %>% 
  st_as_sf()

# split the data into training/testing
set.seed(42)

split <- 
  vector_variables %>% 
  left_join(raster_variables) %>%
  filter(str_sub(GEOID, 1, 5) != holdout) %>%
  drop_na() %>%
  select(-GEOID, -sign, -straightness, -pct_bach, -pct_unit_renter, -pct_unit_vacant) %>%
  # log transform the dependent variable
  mutate(density_gap = log(abs(density_gap))) %>%
  # go with a 50/50 split so it is not too easy
  initial_split(0.5)

# create separate datasets
train <- training(split)
test <- testing(split)

# look at the dependent variable for each
bind_rows(mutate(train, split = "train"),
          mutate(test, split = "test")) %>%
  ggplot(aes(exp(density_gap), fill = split)) +
  geom_histogram() +
  scale_x_log10() +
  facet_wrap(~ split) +
  theme_minimal()

# rescale continous variables
continuous <- c("geometry_count_perha", "area_mean", "isoperi_mean", "orientation_order", "ndvi_mean", "density_gap", "hhi_med", "pct_white", "popden", "dist_popcen", "dist_station_cat", "pct_unit_recent10", "res_acres", "hu_acres_2019", "avgage")

prediction_recipe <- 
  recipe(density_gap ~ ., 
         data = train) %>%
  step_scale(all_of(continuous))

prediction_prep <- prep(prediction_recipe)
prediction_juice <- juice(prediction_prep)

## model it
set.seed(42)

# a boosted tree
mod_boost <-
  boost_tree(trees = 1000, 
             tree_depth = tune(),
             min_n = tune(),
             loss_reduction = tune(), 
             sample_size = tune(),
             mtry = tune(),
             learn_rate = tune()) %>%
  set_mode("regression") %>% 
  set_engine("xgboost")

wflow_boost <- 
  workflow() %>% 
  add_recipe(prediction_recipe) %>% 
  add_model(mod_boost)

## model it
set.seed(42)

# random forest
mod_rf <-
  rand_forest(trees = 1000,
              mtry = tune(),
              min_n = tune()) %>%
  set_mode("regression") %>% 
  set_engine("ranger")

wflow_rf <- 
  workflow() %>% 
  add_recipe(prediction_recipe) %>%
  add_model(mod_rf) 

# this will take a while without parallel processing
doParallel::registerDoParallel(cores = 6)

# create a regular grid to search through
hypercube <- 
  grid_latin_hypercube(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), train),
    learn_rate(),
    size = 10
  )

grid <- 
  grid_regular(
    mtry(range = c(2, 10)),
    min_n(range = c(2, 10)),
    levels = 5
  )

## tune on 10 folds
set.seed(42)

# default is 10 folds
folds <- vfold_cv(train)

# tune the first (six cores, 66.948 seconds)
tictoc::tic()
tuned_boost <- 
  tune_grid(
    wflow_boost,
    resamples = folds,
    grid = hypercube
  )
tictoc::toc()

# tune the next (six cores, 269.703 seconds)
tictoc::tic()
tuned_rf <- 
  tune_grid(
    wflow_rf,
    resamples = folds,
    grid = grid
  )
tictoc::toc()

## much lower RMSE for the random forest
bind_rows(collect_metrics(tuned_boost) %>%
            mutate(model = "boosted tree"),
          collect_metrics(tuned_rf) %>% 
            mutate(model = "random forest")) %>% 
  filter(.metric == "rmse") %>% 
  group_by(model) %>%
  summarise(rmse = mean(mean))

# grab the best models
best_boost <- select_best(tuned_boost, "rmse")
best_rf <- select_best(tuned_rf, "rmse")

final_boost <- 
  finalize_model(
    mod_boost,
    best_boost
  )

final_rf <- 
  finalize_model(
    mod_rf,
    best_rf
  )

# plot the most important variables
final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(density_gap ~ .,
      data = prediction_juice) %>%
  vip::vip(geom = "point") +
  theme_minimal() +
  ggsave("vip_rf.png", height = 6, width = 6, dpi = 300)

# fit boosted tree
fflow_boost <- 
  workflow() %>%
  add_recipe(prediction_recipe) %>%
  add_model(final_boost)

results_boost <- 
  fflow_boost %>%
  last_fit(split)

# fit rf
fflow_rf <- 
  workflow() %>%
  add_recipe(prediction_recipe) %>%
  add_model(final_rf)

results_rf <- 
  fflow_rf %>%
  last_fit(split)

# collect final metrics
results_boost %>%
  collect_predictions() %>%
  filter(density_gap != 0) %>%
  mutate(error = exp(abs(density_gap - .pred))) %>% 
  ggplot(aes(exp(density_gap), exp(.pred), colour = error)) + 
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  scale_y_log10(breaks = c(0.3, 1.0, 3.0, 10),
                limits = c(NA, 20)) +
  scale_colour_gradientn(colours = RColorBrewer::brewer.pal(n = 9, name = 'OrRd'), 
                         breaks = c(5, 10, 15), limits = c(0, 20), oob = scales::squish) +
  labs(title = "XGBoost (in sample)",
       x = "density gap",
       y = "prediction") + 
  theme_minimal() +
  ggsave("boost.png", height = 6, width = 8, dpi = 300)

?scale_y_log10

results_rf %>%
  collect_predictions() %>%
  filter(density_gap != 0) %>%
  mutate(error = exp(abs(density_gap - .pred))) %>% 
  ggplot(aes(exp(density_gap), exp(.pred), colour = error)) + 
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  scale_y_log10(breaks = c(0.3, 1.0, 3.0, 10),
                limits = c(NA, 20)) +
  scale_colour_gradientn(colours = RColorBrewer::brewer.pal(n = 9, name = 'OrRd'), 
                       breaks = c(5, 10, 15), limits = c(0, 20), oob = scales::squish) +
  labs(title = "Random Forest (in sample)",
       x = "density gap",
       y = "prediction") + 
  theme_minimal() +
  ggsave("rf.png", height = 6, width = 8, dpi = 300)

## the MAE is the same (1.77 and 1.78)  for both models
results_boost %>%
  collect_predictions() %>%
  mutate(error = exp(abs(density_gap - .pred))) %>%
  summarise(MAE = mean(error))

results_rf %>%
  collect_predictions() %>%
  mutate(error = exp(abs(density_gap - .pred))) %>%
  summarise(MAE = mean(error))

## holdout
updated_data <- 
  vector_variables %>% 
  left_join(raster_variables) %>%
  drop_na() %>%
  select(-GEOID, -sign, -straightness, -pct_bach, -pct_unit_renter, -pct_unit_vacant) %>%
  # log transform the dependent variable
  mutate(density_gap = log(abs(density_gap)))

holdout_recipe <- 
  recipe(density_gap ~ ., 
         data = updated_data) %>%
  step_scale(all_of(continuous))

holdout_prep <- prep(holdout_recipe)
holdout_juice <- juice(holdout_prep)

holdout_fit <- 
  final_rf %>%
  set_engine("ranger") %>%
  fit(density_gap ~ .,
      # this is the original data
      data = prediction_juice
  )

## MAE of 1.81 on 28 block groups
vector_variables %>% 
  left_join(raster_variables) %>%
  drop_na() %>%
  transmute(GEOID, 
            density_gap = log(abs(density_gap)),
            # this is the county we held out
            prediction = predict(holdout_fit, holdout_juice)$.pred) %>%
  filter(str_sub(GEOID, 1, 5) == holdout) %>%
  summarise(MAE = mean(abs(density_gap - prediction)),
            n = n()) %>% 
  mutate(MAE = exp(MAE))

test_juice <- 
  recipe(density_gap ~ ., 
       data = test) %>%
  step_scale(all_of(continuous)) %>% 
  prep() %>% 
  juice()

# out of sample is way worse with a MAE of 5.83 across 1865 block groups
predict(holdout_fit, test_juice) %>%
  mutate(density_gap = test$density_gap,
         error = exp(abs(density_gap - .pred))) %>% 
  summarise(MAE = mean(error),
            n = n())

options(scipen = 999)

predict(holdout_fit, test_juice) %>%
  mutate(density_gap = test$density_gap,
         error = abs(density_gap - .pred)) %>%
  mutate(error = exp(abs(density_gap - .pred))) %>% 
  ggplot(aes(exp(density_gap), exp(.pred), colour = error)) + 
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  scale_y_log10() +
  scico::scale_colour_scico(palette = 'hawaii', breaks = c(5, 10, 15), limits = c(0, 20), oob = scales::squish) +
  ggtitle("RF (out of sample)") + 
  theme_minimal() +
  ggsave("rf_out.png", height = 6, width = 8, dpi = 300)

## comparison with linear model
mod_linear <-
  linear_reg() %>%
  set_mode("regression") %>% 
  set_engine("lm")

fit_linear <- 
  mod_linear %>%
  fit(density_gap ~ .,
      data = prediction_juice
  )

# MAE is 6.19 for a linear model
fit_linear %>%
  predict(new_data = test_juice) %>% 
  mutate(density_gap = test$density_gap) %>%
  filter(density_gap != 0) %>%
  mutate(error = exp(abs(density_gap - .pred))) %>% 
  summarise(MAE = mean(error))

# compare 
bind_rows(
  fit_linear %>%
    predict(new_data = test_juice) %>% 
    mutate(density_gap = test$density_gap) %>%
    transmute(error = exp(abs(density_gap - .pred)),
              .pred,
              density_gap,
              model = "linear"),
  predict(holdout_fit, test_juice) %>%
    mutate(density_gap = test$density_gap,
           error = abs(density_gap - .pred)) %>%
    mutate(error = exp(abs(density_gap - .pred)),
           .pred,
           density_gap,
           model = "random forest")) %>%
  ggplot(aes(exp(density_gap), exp(.pred), colour = model)) + 
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  scale_y_log10() + 
  scale_color_brewer(palette = 'Set1') +
  labs(title = "Comparison between Linear Model and Random Forest (out of sample)",
       x = "density gap",
       y = "prediction") + 
  theme_minimal() +
  ggsave("comparison.png", height = 6, width = 8, dpi = 300)

###############################
## plotting 
###############################

tmap_mode("plot")

tmap_save(
  blocks %>% 
    left_join(area_landcover) %>% 
    transmute(`development change` = rescale(development_change, to = c(0, 10))) %>%
    tm_shape() + 
    tm_fill(col = "development change", style = "fisher", pal = RColorBrewer::brewer.pal(n = 9, name = 'OrRd')),
  filename = "landcover.png", height = 6, dpi = 300)

tmap_save(
  blocks %>% 
  left_join(area_landcover) %>% 
  transmute(`impervious change` = rescale(impervious_change, to = c(0, 10))) %>%
  tm_shape() + 
  tm_fill(col = "impervious change", style = "fisher", pal = RColorBrewer::brewer.pal(n = 9, name = 'PuBu')),
  filename = "impervious.png", height = 6, dpi = 300)

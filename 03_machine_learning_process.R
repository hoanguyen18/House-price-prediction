source("utils.R")

##################################### #
# Get data ----
##################################### #

houses_raw <- read_excel("temp/houses.xlsx")

##################################### #
# Split data into train/test ----
##################################### #

set.seed(42)

split     <- initial_split(houses_raw, prop = 3/4)
train_raw <- training(split)
test_raw  <- testing(split)

##################################### #
# Cross-validation ----
##################################### #

folds <- vfold_cv(train_raw, 5)

##################################### #
# Create recipe ----
##################################### #

recipe <- train_raw %>%
  recipe(tot_price ~ .) %>% 
  step_select(tot_price, sqm, expense, kommune_name, lat, lng) %>% 
  step_other(kommune_name, threshold = 20) %>% 
  step_dummy(kommune_name) %>% 
  step_log(tot_price, sqm) %>% 
  prep()

train <- bake(recipe, train_raw)
test  <- bake(recipe, test_raw)

##################################### #
# Create model ----
##################################### #

model <- boost_tree(
  trees      = 350,
  tree_depth = tune(),
  min_n      = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression") 

##################################### #
# Tune grid ----
##################################### #

params <- parameters(min_n(), tree_depth())
grid <- grid_max_entropy(params)

tuning <- model %>% 
  tune_grid(
    preprocessor = recipe,
    resamples    = folds,
    grid         = grid,
    metrics      = metric_set(mape, mae),
    control      = control_grid(verbose = TRUE))

show_best(tuning, "mape")
params_best <- select_best(tuning, "mape")

##################################### #
# Train using best model ----
##################################### #

final_model <- model %>% 
  finalize_model(parameters = params_best) %>% 
  fit(tot_price ~ ., data = train)

##################################### #
# Test using best model ----
##################################### #

model_preds <- 
  predict(final_model, test) %>% 
  bind_cols(test_raw) %>% 
  rename(estimate     = .pred,
         truth        = tot_price) %>% 
  mutate(estimate     = exp(estimate),
         abs_dev      = abs(truth - estimate),
         abs_dev_perc = abs_dev/truth)

##################################### #
# Evaluate model ----
##################################### #

mape(model_preds, truth, estimate)

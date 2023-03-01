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
# Create recipe to prep train and test data ----
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
# Create and fit model ----
##################################### #

model <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(tot_price ~ ., data = train)

##################################### #
# Test model ----
##################################### #

model_preds <- 
  predict(model, test) %>% 
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

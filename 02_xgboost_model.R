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
  step_integer(kommune_name) %>% 
  step_log(tot_price, sqm) %>% 
  prep()

train <- bake(recipe, train_raw)
test  <- bake(recipe, test_raw)

##################################### #
# Create and fit model ----
##################################### #

model <- boost_tree(trees = 350) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression") %>% 
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

##################################### #
# Calculate SHAP values----
##################################### #
# SHAP values is a method to explain individual predictions. # The method is based on game theory
# and you can read more about it in chapter 16.8 of this book: https://bradleyboehmke.github.io/HOML/iml.html#shapley-values

# Calculate SHAP values
shap_xgb <- predict.model_fit(model, test, type = "raw", opts = list(predcontrib = TRUE)) %>% 
  as_tibble() %>% 
  pivot_longer(everything(), names_to = "feature", values_to = "shap_value") %>% 
  group_by(feature) %>% 
  mutate(shap_importance = mean(abs(shap_value))) %>% 
  ungroup()

# SHAP contribution plot
shap_xgb %>% 
  filter(feature != "BIAS") %>% 
  ggplot(aes(x = shap_value, y = reorder(feature, shap_importance))) +
  geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size = 0.4, alpha = 0.25) +
  xlab("SHAP value") +
  ylab(NULL) +
  theme_minimal()

# SHAP importance plot
shap_xgb %>% 
  filter(feature != "BIAS") %>% 
  distinct(feature, shap_importance) %>%
  ggplot(aes(x = reorder(feature, shap_importance), y = shap_importance)) +
  geom_col() +
  coord_flip() +
  xlab(NULL) +
  ylab("Mean absolute SHAP value") +
  theme_minimal()

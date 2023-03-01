source("utils.R")

#################### #
# Exercise 1 - Import data ----
#################### #

houses_raw <- read_excel("temp/houses.xlsx")

#################### #
# Exercise 2 - Split data ----
#################### #

set.seed(42)

split     <- initial_split(houses_raw, prop = 3/4)
train_raw <- training(split)
test_raw  <- testing(split)

#################### #
# Exercise 3 - Create recipe and prepare  ----
#################### #

recipe <- train_raw %>% 
  recipe(tot_price ~ .) %>% 
  step_select(tot_price, sqm, expense, kommune_name, lat, lng) %>% 
  step_log(tot_price, sqm) %>% 
  step_other(kommune_name, threshold = 20) %>% 
  step_dummy(kommune_name) %>% 
  prep()

train <- bake(recipe, train_raw)
test  <- bake(recipe, test_raw)

#################### #
# Exercise 4 - Fit a linear model  ----
#################### #

model <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(tot_price ~ ., data = train)

# View summary of fit
summary(model$fit)

#################### #
# Exercise 5 - Use model for predictions  ----
#################### #

model_preds <- 
  predict(model, test) %>% 
  bind_cols(test_raw) %>% 
  mutate(.pred = exp(.pred)) %>% 
  rename(estimate     = .pred, 
         truth        = tot_price) %>% 
  mutate(abs_dev      = abs(truth - estimate),
         abs_dev_perc = abs_dev/truth) %>% 
  relocate(estimate, truth) # See comment below

# Line 64, `relocate(estimate, truth)` is a trick to reorder the columns such that `estimate` and `truth` appears
# as the first two columns in the data set. This was not requested of the students in the exercise.

#################### #
# Exercise 6 - Evaluate model  ----
#################### #

mape(data = model_preds, truth = truth, estimate = estimate)

##################################### #
# Exercise 7 - Plot results ----
##################################### #

model_preds %>%
  ggplot(aes(x = abs_dev_perc)) +
  geom_histogram(fill = "dodgerblue3", color = "white") +
  labs(x = "Prosentvis feil",
       y = "Antall observasjoner") +
  scale_x_continuous(limits = c(0,5), labels = scales::percent) +   
  theme_minimal()

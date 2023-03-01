source("utils.R")

#################### #
# Exercise 1 - Import more data sets ----
#################### #

houses_raw     <- read_excel("input/houses.xlsx")
geo_raw        <- read_excel("input/geo.xlsx")
zip_raw        <- read_excel("input/zip.xlsx")
income_raw     <- read_excel("input/income.xlsx")
attributes_raw <- read_excel("input/attributes.xlsx") # sqm , built, floor

#################### #
# Exercise 2 - Select relevant variables ----
#################### #

geo <- geo_raw %>% 
  select(id, kommune_no, kommune_name, fylke_no, fylke_name)

income <- income_raw %>% 
  select(zip_no      = postnr,
         avg_income  = gjsnitt_inntekt,
         avg_fortune = gjsnitt_formue)

zip        <- zip_raw
attributes <- attributes_raw

#################### #
# Exercise 3 - Merge data ----
#################### #

# Important to join zip before income
houses <- houses_raw %>% 
  left_join(geo,        by = "id") %>%
  left_join(zip,        by = "id") %>% 
  left_join(income,     by = "zip_no") %>%
  left_join(attributes, by = "id")

##################################### #
# Exercise  4 ---- Prep and plot
##################################### #

houses_output <- houses %>% 
  mutate(debt    = ifelse(is.na(debt), 0, debt),
         expense = ifelse(is.na(expense), 0, expense),
         tot_price         = price + debt, 
         tot_price_per_sqm = tot_price/sqm) %>% 
  drop_na()

houses_output %>% 
  ggplot(aes(x = tot_price/1000000)) +
  geom_histogram(fill = "dodgerblue3", color = "white") +
  labs(x = "Price [Million NOK]",
       y = "Frequency") +
  xlim(0, 10) +
  theme_minimal()

houses_output %>% 
  ggplot(aes(x = sqm, y = tot_price/1000000)) +
  geom_point(color = "dodgerblue3", alpha = 0.4) +
  labs(x = "Squared Mater",
       y = "Price [Million NOK]") +
  theme_minimal()

houses_output %>% 
  ggplot(aes(x = log(sqm), y = log(tot_price/1000000))) +
  geom_point(color = "dodgerblue3", alpha = 0.4) +
  labs(x = "log(Squared Mater)",
       y = "log(Price [MNOK])") +
  theme_minimal()


houses_output %>% 
  ggplot(aes(x = built, y = tot_price/1000000)) +
  geom_point(color = "dodgerblue3", alpha = 0.4) +
  labs(x = "built",
       y = "Price [MNOK]") +
  theme_minimal()

houses_output <- houses_output %>% 
  filter(houses_output, floor > 0)

houses_output %>% 
  ggplot(aes(x = floor, y = tot_price/1000000)) +
  geom_point(color = "dodgerblue3", alpha = 0.4) +
  labs(x = "floor",
       y = "Price [MNOK]") +
  theme_minimal()

##################################### #
# Optional Exercise - Create feature ----
##################################### #

# Two suggestions for finding number of houses nearby

# Suggestion 1: Number of houses within zip number
houses_within_zip <- houses_output %>% 
  count(zip_no, name = "n_houses_per_zip")

houses_output_zip <- houses_output %>% 
  left_join(houses_within_zip, by = "zip_no")

# Suggestion 2: Number of houses within 1500 meter
library(sf)

houses_sf <- houses_output %>% 
  st_as_sf(coords = c("lng", "lat"),
           crs    = 4326)
  
houses_within_radius <- houses_sf %>% 
  st_transform(25833) %>% 
  st_buffer(1500) %>% 
  st_transform(4326) %>% 
  st_intersection(houses_sf) %>% 
  st_set_geometry(NULL) %>% 
  count(id, name = "n_houses_nearby")

houses_output_nearby <- houses_output %>% 
  left_join(houses_within_radius, by = "id")

  
#################### #
# Exercise 5 - Write data to excel ----
#################### #

write_xlsx(houses_output, "temp/houses_exercise.xlsx")

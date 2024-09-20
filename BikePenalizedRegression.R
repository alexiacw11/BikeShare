# Load libraries
library(tidymodels)

# Read in data
train <- vroom::vroom("train.csv")
test <- vroom::vroom("test.csv")

# Get rid of casual and registered variables in train set
train <- train |> 
  select(-casual, -registered)

# Change count to log(count)
train <- train |> 
  mutate(count = log(count),
         datetime = ymd_hms(datetime))

train <- train %>%
  mutate(TimeOfDay = ifelse(hour(ymd_hms(datetime)) >= 6 & hour(ymd_hms(datetime))  < 12, "Morning",
                            ifelse(hour(ymd_hms(datetime))  >= 12 & hour(ymd_hms(datetime)) < 18, "Midday", "Evening")))

test <- test %>%
  mutate(TimeOfDay = ifelse(hour(ymd_hms(datetime)) >= 6 & hour(ymd_hms(datetime))  < 12, "Morning",
                            ifelse(hour(ymd_hms(datetime))  >= 12 & hour(ymd_hms(datetime)) < 18, "Midday", "Evening")))


# Create a recipe
my_recipe <- recipe(count ~ . , data=train) %>% 
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>% 
  step_mutate(weather = factor(weather), season = factor(season), 
              workingday = factor(workingday), holiday = factor(holiday)) %>%
  step_date(datetime, features = "month") %>%  # Extracts the month from datetime
  step_time(datetime, features = "hour") %>%  # Extracts the hour from datetime; creates 'datetime_hour'
  step_zv(all_predictors()) %>%  # Removes zero-variance predictors
  step_rm(holiday, temp, weather, datetime) %>% 
  step_dummy(all_nominal_predictors())  %>% # make dummy variables
  step_normalize(all_numeric_predictors()) # make mean 0 and sd = 1
  
# Prep and bake
prepped_recipe <- prep(my_recipe)
bake(prepped_recipe, new_data = test)

## Attempting Different Penalties - 0.85449
preg_model <- linear_reg(penalty = 0, mixture = 0.5) |> 
  set_engine("glmnet") |> 
  set_mode("regression")

# 0.8505
preg_model <- linear_reg(penalty = 0.01, mixture = 1) |> 
  set_engine("glmnet") |> 
  set_mode("regression")

# 1.07 - really bad! 
preg_model <- linear_reg(penalty = 1, mixture = 0.2) |> 
  set_engine("glmnet") |> 
  set_mode("regression")

## Combine into a Workflow and fit
bike_workflow <- workflow() |> 
  add_recipe(my_recipe) |> 
  add_model(preg_model) |> 
  fit(data = train)

## Run all the steps on test data
predictions <- predict(bike_workflow, new_data = test)
bike_predictions <- exp(predictions)

kaggle_submission <- bike_predictions %>%  
  bind_cols(., test) %>% 
  select(datetime, .pred) %>% 
  rename(count=.pred) %>% 
  mutate(count=pmax(0, count)) %>%
  mutate(datetime=as.character(format(datetime)))

# Slightly lower than last, but by .4
vroom_write(x=kaggle_submission, file="./PenalizedLinearPreds.csv", delim=",")

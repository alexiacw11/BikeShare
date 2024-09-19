# Load libraries
library(tidyverse)
library(tidymodels)
library(lubridate)

# Set working directory
setwd("/Users/alexiacw11/Desktop/Fall_24/Stat348/BikeShare")

# Reading in data 
train <- vroom::vroom("train.csv")
test <- vroom::vroom("test.csv")

# Factorizing categorical variables
train <- train |> 
  mutate(across(c(season, holiday, workingday, weather), as.factor))
test <- test |> 
  mutate(across(c(season, holiday, workingday, weather), as.factor))

# Data cleaning - check for duplicates, there is none
anyDuplicated(train)

#-------------------------------------------------------------------------------------

# Linear regression without transforming weather data + without log transform
linear_factor <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")  |> # dealing w quant target
  fit(formula = count ~ windspeed + humidity + atemp + temp + weather + workingday + holiday + season, data = train)

bike_predictions <- predict(linear_factor, new_data = test) # use fit to predict

# Look at output
bike_predictions

# Format predictions for submission to kaggle
kaggle_submission <- bike_predictions |> 
  bind_cols(test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file using vroom
vroom_write(x=kaggle_submission, file="./LinearPredsNoTransform.csv", delim=",")


#-------------------------------------------------------------------------------------

# Linear regression with log transform 
linear_log <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")  |> # dealing w quant target
  fit(formula = log(count) ~ windspeed + humidity + atemp + temp + weather + workingday + holiday + season, data = train)

bike_predictions <- predict(linear_log, new_data = test) # use fit to predict
bike_predictions <- exp(bike_predictions)


kaggle_submission <- bike_predictions %>%  
  bind_cols(., test) %>% 
  select(datetime, .pred) %>% 
  rename(count=.pred) %>% 
  mutate(count=pmax(0, count)) %>%
  mutate(datetime=as.character(format(datetime))) 

# This one performed 2% better than one above
vroom_write(x=kaggle_submission, file="./LinearPredsLog.csv", delim=",")

#-------------------------------------------------------------------------------------
# Linear regression with log + transforming weather data

# Transforming weather so that there is no longer a fourth level. Combined instance to 3rd.
train$weather[train$weather == levels(train$weather)[4]] <- levels(train$weather)[3]
test$weather[test$weather == levels(test$weather)[4]] <- levels(test$weather)[3]


linear_log_wtrans <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")  |> # dealing w quant target
  fit(formula = log(count) ~ windspeed + humidity + atemp + temp + weather + workingday + holiday + season, data = train)

bike_predictions <- predict(linear_log_wtrans, new_data = test) # use fit to predict
bike_predictions <- exp(bike_predictions)

kaggle_submission <- bike_predictions %>%  
  bind_cols(., test) %>% 
  select(datetime, .pred) %>% 
  rename(count=.pred) %>% 
  mutate(count=pmax(0, count)) %>%
  mutate(datetime=as.character(format(datetime))) 

# Slightly lower than last, but by .4
vroom_write(x=kaggle_submission, file="./LinearPredsLogWTrans.csv", delim=",")
#-------------------------------------------------------------------------------------

# Attempting to improve model now with more cleaning + feature engineering

# Reread in og data
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

# MLR to help decide which variables to keep
lm.test <- lm(count ~ datetime + season + workingday + weather + 
                atemp + humidity + windspeed + TimeOfDay, data=train)
summary(lm.test)


View(train)
# Creating a recipe
bike_recipe <- recipe(count ~ ., data = train) %>% 
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>% 
  step_mutate(weather = factor(weather), season = factor(season), 
              workingday = factor(workingday), holiday = factor(holiday)) %>%
  step_date(datetime, features = "month") %>%  # Extracts the month from datetime
  step_time(datetime, features = "hour") %>%  # Extracts the hour from datetime; creates 'datetime_hour'
  step_zv(all_predictors()) %>%  # Removes zero-variance predictors
  step_rm(holiday, temp, weather)

# Sets up the preprocessing using the data
prepped_recipe <- prep(bike_recipe) 
bake(prepped_recipe, new_data=test)

## Define a Model
lin_model <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")

## Combine into a Workflow and fit
bike_workflow <- workflow() |> 
  add_recipe(bike_recipe) |> 
  add_model(lin_model) |> 
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
vroom_write(x=kaggle_submission, file="./LogLinearPreds.csv", delim=",")
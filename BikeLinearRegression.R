# Load libraries
library(tidyverse)
library(tidymodels)

# Reading in data 
train <- vroom::vroom("train.csv")
test <- vroom::vroom("test.csv")

# Factorizing categorical variables
train <- train |> 
  mutate(across(c(season, holiday, workingday, weather), as.factor))
test <- test |> 
  mutate(across(c(season, holiday, workingday, weather), as.factor))

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
  
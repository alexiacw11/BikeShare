
# Load libraries
library(rpart)
library(tidymodels)
library(tidyverse)

# Clean data
train <- vroom::vroom("train.csv")
test <- vroom::vroom("test.csv")

# Get rid of a few variables
train <- train |> 
  select(-c(casual, registered, holiday, temp))

test <- test |> 
  select(-c(holiday, temp))

# Change count to log(count)
train <- train |> 
  mutate(count = log(count),
         datetime = ymd_hms(datetime))

# Set up TimeOfDay variable 
train <- train %>%
  mutate(TimeOfDay = ifelse(hour(ymd_hms(datetime)) >= 6 & hour(ymd_hms(datetime))  < 12, "Morning",
                            ifelse(hour(ymd_hms(datetime))  >= 12 & hour(ymd_hms(datetime)) < 18, "Midday", "Evening")))
test <- test %>%
  mutate(TimeOfDay = ifelse(hour(ymd_hms(datetime)) >= 6 & hour(ymd_hms(datetime))  < 12, "Morning",
                            ifelse(hour(ymd_hms(datetime))  >= 12 & hour(ymd_hms(datetime)) < 18, "Midday", "Evening")))

# Make sure these are factors 
train$TimeOfDay <- as.factor(train$TimeOfDay)
train$season <- as.factor(train$season)
train$workingday <- as.factor(train$workingday)
train$weather <- as.factor(train$weather)
test$TimeOfDay <- as.factor(test$TimeOfDay)
test$season <- as.factor(test$season)
test$workingday <- as.factor(test$workingday)
test$weather <- as.factor(test$weather)

# SVM attempt - 1.09966 kaggle result
svm_model <- svm_linear(cost = 1, margin = 0.1) |> 
  set_mode("regression") |> 
  set_engine("kernlab")  %>% 
  translate()

# Kaggle result - 1.07226
svm_model <- svm_linear(cost = 1, margin = 1) |> 
  set_mode("regression") |> 
  set_engine("kernlab")  %>% 
  translate()

# Create workflow with model & recipe
my_recipe <- recipe(count ~ . , data=train) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>% 
  step_mutate(weather = factor(weather), season = factor(season), 
              workingday = factor(workingday), 
              TimeOfDay = factor(TimeOfDay)) %>%
  step_dummy(weather, season, workingday, TimeOfDay)  %>%
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors(), means = 0, sds = 1)  # For SVM predictors should have the same scale. 

# Prep and bake
prepped_recipe <- prep(my_recipe)
bake(prepped_recipe, new_data = train)

# Set workflow
svm_wf <- workflow() %>% 
  add_recipe(my_recipe) %>% 
  add_model(svm_model)

# Set up workflow 
final_wf <- svm_wf %>% 
  fit(data=train) 

# Predict
predictions <- predict(final_wf, new_data = test)
finalized_predictions <- exp(predictions) # Undo log from earlier

# Prepare for kaggle submission
kaggle_submission <- finalized_predictions %>%  
  bind_cols(., test) %>% 
  select(datetime, .pred) %>% 
  rename(count=.pred) %>% 
  mutate(count=pmax(0, count)) %>%
  mutate(datetime=as.character(format(datetime)))

# Slightly lower than last, but by .4
vroom::vroom_write(x=kaggle_submission, file="./SVM3Predictions.csv", delim=",")


#-------------------------------------------------------------------------------
# STACKING
library(stacks)

# Start with fresh data
train <- vroom::vroom("train.csv")
test <- vroom::vroom("test.csv")

train <- train |> 
  select(-c(holiday,temp, casual, registered)) |> 
  mutate(count = log(count), datetime = ymd_hms(datetime))

train <- train %>%
  mutate(TimeOfDay = ifelse(hour(ymd_hms(datetime)) >= 6 & hour(ymd_hms(datetime))  < 12, "Morning",
                            ifelse(hour(ymd_hms(datetime))  >= 12 & hour(ymd_hms(datetime)) < 18, "Midday", "Evening")))

test <- test %>%
  mutate(TimeOfDay = ifelse(hour(ymd_hms(datetime)) >= 6 & hour(ymd_hms(datetime))  < 12, "Morning",
                            ifelse(hour(ymd_hms(datetime))  >= 12 & hour(ymd_hms(datetime)) < 18, "Midday", "Evening")))

train <- train |> 
  select(-datetime)

my_recipe <- recipe(count ~ . , data=train) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>% 
  step_mutate(weather = factor(weather), season = factor(season), 
              workingday = factor(workingday)) %>%
  step_dummy(weather, season, workingday, TimeOfDay)  %>%
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors(), means = 0, sds = 1)  # For SVM predictors should hav


folds <- vfold_cv(train, v = 5, repeats = 1)

untunedModel <- control_stack_grid() 
tunedModel <- control_stack_resamples() 

preg_model <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(preg_model) 

preg_tuning_grid <- grid_regular(penalty(), mixture(), levels = 10)

preg_models <- preg_wf %>%
  tune_grid(resamples = folds, grid = preg_tuning_grid, metrics = metric_set(rmse), control = untunedModel)

lin_reg <- linear_reg() |>
  set_engine("lm") %>%
  set_mode("regression")

lin_reg_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(lin_reg)

lin_reg_model <- fit_resamples(lin_reg_workflow, resamples = folds, metrics=metric_set(rmse), control = tunedModel)

rf <- rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("regression")

rf_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(rf) 

tree_grid <- grid_regular(
  mtry(range = c(1, 40)), min_n(),             
  levels = 5                             
)

rf_models <- tune_grid(rf_wf, resamples = folds, grid = tree_grid, metrics = metric_set(rmse), control = untunedModel)

my_stack <- stacks() %>%
  add_candidates(preg_models) %>%
  add_candidates(lin_reg_model) %>%
  add_candidates(rf_models)

stack_mod <-my_stack %>%
  blend_predictions() %>%
  fit_members()


# Test 
test <- test |> 
  select(-c(holiday,temp, datetime))

stack_mod_predictions <- predict(stack_mod, new_data = test)

stack_mod_predictions <- exp(stack_mod_predictions)

# Readd datetime to 
test <- vroom::vroom("test.csv")
stack_mod_predictions$datetime <- test$datetime

kaggle_submission <- stack_mod_predictions %>%
  select(datetime, .pred) |>
  rename(count=.pred) |>
  mutate(count=pmax(0, count)) |>
  mutate(datetime=as.character(format(datetime)))

vroom::vroom_write(x=kaggle_submission, file="./StackedPreds.csv", delim=",")
# 1.04 as a result in Kaggle, not the best method








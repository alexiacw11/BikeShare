
# Load libraries
library(rpart)
library(tidymodels)

# Clean data
train <- vroom::vroom("train.csv")
test <- vroom::vroom("test.csv")

# Get rid of casual and registered variables in train set
train <- train |> 
  select(-casual, -registered)

# Change count to log(count)
train <- train |> 
  mutate(count = log(count),
         datetime = ymd_hms(datetime))

# This method didn't help the data as well
train <- train %>%
  mutate(TimeOfDay = ifelse(hour(ymd_hms(datetime)) >= 6 & hour(ymd_hms(datetime))  < 12, "Morning",
                            ifelse(hour(ymd_hms(datetime))  >= 12 & hour(ymd_hms(datetime)) < 18, "Midday", "Evening")))

test <- test %>%
  mutate(TimeOfDay = ifelse(hour(ymd_hms(datetime)) >= 6 & hour(ymd_hms(datetime))  < 12, "Morning",
                            ifelse(hour(ymd_hms(datetime))  >= 12 & hour(ymd_hms(datetime)) < 18, "Midday", "Evening")))
#---------------------------------------------------------------------------------------------------
# Clean data
train <- vroom::vroom("train.csv")
test <- vroom::vroom("test.csv")

# Get rid of casual and registered variables in train set
train <- train |> 
  select(-casual, -registered)

# Change count to log(count)
train <- train |> 
  mutate(count = log(count),
         datetime = ymd_hms(datetime))

# Make decision tree
tree_model <- decision_tree(tree_depth = tune(), cost_complexity = tune(),
                            min_n = tune()) |>  # type of model
  set_engine("rpart") |> # what r function to use
  set_mode("regression")

# Create workflow with model & recipe

# Simple recipe
my_recipe <- recipe(count ~ . , data=train)

# More complex recipe
my_recipe <- recipe(count ~ . , data=train) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>% 
  step_mutate(weather = factor(weather), season = factor(season), 
              workingday = factor(workingday), holiday = factor(holiday)) %>%
  step_zv(all_predictors()) %>%  # Removes zero-variance predictors
  step_rm(holiday, temp, weather)

# Prep and bake
prepped_recipe <- prep(my_recipe)
bake(prepped_recipe, new_data = test)

# Set workflow
tree_wf <- workflow() %>% 
  add_recipe(my_recipe) %>% 
  add_model(tree_model)

# Grid of values to tune over
grid_of_tuning_params <- grid_regular(tree_depth(), cost_complexity(), min_n(),
                                      levels = 5) # L^2 total tuning possibilties


# Split data for CV (5-10 groups)
folds <- vfold_cv(train, v=5, repeats = 1)

# Run the CV
CV_results <- tree_wf %>%
  tune_grid(resamples=folds, 
            grid=grid_of_tuning_params, 
            metrics=metric_set(rmse,mae,rsq)) # or leave metrics null

# Plot results
collect_metrics(CV_results) %>%
  filter(.metric == "rmse") %>%
  ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) + 
  geom_line()

# Find best tuning parameters
bestTune <- CV_results %>%
  select_best(metric = "rmse")

# Finalize the workflow and fit it
final_wf <- tree_wf %>%
  finalize_workflow(bestTune) %>% 
  fit(data=train)

# Predict
final_wf %>%
  predict(new_data = test)


predictions <- predict(final_wf, new_data = test)
finalized_predictions <- exp(predictions)

# Prepare for kaggle submission
kaggle_submission <- finalized_predictions %>%  
  bind_cols(., test) %>% 
  select(datetime, .pred) %>% 
  rename(count=.pred) %>% 
  mutate(count=pmax(0, count)) %>%
  mutate(datetime=as.character(format(datetime)))

# Slightly lower than last, but by .4
vroom_write(x=kaggle_submission, file="./ComplexTreePredictions.csv", delim=",")

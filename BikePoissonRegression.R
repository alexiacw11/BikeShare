library(tidyverse)
library(poissonreg)
library(caret)

# Set working directory
setwd("/Users/alexiacw11/Desktop/Fall_24/Stat348/BikeShare")

vroom::vroom("train.csv")
vroom::vroom("test.csv")

# Factorizing categorical variables
train <- train |> 
  mutate(across(c(season, holiday, workingday, weather), as.factor))
test <- test |> 
  mutate(across(c(season, holiday, workingday, weather), as.factor))

# Transforming weather so that there is no longer a fourth level. Combined instance to 3rd.
train$weather[train$weather == levels(train$weather)[4]] <- levels(train$weather)[3]
test$weather[test$weather == levels(test$weather)[4]] <- levels(test$weather)[3]
#------------------------------------------------------------------------------------------

pois_reg <- poisson_reg() |>
  set_engine("glm") |> 
  set_mode("regression") |> 
  fit(formula = count ~  windspeed + humidity + atemp + temp + weather + workingday + holiday + season, data = train)


# Generate predictions using linear model
bike_predictions <- predict(pois_reg, new_data = test)

# Transform for Kaggle
pois_kaggle <- bike_predictions %>%
bind_cols(., test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction 
  rename(count=.pred) %>% #rename pred to count (for submission 
  mutate(datetime=as.character(format(datetime))) #needed for right


## Write out the file8
vroom_write(x=pois_kaggle, file="./PoissonPreds.csv", delim = ",")

#------------------------------------------------------------------------------------------

# Look into creating a datetime variable that does categorical
head(train)
unique(train$season)

# Separate date, separate time, see if you can do that better 
# Look into season


# Season
ggplot(data = train, mapping = aes(x = season, y=count, fill = season)) + 
  geom_boxplot()


# There is an issue I am noticing - could this be a variable w/ near-zero variance
train |> 
  group_by(season) |> 
  summarize(count = n())

# Checking if there is near zero variance
# Turns out holiday is a near zero predictor
nearZeroVar(train, saveMetrics = TRUE)

#------------------------------------------------------------------------------------------
# Trying pois regression by taking out near zero predictor, holiday
pois_reg <- poisson_reg() |>
  set_engine("glm") |> 
  set_mode("regression") |> 
  fit(formula = count ~  windspeed + humidity + atemp + temp + weather + workingday + season, data = train)


# Generate predictions using linear model
bike_predictions <- predict(pois_reg, new_data = test)

# Transform for Kaggle
pois_kaggle <- bike_predictions %>%
  bind_cols(., test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction 
  rename(count=.pred) %>% #rename pred to count (for submission 
  mutate(datetime=as.character(format(datetime))) #needed for right


## Write out the file8
vroom_write(x=pois_kaggle, file="./NoHolidayPoissonPreds.csv", delim = ",")

# Predictions didn't improve as much as I was hoping for

#------------------------------------------------------------------------------------------

# New approach, separate datetime

library(lubridate)


# Changed datetime to actually be a date
train$datetime |> ymd_hms()

str(train)

sep <- train |> 
  mutate(years = year(ymd_hms(datetime)), months = month(ymd_hms(datetime)), 
         days = day(ymd_hms(datetime)))

#------------------------------------------------------------------------------------------
# Separated datetime approach
# Trying pois regression by taking out near zero predictor, holiday
pois_reg <- poisson_reg() |>
  set_engine("glm") |> 
  set_mode("regression") |> 
  fit(formula = count ~  windspeed + humidity + atemp + temp + weather 
      + workingday + season + years + months + days, data = sep)

test <- test |> 
  mutate(years = year(ymd_hms(datetime)), months = month(ymd_hms(datetime)), 
         days = day(ymd_hms(datetime)))


# Generate predictions using linear model
bike_predictions <- predict(pois_reg, new_data = test)

# Transform for Kaggle
pois_kaggle <- bike_predictions %>%
  bind_cols(., test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction 
  rename(count=.pred) %>% #rename pred to count (for submission 
  mutate(datetime=as.character(format(datetime))) #needed for right


## Write out the file8
vroom_write(x=pois_kaggle, file="./SepDTPoissonPreds.csv", delim = ",")

# Predictions didn't improve as much as I was hoping for

#------------------------------------------------------------------------------------------
# Regular datetime
pois_reg <- poisson_reg() |>
  set_engine("glm") |> 
  set_mode("regression") |> 
  fit(formula = count ~  windspeed + humidity + atemp + temp + weather 
      + workingday + season + datetime + years + months + days, data = sep)

test <- test |> 
  mutate(years = year(ymd_hms(datetime)), months = month(ymd_hms(datetime)), 
         days = day(ymd_hms(datetime)))


# Generate predictions using linear model
bike_predictions <- predict(pois_reg, new_data = test)

# Transform for Kaggle
pois_kaggle <- bike_predictions %>%
  bind_cols(., test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction 
  rename(count=.pred) %>% #rename pred to count (for submission 
  mutate(datetime=as.character(format(datetime))) #needed for right


## Write out the file8
vroom_write(x=pois_kaggle, file="./DTPoissonPreds.csv", delim = ",")






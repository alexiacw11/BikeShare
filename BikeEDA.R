library(tidyverse)
library(tidymodels)
library(skimr)
library(DataExplorer)
library(patchwork)
library(ggplot2)

# Reading in data 
train <- vroom::vroom("train.csv")
test <- vroom::vroom("test.csv")

# Lists the variables of each column 
train |> 
  dplyr::glimpse()

# Factorizing categorical variables
train <- train |> 
  mutate(across(c(season, holiday, workingday, weather), as.factor))
test <- test |> 
  mutate(across(c(season, holiday, workingday, weather), as.factor))

# Overview of the dataset says there is no missing data 
train |> 
  skim()

# Visualization of glimpse
train |> 
  plot_intro()

# Correlation plot
# Observation = casual and temp are correlated 0.49. Perhaps casual usage is 
# highly influence based on weather? Worth further investigation. 
train |> 
  plot_correlation()

# Histograms
#Observations = atemp + temp fairly normal, casual + count + registered + windspeed 
# all right skewed-, humidity left skewed
train |> 
  plot_histogram()

# Combined scatterplot and correlation map 
train |> 
  GGally::ggpairs()

# Lots of outliers in weather boxplot, potentially dangerous
ggplot(data = train, mapping = aes(x = weather, y=count, fill = weather)) + 
  geom_boxplot()

# Bar plot of weather
# Observation = low count for 4th weather group, will need to take care of this
weather_plot <- ggplot(data = train, mapping = aes(x = weather, fill = weather)) + 
  geom_bar() + xlab("Weather") + ylab("Count")

# Scatterplot of humidity
# Observation = count slightly decreases as humidity increases.
humidity_plot <- ggplot(data = train, mapping = aes(x = humidity, y=count)) + 
  geom_point() + geom_smooth(se=FALSE)  + xlab("Humidity") + ylab("Count")

# Histogram of windspeed
# Observation = heavily right skewed
windspeed_plot <- ggplot(data = train, mapping = aes(x = windspeed)) + 
  geom_histogram(fill = "forestgreen") + xlab("Windspeed") + ylab("Count")

# Histogram of temperature feel
# Observation = Fairly normal
atemp_plot <- ggplot(data = train, mapping = aes(x = atemp)) + 
  geom_histogram(fill = "magenta4") + xlab("Atemp") + ylab("Count")

# 4 panel plot
weather_plot + humidity_plot + windspeed_plot + atemp_plot


# Casual vs. Registered Boxplot by Holiday
# Observation = casual has a slightly higher median, wider range, and less outliers while on holiday.
# Registered also has less outliers on holiday, but pretty even spread otherwise between holiday and not. 
casual_holiday <- ggplot(data = train, mapping = aes(x = holiday, y=casual)) + 
  geom_boxplot() + xlab("Holiday") + ylab("Casual")
registered_holiday <- ggplot(data = train, mapping = aes(x = holiday, y=registered)) + 
  geom_boxplot() + xlab("Holiday") + ylab("Registered")

casual_holiday + registered_holiday

# Casual vs. Registered Boxplot by Working Day
# Observation = casual has a huge difference in spread when not a working day,
# but less outliers when it is a working day. Registered tends to have a similar spread besides
# working day having more outliers.
casual_work <- ggplot(data = train, mapping = aes(x = workingday, y=casual)) + 
  geom_boxplot() + xlab("Working Day") + ylab("Casual")
registered_work <- ggplot(data = train, mapping = aes(x = workingday, y=registered)) + 
  geom_boxplot() + xlab("Working Day") + ylab("Registered")

casual_work + registered_work

# Casual vs. Registered Holiday and Working Day
(casual_holiday + registered_holiday)/ (casual_work + registered_work)




setwd('C:/Users/Adam Cihláø/Desktop/materiály/ForecastingModels/assignments/exercise3')

library(tidyverse)
library(readxl)
library(mltools)
library(data.table)

data <- read_xls(path = '../data/Sales new cars US.xls', skip = 10) %>%
    mutate(observation_date = as.Date(observation_date),
           m = lubridate::month(observation_date),
           time_index = c(1:nrow(.)),
           time_index_2 = time_index^2)

data$m <- as.factor(data$m)
data <- as_tibble(one_hot(as.data.table(data)))

train_data <- data %>%
    filter(observation_date < '2015-01-01')

test_data <- data %>%
    filter(observation_date >= '2015-01-01')

trend_m <- lm(formula = TOTALNSA ~ time_index + time_index_2, train_data)
summary(trend_m)

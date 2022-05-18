
# get ready
rm(list = ls())
dev.off()
setwd("C:/Users/Adam Cihlár/Desktop/materiály/ForecastingModels/assignments/exam/")

# libraries
library(tidyverse)
library(readxl)
library(gridExtra)

# constants
n_vars <- 10

# functions


# load and preprocess data
dataset <- read_excel('data/112839377-Homeexam OKA2014 - 2022 Sjur Westgaard_112853997_1652444012642.xlsx')
dataset['date'] <- as.Date(dataset$dateid01)

# plot timeseries
ts_plots <- map(
    1:n_vars,
    ~ ggplot(data = dataset, mapping = aes_string(x='date', y=colnames(dataset[.+1]))) + 
        geom_line(color = 'steelblue') +
        theme_bw() +
        scale_x_date(name = element_blank(), date_minor_breaks = "5 year")
        #scale_y_continuous(name = "Value")
    )
grid.arrange(grobs = ts_plots, ncol = 2)


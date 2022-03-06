
rm(list = ls())
dev.off()

library(tidyverse)
#library(RCurl)
library(readxl)
#library(mltools)
#library(data.table)
library(gridExtra)
library(stargazer)

data <- read_xls(path = 'https://raw.githubusercontent.com/adamcihlar/IntroductionToForecastingModels/main/data_presentation/10year_state_bond_norway.xls', skip = 10)

####----pkg
library(randomForest)
library(tidyverse)
library(magrittr)
library(explainer)

####---- general function to create a grid
#run on priyangt
#Predictions based on different variables
## load random forest
load("phdproject2/daily_training.rda") # training data
load("phdproject2/rfu_m4daily.rda") # random forest model
load("phdproject2/subset_daily.rda")

subset_daily <- subset_daily[,1:26]

sediff_acf5.seasonal_strength1.d <- twowayinteraction(rfu_m4daily, sediff_acf5, seasonal_strength1, 
                                  fulldf = daily_training,
                                  subsetdf = subset_daily, grid.resolution=20)
save(sediff_acf5.seasonal_strength1.d, file="phdproject2/sediff_acf5.seasonal_strength1.d.rda")

diff1y_pacf5.seasonal_strength1.d <- twowayinteraction(rfu_m4daily, diff1y_pacf5, seasonal_strength1, 
                                                  fulldf = daily_training,
                                                  subsetdf = subset_daily, grid.resolution=20)
save(diff1y_pacf5.seasonal_strength1.d, file="phdproject2/diff1y_pacf5.seasonal_strength1.d.rda")

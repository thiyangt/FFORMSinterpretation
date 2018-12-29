####----pkg
library(randomForest)
library(tidyverse)
library(magrittr)
library(explainer)

####---- general function to create a grid

#Predictions based on different variables
## load random forest
load("phdproject2/hourly_training.rda") # training data
load("phdproject2/rfu_m4hourly.rda") # random forest model
load("phdproject2/subset_hourly.rda")

subset_hourly <- subset_hourly[,1:26]

entropy.seasonal_strength1.h <- twowayinteraction(rfu_m4hourly, entropy, seasonal_strength1, 
                                  fulldf = hourly_training,
                                  subsetdf = subset_hourly, grid.resolution=20)
save(entropy.seasonal_strength1.h, file="phdproject2/entropy.seasonal_strength1.h.rda")

hurst.seasonal_strength1.h <- twowayinteraction(rfu_m4hourly, hurst, seasonal_strength1, 
                                                  fulldf = hourly_training,
                                                  subsetdf = subset_hourly, grid.resolution=20)
save(hurst.seasonal_strength1.h, file="phdproject2/hurst.seasonal_strength1.h.rda")

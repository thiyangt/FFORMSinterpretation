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

linearity.diff2y_pacf5.h <- twowayinteraction(rfu_m4hourly, linearity, diff2y_pacf5, 
                                  fulldf = hourly_training,
                                  subsetdf = subset_hourly, grid.resolution=20)
save(linearity.diff2y_pacf5.h, file="phdproject2/linearity.diff2y_pacf5.h.rda")

linearity.sediff_seacf1.h <- twowayinteraction(rfu_m4hourly,linearity, sediff_seacf1, 
                                                  fulldf = hourly_training,
                                                  subsetdf = subset_hourly, grid.resolution=20)
save(linearity.sediff_seacf1.h, file="phdproject2/linearity.sediff_seacf1.h.rda")

entropy.trend.h <- twowayinteraction(rfu_m4hourly,entropy, trend, 
                                               fulldf = hourly_training,
                                               subsetdf = subset_hourly, grid.resolution=20)
save(entropy.trend.h, file="phdproject2/entropy.trend.h.rda")

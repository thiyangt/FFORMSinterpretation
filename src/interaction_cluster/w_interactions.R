####----pkg
library(randomForest)
library(tidyverse)
library(magrittr)
library(explainer)

####---- general function to create a grid
#run on priyangt
#Predictions based on different variables
## load random forest
load("phdproject2/weekly_training.rda") # training data
load("phdproject2/rfu_m4weekly.rda") # random forest model
load("phdproject2/subset_weekly.rda")

subset_weekly <- subset_weekly[,c(1:26, 28)]

lumpiness.trend.w <- twowayinteraction(rfu_m4weekly, lumpiness, trend, 
                                  fulldf = weekly_training,
                                  subsetdf = subset_weekly, grid.resolution=20)
save(lumpiness.trend.w, file="phdproject2/lumpiness.trend.w.rda")

stability.trend.w <- twowayinteraction(rfu_m4weekly, stability, trend, 
                                       fulldf = weekly_training,
                                       subsetdf = subset_weekly, grid.resolution=20)
save(stability.trend.w, file="phdproject2/stability.trend.w.rda")
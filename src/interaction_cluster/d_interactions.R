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

stability.seasonal_strength2.d <- twowayinteraction(rfu_m4daily, stability, seasonal_strength2, 
                                  fulldf = daily_training,
                                  subsetdf = subset_daily, grid.resolution=20)
save(stability.seasonal_strength2.d, file="phdproject2/stability.seasonal_strength2.d.rda")
# 
# curvature.seas_pacf.d <- twowayinteraction(rfu_m4daily, curvature, seas_pacf, 
#                                                   fulldf = daily_training,
#                                                   subsetdf = subset_daily, grid.resolution=20)
# save(curvature.seas_pacf.d, file="phdproject2/curvature.seas_pacf.d.rda")
# 
# seasonal_strength1.e_acf1.d <- twowayinteraction(rfu_m4daily, seasonal_strength1, e_acf1, 
#                                            fulldf = daily_training,
#                                            subsetdf = subset_daily, grid.resolution=20)
# save(seasonal_strength1.e_acf1.d, file="phdproject2/seasonal_strength1.e_acf1.d.rda")
# 
# seasonal_strength1.curvature.d <- twowayinteraction(rfu_m4daily, seasonal_strength1, curvature, 
#                                                  fulldf = daily_training,
#                                                  subsetdf = subset_daily, grid.resolution=20)
# save(seasonal_strength1.curvature.d, file="phdproject2/seasonal_strength1.curvature.d.rda")

####----pkg
library(randomForest)
library(tidyverse)
library(magrittr)
library(explainer)

####---- general function to create a grid

#Predictions based on different variables
## load random forest
load("phdproject2/monthly_training.rda") # training data
load("phdproject2/rfu_m4monthly.rda") # random forest model
load("phdproject2/subset_monthly.rda")

subset_monthly <- subset_monthly[,1:30]

#N.seasonality.m <- twowayinteraction(rfu_m4monthly, N, seasonality, 
#                                  fulldf = monthly_training,
#                                  subsetdf = subset_monthly, grid.resolution=20)
#save(N.seasonality.m, file="phdproject2/N.seasonality.m.rda")

#N.diff1y_acf1.m <- twowayinteraction(rfu_m4monthly, N, diff1y_acf1, 
#                                             fulldf = monthly_training,
#                                             subsetdf = subset_monthly, grid.resolution=20)
#save(N.diff1y_acf1.m, file="phdproject2/N.diff1y_acf1.m.rda")

N.hwgamma.m <- twowayinteraction(rfu_m4monthly, N, hwgamma, 
                                     fulldf = monthly_training,
                                     subsetdf = subset_monthly, grid.resolution=20)
save(N.hwgamma.m, file="phdproject2/N.hwgamma.m.rda")

seasinality.trend.m <- twowayinteraction(rfu_m4monthly, seasonality, trend, 
                                     fulldf = monthly_training,
                                     subsetdf = subset_monthly, grid.resolution=20)
save(seasinality.trend.m, file="phdproject2/seasinality.trend.m.rda")

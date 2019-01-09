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

sediff_acf5.sediff_seacf1.m <- twowayinteraction(rfu_m4monthly, sediff_acf5, sediff_seacf1, 
                                     fulldf = monthly_training,
                                     subsetdf = subset_monthly, grid.resolution=20)
save(sediff_acf5.sediff_seacf1.m, file="phdproject2/sediff_acf5.sediff_seacf1.m.rda")
# 
# spikiness.hurst.m <- twowayinteraction(rfu_m4monthly, spikiness, hurst, 
#                                      fulldf = monthly_training,
#                                      subsetdf = subset_monthly, grid.resolution=20)
# save(spikiness.hurst.m, file="phdproject2/spikiness.hurst.m.rda")
# 
# spikiness.lumpiness.m <- twowayinteraction(rfu_m4monthly, spikiness, lumpiness, 
#                                          fulldf = monthly_training,
#                                          subsetdf = subset_monthly, grid.resolution=20, trim2 = TRUE)
# save(spikiness.lumpiness.m, file="phdproject2/spikiness.lumpiness.m.rda")

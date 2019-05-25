####----pkg
library(randomForest)
library(tidyverse)
library(magrittr)
library(explainer)

####---- general function to create a grid

#Predictions based on different variables
## load random forest
load("phdproject2/yearly_training.rda") # training data
load("phdproject2/rfu_m4yearly.rda") # random forest model
load("phdproject2/subset_yearly.rda")
subset_yearly <- subset_yearly[,1:25]


lumpiness.entropy.y <- twowayinteraction(rfu_m4yearly, lumpiness, entropy,
                                  fulldf = yearly_training,
                                  subsetdf = subset_yearly, grid.resolution=20, trim1=FALSE)
save(lumpiness.entropy.y, file="phdproject2/lumpiness.entropy.y.rda")
# 
# eacf1.diff2yacf1.y <- twowayinteraction(rfu_m4yearly, e_acf1, diff2y_acf1, 
#                                              fulldf = yearly_training,
#                                              subsetdf = subset_yearly, grid.resolution=20)
# save(eacf1.diff2yacf1.y, file="phdproject2/eacf1.diff2yacf1.y.rda")
# 
# hurst.y_acf5.y <- twowayinteraction(rfu_m4yearly, hurst, y_acf5, 
#                                        fulldf = yearly_training,
#                                        subsetdf = subset_yearly, grid.resolution=20)
# save(hurst.y_acf5.y, file="phdproject2/hurst.y_acf5.y.rda")
# 
# hurst.e_acf1.y <- twowayinteraction(rfu_m4yearly, hurst, e_acf1, 
#                                     fulldf = yearly_training,
#                                     subsetdf = subset_yearly, grid.resolution=20)
# save(hurst.e_acf1.y, file="phdproject2/hurst.e_acf1.y.rda")

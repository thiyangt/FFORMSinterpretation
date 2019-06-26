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

# lumpiness.nonlinearity.w <- twowayinteraction(rfu_m4weekly, lumpiness, nonlinearity, 
#                                   fulldf = weekly_training,
#                                   subsetdf = subset_weekly, grid.resolution=20, trim1 = TRUE, trim2=TRUE)
# save(lumpiness.nonlinearity.w, file="phdproject2/lumpiness.nonlinearity.w.rda")
# 
# lumpiness.beta.w <- twowayinteraction(rfu_m4weekly, lumpiness, beta, 
#                                        fulldf = weekly_training,
#                                        subsetdf = subset_weekly, grid.resolution=20, trim1=TRUE)
# save(lumpiness.beta.w, file="phdproject2/lumpiness.beta.w.rda")

curvature.linearity.w <- twowayinteraction(rfu_m4weekly, curvature, linearity, 
                                       fulldf = weekly_training,
                                       subsetdf = subset_weekly, grid.resolution=20)
save(curvature.linearity.w, file="phdproject2/curvature.linearity.w.rda")

# stability.lumpiness.w <- twowayinteraction(rfu_m4weekly, stability, lumpiness, 
#                                      fulldf = weekly_training,
#                                      subsetdf = subset_weekly, grid.resolution=20, trim2 = TRUE)
# save(stability.lumpiness.w, file="phdproject2/stability.lumpiness.w.rda")
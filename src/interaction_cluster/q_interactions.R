####----pkg
library(randomForest)
library(tidyverse)
library(magrittr)
library(explainer)

####---- general function to create a grid

#Predictions based on different variables
## load random forest
load("phdproject2/quarterly_training.rda") # training data
load("phdproject2/rfu_m4quarterly.rda") # random forest model
load("phdproject2/subset_quarterly.rda")
subset_quarterly <- subset_quarterly[-431,]
subset_quarterly <- subset_quarterly[,1:30]

e_acf1.curvature.q <- twowayinteraction(rfu_m4quarterly, e_acf1, curvature, 
                                  fulldf = quarterly_training,
                                  subsetdf = subset_quarterly, grid.resolution=20)
save(e_acf1.curvature.q, file="phdproject2/e_acf1.curvature.q.rda")
# 
# diff1y_acf5.beta.q <- twowayinteraction(rfu_m4quarterly, diff1y_acf5, beta, 
#                                              fulldf = quarterly_training,
#                                              subsetdf = subset_quarterly, grid.resolution=20)
# save(diff1y_acf5.beta.q, file="phdproject2/diff1y_acf5.beta.q.rda")
# 
# diff1y_acf5.stability.q <- twowayinteraction(rfu_m4quarterly, diff1y_acf5, stability, 
#                                          fulldf = quarterly_training,
#                                          subsetdf = subset_quarterly, grid.resolution=20)
# save(diff1y_acf5.stability.q, file="phdproject2/diff1y_acf5.stability.q.rda")

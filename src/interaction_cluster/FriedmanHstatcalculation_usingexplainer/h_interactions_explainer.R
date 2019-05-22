##on my laptop
####----pkg
library(randomForest)
library(tidyverse)
library(magrittr)
library(explainer)

####---- general function to create a grid
#Predictions based on different variables
## load random forest
load("data/hourly/hourly_training.rda") # training data
load("data/hourly/rfu_m4hourly.rda") # random forest model
load("data/hourly/subset_hourly.rda")
subset_hourly <- subset_hourly[,1:26]


subsetdf <- subset_hourly
model <- rfu_m4hourly
fulldf <- hourly_training
allfeatures <- colnames(hourly_training)[-27]
grid.resolution <- 10
trimfeatures <- c("linearity", "curvature")
classnames <- c("mstlarima", 
                "mstlets", 
                "nn",
                "rw",
                "rwd",
                "snaive",
                "stlar",
                "tbats",
                "theta",
                "wn")


friedman_hourly_explainer <- friedmanHstat(model, fulldf, subsetdf, allfeatures,trimfeatures, grid.resolution, classnames)
save(friedman_yearly_explainer, file="phdproject2/friedman_yearly_explainer.rda")
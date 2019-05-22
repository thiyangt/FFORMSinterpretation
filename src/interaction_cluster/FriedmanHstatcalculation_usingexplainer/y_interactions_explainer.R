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


subsetdf <- subset_yearly
model <- rfu_m4yearly
fulldf <- yearly_training
allfeatures <- colnames(yearly_training)[-25]
grid.resolution <- 10
trimfeatures <- c("ur_pp", "linearity", "curvature")
classnames <- c("ARIMA", 
                "ARMA/AR/MA", 
                "ETS-dampedtrend",
                "ETS-notrendnoseasonal",
                "ETS-trend",
                "nn",
                "rw",
                "rwd",
                "theta",
                "wn")


friedman_yearly_explainer <- friedmanHstat(model, fulldf, subsetdf, allfeatures,trimfeatures, grid.resolution, classnames)
save(friedman_yearly_explainer, file="phdproject2/friedman_yearly_explainer.rda")
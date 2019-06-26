##--pkg
library(forecast)
library(randomForest)
library(iml)
library(doParallel)

##---data
load("phdproject2/yearly_training.rda") # training data
load("phdproject2/rfu_m4yearly.rda") # random forest model
load("phdproject2/subset_yearly.rda")

##---Create a prediction function
X <- yearly_training[, -26]
Y <- yearly_training$classlabels
mod = Predictor$new(rfu_m4yearly, data=X, y=Y, type="prob")
overall_interaction_y <- Interaction$new(mod)
overall_interactions_y <- overall_interaction_y$results
save(overall_interactions_y, file="phdproject2/overall_interactions_y.rda")
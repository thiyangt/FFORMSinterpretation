##--pkg
library(forecast)
library(randomForest)
library(iml)
library(doParallel)

##---data
load("phdproject2/monthly_training.rda") # training data
load("phdproject2/rfu_m4monthly.rda") # random forest model
load("phdproject2/subset_monthly.rda")

##---Create a prediction function
X <- subset_monthly[, -31]
Y <- subset_monthly$classlabels
mod = Predictor$new(rfu_m4monthly, data=X, y=Y, type="prob")
overall_interaction_m <- Interaction$new(mod)
overall_interactions_m <- overall_interaction_m$results
save(overall_interactions_m, file="phdproject2/overall_interactions_m.rda")
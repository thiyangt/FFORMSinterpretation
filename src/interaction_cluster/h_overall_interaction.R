##--pkg
library(forecast)
library(randomForest)
library(iml)
library(doParallel)

##---data
load("phdproject2/hourly_training.rda") # training data
load("phdproject2/rfu_m4hourly.rda") # random forest model
load("phdproject2/subset_hourly.rda")


##---Create a prediction function
X <- subset_hourly[, -27]
Y <- subset_hourly$classlabels
mod = Predictor$new(rfu_m4hourly, data=X, y=Y, type="prob")
overall_interaction_h <- Interaction$new(mod)
overall_interactions_h <- overall_interaction_h$results
save(overall_interactions_h, file="phdproject2/overall_interactions_h.rda")
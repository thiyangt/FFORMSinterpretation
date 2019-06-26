##--pkg
library(forecast)
library(randomForest)
library(iml)
library(doParallel)

##---data
load("phdproject2/weekly_training.rda") # training data
load("phdproject2/rfu_m4weekly.rda") # random forest model
load("phdproject2/subset_weekly.rda")


##---Create a prediction function
X <- subset_weekly[, -27]
Y <- subset_weekly$classlabels
mod = Predictor$new(rfu_m4weekly, data=X, y=Y, type="prob")
overall_interaction_w <- Interaction$new(mod)
overall_interactions_w <- overall_interaction_w$results
save(overall_interactions_w, file="phdproject2/overall_interactions_w.rda")
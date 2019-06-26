##--pkg
library(forecast)
library(randomForest)
library(iml)
library(doParallel)

##---data
load("phdproject2/daily_training.rda") # training data
load("phdproject2/rfu_m4daily.rda") # random forest model
load("phdproject2/subset_daily.rda")


##---Create a prediction function
X <- subset_daily[, -27]
Y <- subset_daily$classlabels
mod = Predictor$new(rfu_m4daily, data=X, y=Y, type="prob")
overall_interaction_d <- Interaction$new(mod)
overall_interactions_d <- overall_interaction_d$results
save(overall_interactions_d, file="phdproject2/overall_interactions_d.rda")
##--pkg
library(forecast)
library(randomForest)
library(iml)
library(doParallel)

##---data
load("phdproject2/quarterly_training.rda") # training data
load("phdproject2/rfu_m4quarterly.rda") # random forest model
load("phdproject2/subset_quarterly.rda")


##---Create a prediction function
X <- subset_quarterly[, -31]
Y <- subset_quarterly$classlabels
mod = Predictor$new(rfu_m4quarterly, data=X, y=Y, type="prob")
overall_interaction_q <- Interaction$new(mod)
overall_interactions_q <- overall_interaction_q$results
save(overall_interactions_q, file="phdproject2/overall_interactions_q.rda")
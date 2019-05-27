##--pkg
library(forecast)
library(randomForest)
library(iml)
library(ggplot2)

##---data
load("phdproject2/rfu_m4yearly.rda") # random forest model
load("phdproject2/subset_yearly.rda")

##---Create a prediction function
X <- subset_yearly[, -26]
Y <- subset_yearly$classlabels
predictor = Predictor$new(rfu_m4yearly, data=X, y=Y, type="prob")

##compute feature interaction
interact_trend_prob = Interaction$new(predictor, feature = "ur_pp")
trend_pob_interact <- interact_trend_prob$results
save(trend_pob_interact, file="phdproject2/trend_pob_interact.rda")
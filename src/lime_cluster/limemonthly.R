library(lime)
library(randomForest)
# create a test set
load("phdproject2/monthly_training.rda") # training data
load("phdproject2/rfu_m4monthly.rda") # random forest model

mtest <- monthly_training[c(1, 971, 980, 632, 403, 1810, 445, 999, 997, 2036), 1:30]
model_type.randomForest <- function(x,...){
  return("classification")
}
predict_model.randomForest = function(x, newdata, type, ...){
  results <- as.data.frame(predict(x, newdata, type="prob"))
  return(results)}

explainer <- lime(mtest, rfu_m4monthly)

explanationm <- explain(
  mtest,
  explainer = explainer,
  n_labels=1,
  n_features=10
)

save(explanationm, file="phdproject2/explanationm.rda")
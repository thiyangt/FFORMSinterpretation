library(lime)
library(randomForest)
# create a test set
load("phdproject2/hourly_training.rda") # training data
load("phdproject2/rfu_m4hourly.rda") # random forest model

htest <- hourly_training[c(53,199, 196, 137), 1:26]
model_type.randomForest <- function(x,...){
  return("classification")
}
predict_model.randomForest = function(x, newdata, type, ...){
  results <- as.data.frame(predict(x, newdata, type="prob"))
  return(results)}

explainer <- lime(htest, rfu_m4hourly)

explanationh <- explain(
  htest,
  explainer = explainer,
  n_labels=1,
  n_features=10
)

save(explanationh, file="phdproject2/explanationh.rda")
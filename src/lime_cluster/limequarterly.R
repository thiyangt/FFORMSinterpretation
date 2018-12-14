library(lime)
library(randomForest)
# create a test set
load("phdproject2/quarterly_training.rda") # training data
load("phdproject2/rfu_m4quarterly.rda") # random forest model

qtest <- quarterly_training[c(25,405, 653, 908), 1:30]
model_type.randomForest <- function(x,...){
  return("classification")
}
predict_model.randomForest = function(x, newdata, type, ...){
  results <- as.data.frame(predict(x, newdata, type="prob"))
  return(results)}

explainer <- lime(qtest, rfu_m4quarterly)

explanationq <- explain(
  qtest,
  explainer = explainer,
  n_labels=1,
  n_features=10
)

save(explanationq, file="phdproject2/explanationq.rda")
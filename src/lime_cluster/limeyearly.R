library(lime)
library(randomForest)
# create a test set
load("phdproject2/yearly_training.rda") # training data
load("phdproject2/rfu_m4yearly.rda") # random forest model

ytest <- yearly_training[c(373, 227743, 393474, 13442, 480357, 79), 1:25]
model_type.randomForest <- function(x,...){
  return("classification")
}
predict_model.randomForest = function(x, newdata, type, ...){
  results <- as.data.frame(predict(x, newdata, type="prob"))
  return(results)}

explainer <- lime(ytest, rfu_m4yearly)

explanationy <- explain(
  ytest,
  explainer = explainer,
  n_labels=1,
  n_features=10
)

save(explanationy, file="phdproject2/explanationy.rda")
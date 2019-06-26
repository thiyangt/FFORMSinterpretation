## --- Train a random forest
library(seer)
library(forecast)
library(randomForest)

load("phdproject2/featuresM4Y_test.rda")
load("phdproject2/yearly_training.rda")

yearly_ru <- build_rf(yearly_training, featuresM4Y_test, rf_type="ru", ntree=1000, seed=1, import = TRUE)
predictions_yearly_ru <- yearly_ru$predictions
rfu_m4yearly <- yearly_ru$randomforest
save(predictions_yearly_ru, file="phdproject2/predictions_yearly_ru.rda")
save(rfu_m4yearly, file="phdproject2/rfu_m4yearly.rda")

train_features <- yearly_training[, 1:25]
train_predictions <- predict(rfu_m4yearly, newdata = train_features)
train_predictions_oob <- rfu_m4yearly$predicted
train_confusion <- rfu_m4yearly$confusion
train_votes <- rfu_m4yearly$votes
train_importance <- rfu_m4yearly$importance
train_importanceSD <- rfu_m4yearly$importanceSD
train_localImportance <- rfu_m4yearly$localImportance
train_oob.times <- rfu_m4yearly$oob.times
save(train_predictions, file="phdproject2/train_predictions.rda")
save(train_predictions_oob, file="phdproject2/train_predictions_oob.rda")
save(train_confusion, file="phdproject2/train_confusion.rda")
save(train_votes, file="phdproject2/train_votes.rda")
save(train_importance, file="phdproject2/train_importance.rda")
save(train_importanceSD, file="phdproject2/train_importanceSD.rda")
save(train_localImportance, file="phdproject2/train_localImportance.rda")
save(train_oob.times, file="phdproject2/train_oob.times.rda")
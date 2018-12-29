####----pkg
library(randomForest)
library(tidyverse)
library(magrittr)
library(explainer)

####---- general function to create a grid

#Predictions based on different variables
## load random forest
load("phdproject2/quarterly_training.rda") # training data
load("phdproject2/rfu_m4quarterly.rda") # random forest model
load("phdproject2/subset_quarterly.rda")
subset_quarterly <- subset_quarterly[,1:30]

trend.seasonality.q <- twowayinteraction(rfu_m4quarterly, trend, seasonality, 
                                  fulldf = quarterly_training,
                                  subsetdf = subset_quarterly, grid.resolution=20)
save(trend.seasonality.q, file="phdproject2/trend.seasonality.q.rda")

seasonality.lumpiness.q <- twowayinteraction(rfu_m4quarterly, seasonality, lumpiness, 
                                             fulldf = quarterly_training,
                                             subsetdf = subset_quarterly, grid.resolution=20)
save(seasonality.lumpiness.q, file="phdproject2/seasonality.lumpiness.q.rda")

diff1ypacf5.seasonality.q <- twowayinteraction(rfu_m4quarterly, diff1y_pacf5, seasonality, 
                                         fulldf = quarterly_training,
                                         subsetdf = subset_quarterly, grid.resolution=20)
save(diff1ypacf5.seasonality.q, file="phdproject2/diff1ypacf5.seasonality.q.rda")

seasonality.spikiness.q <- twowayinteraction(rfu_m4quarterly, seasonality, spikiness, 
                                             fulldf = quarterly_training,
                                             subsetdf = subset_quarterly, grid.resolution=20)
save(seasonality.spikiness.q, file="phdproject2/seasonality.spikiness.q.rda")

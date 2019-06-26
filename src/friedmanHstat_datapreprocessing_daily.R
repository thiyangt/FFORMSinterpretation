## packages
library(iml) # Friedman's H statistic calculation package
library(tidyverse)

## Daily data-------------------
## load all Friedman's H statistic calculation files
load("data/FriedmanHstat/daily/entropy_interacD.rda")
load("data/FriedmanHstat/daily/lumpiness_interactD.rda")
load("data/FriedmanHstat/daily/stability_interactD.rda")
load("data/FriedmanHstat/daily/hurst_interactD.rda")
load("data/FriedmanHstat/daily/trend_interactD.rda")
load("data/FriedmanHstat/daily/pikiness_interactD.rda")
load("data/FriedmanHstat/daily/linearity_interactD.rda")
load("data/FriedmanHstat/daily/curvature_interactD.rda")
load("data/FriedmanHstat/daily/e_acf1_interactD.rda")
load("data/FriedmanHstat/daily/y_acf1_interactD.rda")
load("data/FriedmanHstat/daily/diff1y_acf1_interactD.rda")
load("data/FriedmanHstat/daily/diff2y_acf1_interactD.rda")
load("data/FriedmanHstat/daily/y_pacf5_interactD.rda")
load("data/FriedmanHstat/daily/diff1y_pacf5_interactD.rda")
load("data/FriedmanHstat/daily/diff2y_pacf5_interactD.rda")
load("data/FriedmanHstat/daily/nonlinearity_interactD.rda")
load("data/FriedmanHstat/daily/seas_pacf_interactD.rda")
load("data/FriedmanHstat/daily/seasonal_strength1_interactD.rda")
load("data/FriedmanHstat/daily/seasonal_strength2_interactD.rda")
load("data/FriedmanHstat/daily/sediff_acf1_interactD.rda")
load("data/FriedmanHstat/daily/sediff_seacf1_interactD.rda")
load("data/FriedmanHstat/daily/sediff_acf5_interactD.rda")
load("data/FriedmanHstat/daily/N_interactD.rda")
load("data/FriedmanHstat/daily/y_acf5_interactD.rda")
load("data/FriedmanHstat/daily/diff1y_acf5_interactD.rda")
load("data/FriedmanHstat/daily/diff2y_acf5_interactD.rda")


## merge all Friedman's H statistic calculations to one data frame
friedmanHstat_daily <- do.call("rbind", list(
  entropy_interactD,
  lumpiness_interactD,
  stability_interactD,
  hurst_interactD,
  trend_interactD,
  spikiness_interactD,
  linearity_interactD,
  curvature_interactD,
  e_acf1_interactD,
  y_acf1_interactD,
  diff1y_acf1_interactD,
  diff2y_acf1_interactD,
  y_pacf5_interactD,
  diff1y_pacf5_interactD,
  diff2y_pacf5_interactD,
  nonlinearity_interactD,
  seas_pacf_interactD,
  seasonal_strength1_interactD,
  seasonal_strength2_interactD,
  sediff_acf1_interactD,
  sediff_seacf1_interactD,
  sediff_acf5_interactD,
  N_interactD,
  y_acf5_interactD,
  diff1y_acf5_interactD,
  diff2y_acf5_interactD
))

names(friedmanHstat_daily) <- c("feature", "class", "interaction")
head(friedmanHstat_daily)

# devide the feature column to feature 1 and feature 2
friedmanHstat_daily <- friedmanHstat_daily %>% separate(
  feature, into=c("feature1", "feature2"), ":") %>% select(c("feature1", "feature2", "class", "interaction"))
table(friedmanHstat_daily$feature1)
table(friedmanHstat_daily$feature2)
# replace all NA and Inf swith zero
is.na(friedmanHstat_daily) <- sapply(friedmanHstat_daily, is.infinite)
friedmanHstat_daily[is.na(friedmanHstat_daily)] <- 0
summary(friedmanHstat_daily$interaction)
boxplot(friedmanHstat_daily$interaction)
# replace all values greater than 1 to 1
friedmanHstat_daily$interaction[friedmanHstat_daily$interaction > 1.0] <- 1
head(friedmanHstat_daily)
friedmanHstat_daily$class <- as.character(friedmanHstat_daily$class)
save(friedmanHstat_daily, file="data/friedmanHstat_daily.rda")


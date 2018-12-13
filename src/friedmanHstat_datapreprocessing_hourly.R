## packages
library(iml) # Friedman's H statistic calculation package
library(tidyverse)

## Daily data-------------------
## load all Friedman's H statistic calculation files
load("data/FriedmanHstat/hourly/entropy_interactH.rda")
load("data/FriedmanHstat/hourly/lumpiness_interactH.rda")
load("data/FriedmanHstat/hourly/stability_interactH.rda")
load("data/FriedmanHstat/hourly/hurst_interactH.rda")
load("data/FriedmanHstat/hourly/trend_interactH.rda")
load("data/FriedmanHstat/hourly/pikiness_interactH.rda")
load("data/FriedmanHstat/hourly/linearity_interactH.rda")
load("data/FriedmanHstat/hourly/curvature_interactH.rda")
load("data/FriedmanHstat/hourly/e_acf1_interactH.rda")
load("data/FriedmanHstat/hourly/y_acf1_interactH.rda")
load("data/FriedmanHstat/hourly/diff1y_acf1_interactH.rda")
load("data/FriedmanHstat/hourly/diff2y_acf1_interactH.rda")
load("data/FriedmanHstat/hourly/y_pacf5_interactH.rda")
load("data/FriedmanHstat/hourly/diff1y_pacf5_interactH.rda")
load("data/FriedmanHstat/hourly/diff2y_pacf5_interactH.rda")
load("data/FriedmanHstat/hourly/nonlinearity_interactH.rda")
load("data/FriedmanHstat/hourly/seas_pacf_interactH.rda")
load("data/FriedmanHstat/hourly/seasonal_strength1_interactM.rda")
load("data/FriedmanHstat/hourly/seasonal_strength2_interactM.rda")
load("data/FriedmanHstat/hourly/sediff_acf1_interactH.rda")
load("data/FriedmanHstat/hourly/sediff_seacf1_interactH.rda")
load("data/FriedmanHstat/hourly/sediff_acf5_interactH.rda")
load("data/FriedmanHstat/hourly/N_interactH.rda")
load("data/FriedmanHstat/hourly/y_acf5_interactH.rda")
load("data/FriedmanHstat/hourly/diff1y_acf5_interactH.rda")
load("data/FriedmanHstat/hourly/diff2y_acf5_interactH.rda")


## merge all Friedman's H statistic calculations to one data frame
friedmanHstat_hourly <- do.call("rbind", list(
  entropy_interactH,
  lumpiness_interactH,
  stability_interactH,
  hurst_interactH,
  trend_interactH,
  spikiness_interactH,
  linearity_interactH,
  curvature_interactH,
  e_acf1_interactH,
  y_acf1_interactH,
  diff1y_acf1_interactH,
  diff2y_acf1_interactH,
  y_pacf5_interactH,
  diff1y_pacf5_interactH,
  diff2y_pacf5_interactH,
  nonlinearity_interactH,
  seas_pacf_interactH,
  seasonal_strength1_interactM,
  seasonal_strength2_interactM,
  sediff_acf1_interactH,
  sediff_seacf1_interactH,
  sediff_acf5_interactH,
  N_interactH,
  y_acf5_interactH,
  diff1y_acf5_interactH,
  diff2y_acf5_interactH
))

names(friedmanHstat_hourly) <- c("feature", "class", "interaction")
head(friedmanHstat_hourly)

# devide the feature column to feature 1 and feature 2
friedmanHstat_hourly <- friedmanHstat_hourly %>% separate(
  feature, into=c("feature1", "feature2"), ":") %>% select(c("feature1", "feature2", "class", "interaction"))
table(friedmanHstat_hourly$feature1)
table(friedmanHstat_hourly$feature2)
# replace all NA and Inf swith zero
is.na(friedmanHstat_hourly) <- sapply(friedmanHstat_hourly, is.infinite)
friedmanHstat_hourly[is.na(friedmanHstat_hourly)] <- 0
summary(friedmanHstat_hourly$interaction)
boxplot(friedmanHstat_hourly$interaction)
# replace all values greater than 1 to 1
friedmanHstat_hourly$interaction[friedmanHstat_hourly$interaction > 1.0] <- 1
head(friedmanHstat_hourly)
friedmanHstat_hourly$class <- as.character(friedmanHstat_hourly$class)
save(friedmanHstat_hourly, file="data/friedmanHstat_hourly.rda")


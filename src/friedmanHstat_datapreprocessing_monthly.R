## packages
library(iml) # Friedman's H statistic calculation package
library(tidyverse)

## Yearly data-------------------
## load all Friedman's H statistic calculation files
load("data/FriedmanHstat/entropy_interactM.rda")
load("data/FriedmanHstat/lumpiness_interactM.rda")
load("data/FriedmanHstat/stability_interactM.rda")
load("data/FriedmanHstat/hurst_interactM.rda")
load("data/FriedmanHstat/trend_interactM.rda")
load("data/FriedmanHstat/spikiness_interactM.rda")
load("data/FriedmanHstat/linearity_interactM.rda")
load("data/FriedmanHstat/curvature_interactM.rda")
load("data/FriedmanHstat/e_acf1_interactM.rda")
load("data/FriedmanHstat/y_acf1_interactM.rda")
load("data/FriedmanHstat/diff1y_acf1_interactM.rda")
load("data/FriedmanHstat/diff2y_acf1_interactM.rda")
load("data/FriedmanHstat/y_pacf5_interactM.rda")
load("data/FriedmanHstat/diff1y_pacf5_interactM.rda")
load("data/FriedmanHstat/diff2y_pacf5_interactM.rda")
load("data/FriedmanHstat/nonlinearity_interactM.rda")
load("data/FriedmanHstat/seas_pacf_interactM.rda")
load("data/FriedmanHstat/seasonality_interactM.rda")
load("data/FriedmanHstat/hwalpha_interactM.rda")
load("data/FriedmanHstat/hwbeta_interactM.rda")
load("data/FriedmanHstat/hwgamma_interactM.rda")
load("data/FriedmanHstat/sediff_acf1_interactM.rda")
load("data/FriedmanHstat/sediff_seacf1_interactM.rda")
load("data/FriedmanHstat/sediff_acf5_interactM.rda")
load("data/FriedmanHstat/N_interactM.rda")
load("data/FriedmanHstat/y_acf5_interactM.rda")
load("data/FriedmanHstat/diff1y_acf5_interactM.rda")
load("data/FriedmanHstat/diff2y_acf5_interactM.rda")
load("data/FriedmanHstat/alpha_interactM.rda")
load("data/FriedmanHstat/beta_interactM.rda")

## merge all Friedman's H statistic calculations to one data frame
friedmanHstat_monthly <- do.call("rbind", list(
  entropy_interactM, lumpiness_interactM,
  stability_interactM, hurst_interactM,
  trend_interactM, spikiness_interactM,
  linearity_interactM, curvature_interactM,
  e_acf1_interactM, y_acf1_interactM,
  diff1y_acf1_interactM, diff2y_acf1_interactM,
  y_pacf5_interactM, diff1y_pacf5_interactM,
  diff2y_pacf5_interactM, nonlinearity_interactM,
  seas_pacf_interactM, seasonality_interactM,
  hwalpha_interactM, hwbeta_interactM,
  hwgamma_interactM, sediff_acf1_interactM,
  sediff_seacf1_interactM, sediff_acf5_interactM,
  N_interactM,
  y_acf5_interactM, diff1y_acf5_interactM,
  diff2y_acf5_interactM, alpha_interactM,
  beta_interactM))

names(friedmanHstat_monthly) <- c("feature", "class", "interaction")
head(friedmanHstat_monthly)

# devide the feature column to feature 1 and feature 2
friedmanHstat_monthly <- friedmanHstat_monthly %>% separate(
  feature, into=c("feature1", "feature2"), ":") %>% select(c("feature1", "feature2", "class", "interaction"))
table(friedmanHstat_monthly$feature1)
table(friedmanHstat_monthly$feature2)
# replace all NA and Inf swith zero
is.na(friedmanHstat_monthly) <- sapply(friedmanHstat_monthly, is.infinite)
friedmanHstat_monthly[is.na(friedmanHstat_monthly)] <- 0
summary(friedmanHstat_monthly$interaction)
boxplot(friedmanHstat_monthly$interaction)
# replace all values greater than 1 to 1
friedmanHstat_monthly$interaction[friedmanHstat_monthly$interaction > 1.0] <- 1
head(friedmanHstat_monthly)
friedmanHstat_monthly$class <- as.character(friedmanHstat_monthly$class)
save(friedmanHstat_monthly, file="data/friedmanHstat_monthly.rda")


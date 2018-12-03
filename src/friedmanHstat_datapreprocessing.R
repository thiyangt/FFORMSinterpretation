## packages
library(iml) # Friedman's H statistic calculation package
library(tidyverse)

## Yearly data-------------------
## load all Friedman's H statistic calculation files
load("data/FriedmanHstat/entropy_interact.rda")
load("data/FriedmanHstat/lumpiness_interact.rda")
load("data/FriedmanHstat/stability_interact.rda")
load("data/FriedmanHstat/hurst_interact.rda")
load("data/FriedmanHstat/trend_interact.rda")
load("data/FriedmanHstat/spikiness_interact.rda")
load("data/FriedmanHstat/linearity_interact.rda")
load("data/FriedmanHstat/curvature_interact.rda")
load("data/FriedmanHstat/e_acf1_interact.rda")
load("data/FriedmanHstat/y_acf1_interact.rda")
load("data/FriedmanHstat/diff1y_acf1_interact.rda")
load("data/FriedmanHstat/diff2y_acf1_interact.rda")
load("data/FriedmanHstat/y_pacf5_interact.rda")
load("data/FriedmanHstat/diff1y_pacf5_interact.rda")
load("data/FriedmanHstat/diff2y_pacf5_interact.rda")
load("data/FriedmanHstat/nonlinearity_interact.rda")
load("data/FriedmanHstat/lmres_acf1_interact.rda")
load("data/FriedmanHstat/ur_pp_interact.rda")
load("data/FriedmanHstat/ur_kpss_interact.rda")
load("data/FriedmanHstat/N_interact.rda")
load("data/FriedmanHstat/y_acf5_interact.rda")
load("data/FriedmanHstat/diff1y_acf5_interact.rda")
load("data/FriedmanHstat/diff2y_acf5_interact.rda")
load("data/FriedmanHstat/alpha_interact.rda")
load("data/FriedmanHstat/beta_interact.rda")

## merge all Friedman's H statistic calculations to one data frame
friedmanHstat_yearly <- do.call("rbind", list(
  entropy_interact, lumpiness_interact,
  stability_interact, hurst_interact,
  trend_interact, spikiness_interact,
  linearity_interact, curvature_interact,
  e_acf1_interact, y_acf1_interact,
  diff1y_acf1_interact, diff2y_acf1_interact,
  y_pacf5_interact, diff1y_pacf5_interact,
  diff2y_pacf5_interact, nonlinearity_interact,
  lmres_acf1_interact, ur_pp_interact,
  ur_kpss_interact, N_interact,
  y_acf5_interact, diff1y_acf5_interact,
  diff2y_acf5_interact, alpha_interact,
  beta_interact))

names(friedmanHstat_yearly) <- c("feature", "class", "interaction")
head(friedmanHstat_yearly)

# devide the feature column to feature 1 and feature 2
friedmanHstat_yearly <- friedmanHstat_yearly %>% separate(
  feature, into=c("feature1", "feature2", sep="(:)")
) %>% select(c("feature1", "feature2", "class", "interaction"))
# replace all NA and Inf swith zero
is.na(friedmanHstat_yearly) <- sapply(friedmanHstat_yearly, is.infinite)
friedmanHstat_yearly[is.na(friedmanHstat_yearly)] <- 0
summary(friedmanHstat_yearly$interaction)
boxplot(friedmanHstat_yearly$interaction)
# replace all values greater than 1 to 1
friedmanHstat_yearly$interaction[friedmanHstat_yearly$interaction > 1.0] <- 1

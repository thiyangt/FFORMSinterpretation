## packages
library(iml) # Friedman's H statistic calculation package
library(tidyverse)

## Yearly data-------------------
## load all Friedman's H statistic calculation files
load("data/FriedmanHstat/weekly/alpha_interactW.rda")
load("data/FriedmanHstat/weekly/beta_interactW.rda")
load("data/FriedmanHstat/weekly/curvature_interactW.rda")
load("data/FriedmanHstat/weekly/diff1y_acf1_interactW.rda")
load("data/FriedmanHstat/weekly/diff1y_acf5_interactW.rda")
load("data/FriedmanHstat/weekly/diff1y_pacf5_interactW.rda")
load("data/FriedmanHstat/weekly/diff2y_acf1_interactW.rda")
load("data/FriedmanHstat/weekly/diff2y_acf5_interactW.rda")
load("data/FriedmanHstat/weekly/diff2y_pacf5_interactW.rda")
load("data/FriedmanHstat/weekly/e_acf1_interactW.rda")
load("data/FriedmanHstat/weekly/entropy_interactW.rda")
load("data/FriedmanHstat/weekly/hurst_interactW.rda")
load("data/FriedmanHstat/weekly/linearity_interactW.rda")
load("data/FriedmanHstat/weekly/lumpiness_interactW.rda")
load("data/FriedmanHstat/weekly/N_interactW.rda")
load("data/FriedmanHstat/weekly/nonlinearity_interactW.rda")
load("data/FriedmanHstat/weekly/seas_pacf_interactW.rda")
load("data/FriedmanHstat/weekly/seasonality_interactW.rda")
load("data/FriedmanHstat/weekly/sediff_acf1_interactW.rda")
load("data/FriedmanHstat/weekly/sediff_seacf1_interactW.rda")
load("data/FriedmanHstat/weekly/sediff_acf5_interactW.rda")
load("data/FriedmanHstat/weekly/spikiness_interactW.rda")
load("data/FriedmanHstat/weekly/stability_interactW.rda")
load("data/FriedmanHstat/weekly/trend_interactW.rda")
load("data/FriedmanHstat/weekly/y_acf1_interactW.rda")
load("data/FriedmanHstat/weekly/y_acf5_interactW.rda")
load("data/FriedmanHstat/weekly/y_pacf5_interactW.rda")

## merge all Friedman's H statistic calculations to one data frame
friedmanHstat_weekly <- do.call("rbind", list(
  entropy_interactW, lumpiness_interactW,
  stability_interactW, hurst_interactW,
  trend_interactW, spikiness_interactW,
  linearity_interactW, curvature_interactW,
  e_acf1_interactW, y_acf1_interactW,
  diff1y_acf1_interactW, diff2y_acf1_interactW,
  y_pacf5_interactW, diff1y_pacf5_interactW,
  diff2y_pacf5_interactW, nonlinearity_interactW,
  seas_pacf_interactW, seasonality_interactW,
  sediff_acf1_interactW,
  sediff_seacf1_interactW, sediff_acf5_interactW,
  N_interactW,
  y_acf5_interactW, diff1y_acf5_interactW,
  diff2y_acf5_interactW, alpha_interactW,
  beta_interactW))

names(friedmanHstat_weekly) <- c("feature", "class", "interaction")
head(friedmanHstat_weekly)

# devide the feature column to feature 1 and feature 2
friedmanHstat_weekly <- friedmanHstat_weekly %>% separate(
  feature, into=c("feature1", "feature2"), ":") %>% select(c("feature1", "feature2", "class", "interaction"))
table(friedmanHstat_weekly$feature1)
table(friedmanHstat_weekly$feature2)
# replace all NA and Inf swith zero
is.na(friedmanHstat_weekly) <- sapply(friedmanHstat_weekly, is.infinite)
friedmanHstat_weekly[is.na(friedmanHstat_weekly)] <- 0
summary(friedmanHstat_weekly$interaction)
boxplot(friedmanHstat_weekly$interaction)
# replace all values greater than 1 to 1
friedmanHstat_weekly$interaction[friedmanHstat_weekly$interaction > 1.0] <- 1
head(friedmanHstat_weekly)
friedmanHstat_weekly$class <- as.character(friedmanHstat_weekly$class)
save(friedmanHstat_weekly, file="data/friedmanHstat_weekly.rda")


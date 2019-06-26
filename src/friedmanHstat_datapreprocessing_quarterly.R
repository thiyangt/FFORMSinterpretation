## packages
library(iml) # Friedman's H statistic calculation package
library(tidyverse)

## Yearly data-------------------
## load all Friedman's H statistic calculation files
load("data/FriedmanHstat/entropy_interactQ.rda")
load("data/FriedmanHstat/lumpiness_interactQ.rda")
load("data/FriedmanHstat/stability_interactQ.rda")
load("data/FriedmanHstat/hurst_interactQ.rda")
load("data/FriedmanHstat/trend_interactQ.rda")
load("data/FriedmanHstat/spikiness_interactQ.rda")
load("data/FriedmanHstat/linearity_interactQ.rda")
load("data/FriedmanHstat/curvature_interactQ.rda")
load("data/FriedmanHstat/e_acf1_interactQ.rda")
load("data/FriedmanHstat/y_acf1_interactQ.rda")
load("data/FriedmanHstat/diff1y_acf1_interactQ.rda")
load("data/FriedmanHstat/diff2y_acf1_interactQ.rda")
load("data/FriedmanHstat/y_pacf5_interactQ.rda")
load("data/FriedmanHstat/diff1y_pacf5_interactQ.rda")
load("data/FriedmanHstat/diff2y_pacf5_interactQ.rda")
load("data/FriedmanHstat/nonlinearity_interactQ.rda")
load("data/FriedmanHstat/seas_pacf_interactQ.rda")
load("data/FriedmanHstat/seasonality_interactQ.rda")
load("data/FriedmanHstat/hwalpha_interactQ.rda")
load("data/FriedmanHstat/hwbeta_interactQ.rda")
load("data/FriedmanHstat/hwgamma_interactQ.rda")
load("data/FriedmanHstat/sediff_acf1_interactQ.rda")
load("data/FriedmanHstat/sediff_seacf1_interactQ.rda")
load("data/FriedmanHstat/sediff_acf5_interactQ.rda")
load("data/FriedmanHstat/N_interactQ.rda")
load("data/FriedmanHstat/y_acf5_interactQ.rda")
load("data/FriedmanHstat/diff1y_acf5_interactQ.rda")
load("data/FriedmanHstat/diff2y_acf5_interactQ.rda")
load("data/FriedmanHstat/alpha_interactQ.rda")
load("data/FriedmanHstat/beta_interactQ.rda")

## merge all Friedman's H statistic calculations to one data frame
friedmanHstat_quarterly <- do.call("rbind", list(
  entropy_interactQ, lumpiness_interactQ,
  stability_interactQ, hurst_interactQ,
  trend_interactQ, spikiness_interactQ,
  linearity_interactQ, curvature_interactQ,
  e_acf1_interactQ, y_acf1_interactQ,
  diff1y_acf1_interactQ, diff2y_acf1_interactQ,
  y_pacf5_interactQ, diff1y_pacf5_interactQ,
  diff2y_pacf5_interactQ, nonlinearity_interactQ,
  seas_pacf_interactQ, seasonality_interactQ,
  hwalpha_interactQ, hwbeta_interactQ,
  hwgamma_interactQ, sediff_acf1_interactQ,
  sediff_seacf1_interactQ, sediff_acf5_interactQ,
  N_interactQ,
  y_acf5_interactQ, diff1y_acf5_interactQ,
  diff2y_acf5_interactQ, alpha_interactQ,
  beta_interactQ))

names(friedmanHstat_quarterly) <- c("feature", "class", "interaction")
head(friedmanHstat_quarterly)

# devide the feature column to feature 1 and feature 2
friedmanHstat_quarterly <- friedmanHstat_quarterly %>% separate(
  feature, into=c("feature1", "feature2"), ":") %>% select(c("feature1", "feature2", "class", "interaction"))
table(friedmanHstat_quarterly$feature1)
table(friedmanHstat_quarterly$feature2)
# replace all NA and Inf swith zero
is.na(friedmanHstat_quarterly) <- sapply(friedmanHstat_quarterly, is.infinite)
friedmanHstat_quarterly[is.na(friedmanHstat_quarterly)] <- 0
summary(friedmanHstat_quarterly$interaction)
boxplot(friedmanHstat_quarterly$interaction)
# replace all values greater than 1 to 1
friedmanHstat_quarterly$interaction[friedmanHstat_quarterly$interaction > 1.0] <- 1
head(friedmanHstat_quarterly)
friedmanHstat_quarterly$class <- as.character(friedmanHstat_quarterly$class)
save(friedmanHstat_quarterly, file="data/friedmanHstat_quarterly.rda")


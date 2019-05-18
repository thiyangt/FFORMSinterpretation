## packages
library(iml) # Friedman's H statistic calculation package
library(tidyverse)

## Yearly data-------------------
## load all Friedman's H statistic calculation files
load("data/FriedmanHstat_cal2/entropy_interact.rda")
load("data/FriedmanHstat_cal2/lumpiness_interact.rda")
load("data/FriedmanHstat_cal2/stability_interact.rda")
load("data/FriedmanHstat_cal2/hurst_interact.rda")
load("data/FriedmanHstat_cal2/trend_interact.rda")
load("data/FriedmanHstat_cal2/spikiness_interact.rda")
load("data/FriedmanHstat_cal2/linearity_interact.rda")
load("data/FriedmanHstat_cal2/curvature_interact.rda")
load("data/FriedmanHstat_cal2/e_acf1_interact.rda")
load("data/FriedmanHstat_cal2/y_acf1_interact.rda")
load("data/FriedmanHstat_cal2/diff1y_acf1_interact.rda")
load("data/FriedmanHstat_cal2/diff2y_acf1_interact.rda")
load("data/FriedmanHstat_cal2/y_pacf5_interact.rda")
load("data/FriedmanHstat_cal2/diff1y_pacf5_interact.rda")
load("data/FriedmanHstat_cal2/diff2y_pacf5_interact.rda")
load("data/FriedmanHstat_cal2/nonlinearity_interact.rda")
load("data/FriedmanHstat_cal2/lmres_acf1_interact.rda")
load("data/FriedmanHstat_cal2/ur_pp_interact.rda")
load("data/FriedmanHstat_cal2/ur_kpss_interact.rda")
load("data/FriedmanHstat_cal2/N_interact.rda")
load("data/FriedmanHstat_cal2/y_acf5_interact.rda")
load("data/FriedmanHstat_cal2/diff1y_acf5_interact.rda")
load("data/FriedmanHstat_cal2/diff2y_acf5_interact.rda")
load("data/FriedmanHstat_cal2/alpha_interact.rda")
load("data/FriedmanHstat_cal2/beta_interact.rda")

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
  feature, into=c("feature1", "feature2"), ":") %>% select(c("feature1", "feature2", "class", "interaction"))
table(friedmanHstat_yearly$feature1)
table(friedmanHstat_yearly$feature2)
head(friedmanHstat_yearly)
friedmanHstat_yearly[316,]
# replace all NA and Inf swith zero
is.na(friedmanHstat_yearly) <- sapply(friedmanHstat_yearly, is.infinite)
friedmanHstat_yearly[is.na(friedmanHstat_yearly)] <- 0
summary(friedmanHstat_yearly$interaction)
boxplot(friedmanHstat_yearly$interaction)
# replace all values greater than 1 to 1
friedmanHstat_yearly$interaction[friedmanHstat_yearly$interaction > 1.0] <- 1
head(friedmanHstat_yearly)
friedmanHstat_yearly$class <- as.character(friedmanHstat_yearly$class)
save(friedmanHstat_yearly, file="data/friedmanHstat_yearly.rda")
#ARIMA_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="ARIMA",]
#dim(ARIMA_YFH)
#head(ARIMA_YFH)

ARMA_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="ARMA",]

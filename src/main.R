## ---- loadPackages
library(Mcomp)
library(patchwork)
library(tidyverse)
library(reshape2)
library(grid)
library(gridExtra)
library(ggrepel)
library(png)
library(tsfeatures)
library(ggpubr)
library(RColorBrewer)
library(iml) # machine learning interpretability package
library(ggcorrplot) # to draw  ggcorrplot
library(lime)
library(seer)
library(Hmisc) # to draw confidence intervals

## ---- functionfriedman
source("src/friedmanHstatmatrix.R")



## ---- friedmanQ


## ---- quarterlypca
pcaQvariables <- quarterly_training[, 1:30]
pcaM4Q <- prcomp(pcaQvariables, center = TRUE, scale = TRUE)
# summary(pcaM1Y)
PC1m4q <- pcaM4Q$x[, 1]
PC2m4q <- pcaM4Q$x[, 2]
PC3m4q <- pcaM4Q$x[, 3]
m4qPCAresults <- data.frame(PC1 = PC1m4q, PC2 = PC2m4q, PC3 = PC3m4q, pcaQvariables)
m4qPCAresults$predicted <- trainQ_predictions_oob
pca1M4Q_snaive <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "snaive", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "snaive") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_rwd <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "rwd", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "rwd") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_rw <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "rw", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "rw") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))


pca1M4Q_notrend <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "ETS-notrendnoseasonal", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-notrendnoseasonal") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_etsdamtrend <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "ETS-dampedtrend", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-dampedtrend") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_etstrend <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "ETS-trend", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-trend") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_etsdtrends <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "ETS-dampedtrendseasonal", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-DTS") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_trends <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "ETS-trendseasonal", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-trendseasonal") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_s <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "ETS-seasonal", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-seasonal") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_sarima <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "SARIMA", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "SARIMA") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_ARIMA <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "ARIMA", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ARIMA") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_ARMA <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "ARMA/AR/MA", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ARMA/AR/MA") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_stlar <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "stlar", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "stlar") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_tbats <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "tbats", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "tbats") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_wn <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "wn", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "wn") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_theta <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "theta", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "theta") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_nn <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "nn", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "nn") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))


pca1M4Q_wn+pca1M4Q_ARMA+pca1M4Q_ARIMA+pca1M4Q_rwd+pca1M4Q_rw+pca1M4Q_notrend+pca1M4Q_etsdamtrend+
  pca1M4Q_etstrend+pca1M4Q_etsdtrends+pca1M4Q_trends+pca1M4Q_s+pca1M4Q_sarima+pca1M4Q_snaive+
  pca1M4Q_stlar+pca1M4Q_tbats+
  pca1M4Q_theta+pca1M4Q_nn+plot_layout(ncol = 5, nrow = 4)

################################################################################
#                      Monthly  data                                           #
################################################################################


## ---- friedmanQM
load("data/friedmanHstat_quarterly.rda")
col.order <- c("seasonality", "lumpiness","diff1y_pacf5", "trend",
               "linearity", "spikiness", "alpha", "diff1y_acf5",
               "y_pacf5","curvature", "seas_pacf", "diff2y_pacf5", "diff2y_acf5",
               "stability", "beta", "hwgamma", "nonlinearity", "hwbeta", "sediff_seacf1",
               "diff1y_acf1",
               "entropy", "hwalpha", "N", "sediff_acf1", "hurst", "y_acf1",
               "y_acf5", "sediff_acf5", "e_acf1", "diff2y_acf1")

## snaive
snaive_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="snaive",]
snaive_QFH_cormat <- friedmanHstat_matrix(snaive_QFH, 30, rev(col.order))
## random walk with drift
rwd_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="rwd",]
rwd_QFH_cormat <- friedmanHstat_matrix(rwd_QFH, 30, rev(col.order))
## random walk
rw_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="rw",]
rw_QFH_cormat <- friedmanHstat_matrix(rw_QFH, 30, rev(col.order))
## ETSNTNS
etsntns_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="ETS.notrendnoseasonal",]
etsntns_QFH_cormat <- friedmanHstat_matrix(etsntns_QFH, 30, rev(col.order))
## ETS-damped trend
etsdt_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="ETS.dampedtrend",]
etsdt_QFH_cormat <- friedmanHstat_matrix(etsdt_QFH, 30, rev(col.order))
## ETS-trend
etst_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="ETS.trend",]
etst_QFH_cormat <- friedmanHstat_matrix(etst_QFH, 30, rev(col.order))
## ETS-dampedtrendseasonal
etsdts_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="ETS.dampedtrendseasonal",]
etsdts_QFH_cormat <- friedmanHstat_matrix(etsdts_QFH , 30, rev(col.order))
## ETS-trendseasonal
etsts_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="ETS.trendseasonal",]
etsts_QFH_cormat <- friedmanHstat_matrix(etsts_QFH , 30, rev(col.order))
## ETS-seasonal
etss_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="ETS.seasonal",]
etss_QFH_cormat <- friedmanHstat_matrix(etss_QFH , 30, rev(col.order))
## SARIMA
sarima_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="SARIMA",]
sarima_QFH_cormat <- friedmanHstat_matrix(sarima_QFH , 30, rev(col.order))
## ARIMA
arima_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="ARIMA",]
arima_QFH_cormat <- friedmanHstat_matrix(arima_QFH , 30, rev(col.order))
## ARMA
arma_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="ARMA.AR.MA",]
arma_QFH_cormat <- friedmanHstat_matrix(arma_QFH , 30, rev(col.order))
## stlar
stlar_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="stlar",]
stlar_QFH_cormat <- friedmanHstat_matrix(stlar_QFH , 30, rev(col.order))
## tbats
tbats_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="tbats",]
tbats_QFH_cormat <- friedmanHstat_matrix(tbats_QFH , 30, rev(col.order))
## wn
wn_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="wn",]
wn_QFH_cormat <- friedmanHstat_matrix(wn_QFH , 30, rev(col.order))
## theta
theta_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="theta",]
theta_QFH_cormat <- friedmanHstat_matrix(theta_QFH , 30, rev(col.order))
## nn
nn_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="nn",]
nn_QFH_cormat <- friedmanHstat_matrix(nn_QFH , 30, rev(col.order))
friedman.quarterly.mean <- (snaive_QFH_cormat + rwd_QFH_cormat + rw_QFH_cormat +
                              etsntns_QFH_cormat + etsdt_QFH_cormat + etst_QFH_cormat +
                              etsdts_QFH_cormat + etsts_QFH_cormat + etss_QFH_cormat +
                              sarima_QFH_cormat + arima_QFH_cormat + arma_QFH_cormat + 
                              stlar_QFH_cormat + tbats_QFH_cormat + wn_QFH_cormat + theta_QFH_cormat +
                              nn_QFH_cormat)/17

fried.mat.quarterly <- ggcorrplot(friedman.quarterly.mean, hc.order = FALSE, type = "upper",
                                  outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7",
                       name = "Friedman's H-statistic")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1),
        panel.margin=unit(c(0,0,0,0), "null"),
        plot.margin=unit(c(0,0,-1,0), "null"))+ggtitle("A: Quarterly")

load("data/friedmanHstat_monthly.rda")
col.order <- c("seasonality", "linearity","lumpiness", "trend",
               "spikiness", "stability", "y_pacf5", "hwgamma", "hwalpha",
               "sediff_acf5", "sediff_seacf1", "seas_pacf",
               "curvature", "beta", "N", "nonlinearity", "e_acf1", "hurst", "entropy",
               "hwbeta", "sediff_acf1", "y_acf5", "diff1y_pacf5", "alpha",
               "y_acf1", "diff1y_acf5", "diff1y_acf1", "diff2y_acf5",
               "diff2y_acf1", "diff2y_pacf5")

## snaive
snaive_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="snaive",]
snaive_MFH_cormat <- friedmanHstat_matrix(snaive_MFH, 30, rev(col.order))
## random walk with drift
rwd_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="rwd",]
rwd_MFH_cormat <- friedmanHstat_matrix(rwd_MFH, 30, rev(col.order))
## random walk
rw_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="rw",]
rw_MFH_cormat <- friedmanHstat_matrix(rw_MFH, 30, rev(col.order))
## ETSNTNS
etsntns_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="ETS.notrendnoseasonal",]
etsntns_MFH_cormat <- friedmanHstat_matrix(etsntns_QFH, 30, rev(col.order))
## ETS-damped trend
etsdt_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="ETS.dampedtrend",]
etsdt_MFH_cormat <- friedmanHstat_matrix(etsdt_MFH, 30, rev(col.order))
## ETS-trend
etst_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="ETS.trend",]
etst_MFH_cormat <- friedmanHstat_matrix(etst_MFH, 30, rev(col.order))
## ETS-dampedtrendseasonal
etsdts_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="ETS.dampedtrendseasonal",]
etsdts_MFH_cormat <- friedmanHstat_matrix(etsdts_MFH , 30, rev(col.order))
## ETS-trendseasonal
etsts_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="ETS.trendseasonal",]
etsts_MFH_cormat <- friedmanHstat_matrix(etsts_MFH , 30, rev(col.order))
## ETS-seasonal
etss_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="ETS.seasonal",]
etss_MFH_cormat <- friedmanHstat_matrix(etss_MFH , 30, rev(col.order))
## SARIMA
sarima_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="SARIMA",]
sarima_MFH_cormat <- friedmanHstat_matrix(sarima_MFH , 30, rev(col.order))
## ARIMA
arima_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="ARIMA",]
arima_MFH_cormat <- friedmanHstat_matrix(arima_MFH , 30, rev(col.order))
## ARMA
arma_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="ARMA.AR.MA",]
arma_MFH_cormat <- friedmanHstat_matrix(arma_MFH , 30, rev(col.order))
## stlar
stlar_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="stlar",]
stlar_MFH_cormat <- friedmanHstat_matrix(stlar_MFH , 30, rev(col.order))
## tbats
tbats_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="tbats",]
tbats_MFH_cormat <- friedmanHstat_matrix(tbats_MFH , 30, rev(col.order))
## wn
wn_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="wn",]
wn_MFH_cormat <- friedmanHstat_matrix(wn_MFH , 30, rev(col.order))
## theta
theta_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="theta",]
theta_MFH_cormat <- friedmanHstat_matrix(theta_MFH , 30, rev(col.order))
## nn
nn_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="nn",]
nn_MFH_cormat <- friedmanHstat_matrix(nn_MFH , 30, rev(col.order))
friedman.monthly.mean <- (snaive_MFH_cormat + rwd_MFH_cormat + rw_MFH_cormat +
                   etsntns_MFH_cormat + etsdt_MFH_cormat + etst_MFH_cormat +
                   etsdts_MFH_cormat  + etsts_MFH_cormat + etss_MFH_cormat +
                   sarima_MFH_cormat + arima_MFH_cormat  + arma_MFH_cormat+
                   stlar_MFH_cormat + tbats_MFH_cormat + wn_MFH_cormat +
                   theta_MFH_cormat + nn_MFH_cormat)/17 

fried.mat.monthly <- ggcorrplot(friedman.monthly.mean, hc.order = FALSE, type = "upper",
                                  outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7",
                       name = "Friedman's H-statistic")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1),
        panel.margin=unit(c(0,0,0,0), "null"),
        plot.margin=unit(c(0,0,-1,0), "null"))+ggtitle("B: Monthly")

fried.mat.quarterly/fried.mat.monthly 

## ---- monthlypca
pcaMvariables <- monthly_training[, 1:30]
pcaM4M <- prcomp(pcaMvariables, center = TRUE, scale = TRUE)
PC1m4m <- pcaM4M$x[, 1]
PC2m4m <- pcaM4M$x[, 2]
PC3m4m <- pcaM4M$x[, 3]
#which.max(PC2m4m)249937
m4mPCAresults <- data.frame(PC1 = PC1m4m, PC2 = PC2m4m, PC3 = PC3m4m, pcaMvariables)
m4mPCAresults$predicted <- trainM_predictions_oob
m4mPCAresults <- m4mPCAresults[-249937, ]
pca1M4Msnaive <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "snaive", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "rwd") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Mrwd <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "rwd", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "rwd") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Mrw <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "rw", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "rw") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))


pca1M4Mnotrend <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ETS-notrendnoseasonal", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-notrendnoseasonal") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Metsdamtrend <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ETS-dampedtrend", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-dampedtrend") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Metstrend <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ETS-trend", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-trend") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Metsdtrends <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ETS-dampedtrendseasonal", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-DTS") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Mtrends <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ETS-trendseasonal", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-trendseasonal") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Ms <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ETS-seasonal", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-seasonal") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Msarima <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "SARIMA", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "SARIMA") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4MARIMA <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ARIMA", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ARIMA") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4MARMA <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ARMA/AR/MA", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ARMA/AR/MA") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Mstlar <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "stlar", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "stlar") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Mtbats <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "tbats", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "stlar") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Mwn <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "wn", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "wn") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Mtheta <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "theta", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "theta") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Mnn <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "nn", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "nn") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))


pca1M4Msnaive+pca1M4Mrwd+pca1M4Mrw+pca1M4Mnotrend+pca1M4Metsdamtrend+
  pca1M4Metstrend+pca1M4Metsdtrends+pca1M4Mtrends+pca1M4Ms+pca1M4Msarima+
  pca1M4MARIMA+pca1M4MARMA+pca1M4Mstlar+pca1M4Mtbats+pca1M4Mwn+
  pca1M4Mtheta+pca1M4Mnn+plot_layout(ncol = 5, nrow = 4)


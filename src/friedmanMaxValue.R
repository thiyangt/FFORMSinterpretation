friedmanMaxValue <- function(matrix_name, top_num){
  matrix_name <- as.data.frame(matrix_name) 
  matrix_name[upper.tri(matrix_name,diag=TRUE)] <- 0 
  matrix_name <- as.matrix(matrix_name)
  idx <- which( 
    matrix(matrix_name %in% head(sort(matrix_name, TRUE), top_num), 
           nr = nrow(matrix_name)), arr.ind = TRUE)
  rw_name <- row.names(matrix_name)
  col_name <- colnames(matrix_name)
  df <- data.frame(row=rw_name[idx[,1]], col=col_name[idx[,2]])
  df
}



# test
#m <- matrix(16:1, 4, 4) 
#colnames(m) <- c("A", "B", "C", "D")
#row.names(m) <- c("a", "b", "c", "d")
#friedmanMaxValue(m,3)

# yearly----
load("data/friedmanHstat_yearly.rda")
col.order <- c("trend", "ur_pp","spikiness", "beta",
               "diff1y_acf1", "linearity", "diff1y_acf5", "curvature",
               "lmres_acf1","y_pacf5", "ur_kpss", "y_acf1", "nonlinearity",
               "alpha", "diff1y_pacf5", "hurst", "entropy", "e_acf1", "y_acf5",
               "diff2y_pacf5",
               "diff2y_acf1", "N", "diff2y_acf5", "lumpiness", "stability")
## random walk with drift
rwd_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="rwd",]
rwd_YFH_cormat <- friedmanHstat_matrix(rwd_YFH, 25, rev(col.order))
## random walk
rw_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="rw",]
rw_YFH_cormat <- friedmanHstat_matrix(rw_YFH, 25, rev(col.order))
## ETS-trend
etst_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="ETS.trend",]
etst_YFH_cormat <- friedmanHstat_matrix(etst_YFH, 25, rev(col.order))
## ETS-dampedtrend
etsdt_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="ETS.dampedtrend",]
etsdt_YFH_cormat <- friedmanHstat_matrix(etsdt_YFH, 25, rev(col.order))
## ETS-notrendnoseasonal
etsntns_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="ETS.notrendnoseasonal",]
etsntns_YFH_cormat <- friedmanHstat_matrix(etsntns_YFH, 25, rev(col.order))
## ARIMA
arima_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="ARIMA",]
arima_YFH_cormat <- friedmanHstat_matrix(arima_YFH, 25, rev(col.order))
##  ARMA.AR.MA 
arma_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="ARMA.AR.MA",]
arma_YFH_cormat <- friedmanHstat_matrix(arma_YFH, 25, rev(col.order))
##  wn 
wn_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="wn",]
wn_YFH_cormat <- friedmanHstat_matrix(wn_YFH, 25, rev(col.order))
## theta
theta_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="theta",]
theta_YFH_cormat <- friedmanHstat_matrix(theta_YFH, 25, rev(col.order))
## nn
nn_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="nn",]
nn_YFH_cormat <- friedmanHstat_matrix(nn_YFH, 25, rev(col.order))

yearly_mean <- (rwd_YFH_cormat + rw_YFH_cormat + etst_YFH_cormat + 
  etsdt_YFH_cormat + etsntns_YFH_cormat + arima_YFH_cormat +
  arma_YFH_cormat + wn_YFH_cormat + theta_YFH_cormat + nn_YFH_cormat)/10
## maximum value of top 3 combinations
friedmanMaxValue(yearly_mean,1)
## results_yearly-result 1
#      row         col
#1 lumpiness   stability
#2    e_acf1 diff2y_acf1
#3     hurst      y_acf5
#4     hurst      e_acf1

## results_yearly-result 2
#     row         col
# 1 lumpiness   stability
# 2    e_acf1   stability
# 3     hurst           N
# 4    e_acf1 diff2y_acf1
# 5     hurst      y_acf5
# 6     hurst      e_acf1

# quarterly----
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
## top 3 scores
quarterly_mean <- (snaive_QFH_cormat + rwd_QFH_cormat + rw_QFH_cormat +
                     etsntns_QFH_cormat + etsdt_QFH_cormat + etst_QFH_cormat +
                     etsdts_QFH_cormat + etsts_QFH_cormat + etss_QFH_cormat +
                     sarima_QFH_cormat + arima_QFH_cormat + arma_QFH_cormat + 
                     stlar_QFH_cormat + tbats_QFH_cormat + wn_QFH_cormat + theta_QFH_cormat +
                     nn_QFH_cormat)/17
friedmanMaxValue(quarterly_mean,3)
#        row       col
#1 diff1y_acf5         N
#2 diff1y_acf5      beta
#3 diff1y_acf5 stability

# monthly----
load("data/friedmanHstat_monthly.rda")
col.order <- c("seasonality", "linearity","lumpiness", "trend",
               "spikiness", "stability", "y_pacf5", "hwgamma", "hwalpha",
               "sediff_acf5", "sediff_seacf1", "seas_pacf",
               "curvature", "beta", "N", "nonlinearity", "e_acf1", "hurst", "entropy",
               "hwbeta", "sediff_acf1", "y_acf5", "diff1y_pacf5", "alpha",
               "y_acf1", "diff1y_acf5", "diff1y_acf1", "diff2y_acf5",
               "diff2y_acf1", "diff2y_pacf5"
)

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
## top 3 among mean values
monthly_mean <- (snaive_MFH_cormat + rwd_MFH_cormat + rw_MFH_cormat +
  etsntns_MFH_cormat + etsdt_MFH_cormat + etst_MFH_cormat +
  etsdts_MFH_cormat  + etsts_MFH_cormat + etss_MFH_cormat +
  sarima_MFH_cormat + arima_MFH_cormat  + arma_MFH_cormat+
  stlar_MFH_cormat + tbats_MFH_cormat + wn_MFH_cormat +
  theta_MFH_cormat + nn_MFH_cormat)/17 
friedmanMaxValue(monthly_mean,3)
#    row       col
#1 spikiness     hurst
#2 spikiness   hwalpha
#3 lumpiness spikiness


# weekly----
load("data/friedmanHstat_weekly.rda")
col.order <- c("spikiness", "linearity", "trend", "seasonality", "stability",
               "lumpiness", "curvature", "sediff_acf5", "entropy", "beta","y_pacf5",
               "seas_pacf", "N", "diff2y_acf5", "nonlinearity", "sediff_seacf1",
               "y_acf5", "diff1y_pacf5", "y_acf1", "diff1y_acf5", "diff2y_pacf5", 
               "e_acf1", "sediff_acf1", "diff2y_acf1", "hurst", "alpha", "diff1y_acf1")

## snaive
snaive_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="snaive",]
snaive_WFH_cormat <- friedmanHstat_matrix(snaive_WFH, 27, rev(col.order))
## rwd
rwd_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="rwd",]
rwd_WFH_cormat <- friedmanHstat_matrix(rwd_WFH, 27, rev(col.order))
## rw
rw_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="rw",]
rw_WFH_cormat <- friedmanHstat_matrix(rw_WFH, 27, rev(col.order))
## ARIMA
arima_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="ARIMA",]
arima_WFH_cormat <- friedmanHstat_matrix(arima_WFH, 27, rev(col.order))
## SARIMA
sarima_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="SARIMA",]
sarima_WFH_cormat <- friedmanHstat_matrix(sarima_WFH, 27, rev(col.order))
## stlar
stlar_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="stlar",]
stlar_WFH_cormat <- friedmanHstat_matrix(stlar_WFH, 27, rev(col.order))
## mstlets
mstlets_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="mstlets",]
mstlets_WFH_cormat <- friedmanHstat_matrix(mstlets_WFH, 27, rev(col.order))
## tbats
tbats_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="tbats",]
tbats_WFH_cormat <- friedmanHstat_matrix(tbats_WFH, 27, rev(col.order))
## ARMA
arma_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="ARMA.AR.MA",]
arma_WFH_cormat <- friedmanHstat_matrix(arma_WFH, 27, rev(col.order))
## wn
wn_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="wn",]
wn_WFH_cormat <- friedmanHstat_matrix(wn_WFH, 27, rev(col.order))
## theta
theta_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="theta",]
theta_WFH_cormat <- friedmanHstat_matrix(theta_WFH, 27, rev(col.order))
## nn
nn_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="nn",]
nn_WFH_cormat <- friedmanHstat_matrix(nn_WFH, 27, rev(col.order))
## top 3 values of Friedman H stat
weekly_mean <- (snaive_WFH_cormat + rwd_WFH_cormat + rw_WFH_cormat +
  arima_WFH_cormat + sarima_WFH_cormat + stlar_WFH_cormat+
  mstlets_WFH_cormat + tbats_WFH_cormat + arma_WFH_cormat +
  wn_WFH_cormat + theta_WFH_cormat + nn_WFH_cormat)/12
friedmanMaxValue(weekly_mean,3)
#      row          col
#1 lumpiness nonlinearity
#2 lumpiness         beta
#3     trend      entropy
#4 stability    lumpiness
#5     trend    lumpiness

# daily----
load("data/friedmanHstat_daily.rda")
col.order <- c("seasonal_strength1", "stability", "trend", "lumpiness",
               "linearity", "nonlinearity", "y_pacf5", "curvature", "e_acf1",
               "spikiness", "sediff_seacf1","seas_pacf", "N", "sediff_acf5",
               "entropy", "y_acf5", "y_acf1", "diff1y_acf5", "sediff_acf1",
               "diff1y_pacf5", "diff1y_acf1", "hurst", "seasonal_strength2",
               "diff2y_acf1", "diff2y_acf5", "diff2y_pacf5")
## snaive
snaive_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="snaive",]
snaive_DFH_cormat <- friedmanHstat_matrix(snaive_DFH, 26, rev(col.order))
## rw
rw_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="rw",]
rw_DFH_cormat <- friedmanHstat_matrix(rw_DFH, 26, rev(col.order))
## rwd
rwd_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="rwd",]
rwd_DFH_cormat <- friedmanHstat_matrix(rwd_DFH, 26, rev(col.order))
#mstlarima
mstlarima_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="mstlarima",]
mstlarima_DFH_cormat <- friedmanHstat_matrix(mstlarima_DFH, 26, rev(col.order))
#mstlets
mstlets_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="mstlets",]
mstlets_DFH_cormat <- friedmanHstat_matrix(mstlets_DFH, 26, rev(col.order))
# tbats
tbats_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="tbats",]
tbats_DFH_cormat <- friedmanHstat_matrix(tbats_DFH, 26, rev(col.order))
#stlar
stlar_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="stlar",]
stlar_DFH_cormat <- friedmanHstat_matrix(stlar_DFH, 26, rev(col.order))
#theta
theta_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="theta",]
theta_DFH_cormat <- friedmanHstat_matrix(theta_DFH, 26, rev(col.order))
#nn
nn_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="nn",]
nn_DFH_cormat <- friedmanHstat_matrix(nn_DFH, 26, rev(col.order))
#wn
wn_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="wn",]
wn_DFH_cormat <- friedmanHstat_matrix(wn_DFH, 26, rev(col.order))
## top 3 values of mean 
daily_mean <- (snaive_DFH_cormat + rw_DFH_cormat + rwd_DFH_cormat +
  mstlarima_DFH_cormat + mstlets_DFH_cormat + tbats_DFH_cormat +
  stlar_DFH_cormat + theta_DFH_cormat + nn_DFH_cormat + wn_DFH_cormat)/10
friedmanMaxValue(daily_mean,4)
#              row         col
#1        sediff_acf5 sediff_acf1
#2          curvature   seas_pacf
#3 seasonal_strength1      e_acf1
#4 seasonal_strength1   curvature

# hourly----
load("data/friedmanHstat_hourly.rda")
col.order <- c("seasonal_strength1", "entropy", "linearity", "y_pacf5",
               "curvature", "spikiness", "trend", "stability", "lumpiness",
               "y_acf5", "diff2y_acf1","sediff_seacf1", "nonlinearity", "seas_pacf",
               "diff2y_pacf5", "seasonal_strength2", "hurst", "y_acf1", "diff1y_pacf5",
               "sediff_acf5", "diff1y_acf1", "diff2y_acf5", "diff1y_acf5",
               "e_acf1", "sediff_acf1", "N")
## snaive
snaive_HFH <- friedmanHstat_hourly[friedmanHstat_hourly$class=="snaive",]
snaive_HFH_cormat <- friedmanHstat_matrix(snaive_HFH, 26, rev(col.order))
## rw
rw_HFH <- friedmanHstat_hourly[friedmanHstat_hourly$class=="rw",]
rw_HFH_cormat <- friedmanHstat_matrix(rw_HFH, 26, rev(col.order))
## rwd
rwd_HFH <- friedmanHstat_hourly[friedmanHstat_hourly$class=="rwd",]
rwd_HFH_cormat <- friedmanHstat_matrix(rwd_HFH, 26, rev(col.order))
#mstlarima
mstlarima_HFH <- friedmanHstat_hourly[friedmanHstat_hourly$class=="mstlarima",]
mstlarima_HFH_cormat <- friedmanHstat_matrix(mstlarima_HFH, 26, rev(col.order))
#mstlets
mstlets_HFH <- friedmanHstat_hourly[friedmanHstat_hourly$class=="mstlets",]
mstlets_HFH_cormat <- friedmanHstat_matrix(mstlets_HFH, 26, rev(col.order))
# tbats
tbats_HFH <- friedmanHstat_hourly[friedmanHstat_hourly$class=="tbats",]
tbats_HFH_cormat <- friedmanHstat_matrix(tbats_HFH, 26, rev(col.order))
#stlar
stlar_HFH <- friedmanHstat_hourly[friedmanHstat_hourly$class=="stlar",]
stlar_HFH_cormat <- friedmanHstat_matrix(stlar_HFH, 26, rev(col.order))
#theta
theta_HFH <- friedmanHstat_hourly[friedmanHstat_hourly$class=="theta",]
theta_HFH_cormat <- friedmanHstat_matrix(theta_HFH, 26, rev(col.order))
#nn
nn_HFH <- friedmanHstat_hourly[friedmanHstat_hourly$class=="nn",]
nn_HFH_cormat <- friedmanHstat_matrix(nn_HFH, 26, rev(col.order))
#wn
wn_HFH <- friedmanHstat_hourly[friedmanHstat_hourly$class=="wn",]
wn_HFH_cormat <- friedmanHstat_matrix(wn_HFH, 26, rev(col.order))
## top 3 of means
hourly_mean <- (snaive_HFH_cormat+rw_HFH_cormat+rwd_HFH_cormat+mstlarima_HFH_cormat+
  mstlets_HFH_cormat + tbats_HFH_cormat + stlar_HFH_cormat + theta_HFH_cormat +
  nn_HFH_cormat+wn_HFH_cormat)/10
friedmanMaxValue(hourly_mean,3)
#      row           col
#1 linearity  diff2y_pacf5
#2 linearity  nonlinearity
#3 linearity sediff_seacf1
#4   entropy         trend




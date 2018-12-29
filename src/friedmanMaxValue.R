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

# yearly
## ---- friedmany
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

# quarterly

# monthly

# weekly

# daily

# quarterly

# hourly



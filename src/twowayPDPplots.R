## ---- ytwopdp
load("data/yearly/lumpiness.stability.y.rda")
load("data/yearly/eacf1.diff2yacf1.y.rda")
load("data/yearly/hurst.y_acf5.y.rda")
load("data/yearly/hurst.e_acf1.y.rda")
colNamesls <- colnames(lumpiness.stability.y)[27:36]
colNamesed <- colnames(eacf1.diff2yacf1.y)[27:36]
colNamesha <- colnames(hurst.y_acf5.y)[27:36]
colNameshe <- colnames(hurst.e_acf1.y)[27:36]

# # rwd
# int1 <- ggplot(
#   data = lumpiness.stability.y, aes_string(x = lumpiness.stability.y$lumpiness,y = lumpiness.stability.y$stability, z = colNamesls[8], fill = colNamesls[8]
#   ))+geom_tile() + 
#   scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100),option = "A", direction = -1)+
#   xlab("lumpiness") + ylab("stability") + theme(legend.position="none")+ggtitle("rwd")
# ## rw
# int2 <- ggplot(
#   data = lumpiness.stability.y, aes_string(x = lumpiness.stability.y$lumpiness,y = lumpiness.stability.y$stability, z = colNamesls[7], fill = colNamesls[7]
#   ))+geom_tile() + 
#   scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100),option = "A", direction = -1)+
#   xlab("lumpiness") + ylab("stability") + theme(legend.position="none")+ggtitle("rw")
# # ETS-trend
# int3 <- ggplot(
#   data = lumpiness.stability.y, aes_string(x = lumpiness.stability.y$lumpiness,y = lumpiness.stability.y$stability, z = colNamesls[5], fill = colNamesls[5]
#   ))+geom_tile() + 
#   scale_fill_viridis_c(limits = c(0, 0.05), breaks = seq(0, 0.05, 100),option = "A", direction = -1)+
#   xlab("lumpiness") + ylab("stability") + theme(legend.position="none")+ggtitle("ETS-trend")
# ## ETS_dampedtrend
# int4 <- ggplot(
#   data = lumpiness.stability.y, aes_string(x = lumpiness.stability.y$lumpiness,y = lumpiness.stability.y$stability, z = colNamesls[3], fill = colNamesls[3]
#   ))+geom_tile() + 
#   scale_fill_viridis_c(limits = c(0, 0.003), breaks = seq(0, 0.003, 100),option = "A", direction = -1)+
#   xlab("lumpiness") + ylab("stability") + theme(legend.position="none")+ggtitle("ETS-dampedtrend")
# # ETS-notrendseasonal
# int5 <- ggplot(
#   data = lumpiness.stability.y, aes_string(x = lumpiness.stability.y$lumpiness,y = lumpiness.stability.y$stability, z = colNamesls[4], fill = colNamesls[4]
#   ))+geom_tile() + 
#   scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100),option = "A", direction = -1)+
#   xlab("lumpiness") + ylab("stability") + theme(legend.position="none")+ggtitle("ETS-notrendnoseasonal")
# # ARIMA
# int6 <- ggplot(
#   data = lumpiness.stability.y, aes_string(x = lumpiness.stability.y$lumpiness,y = lumpiness.stability.y$stability, z = colNamesls[1], fill = colNamesls[1]
#   ))+geom_tile() + 
#   scale_fill_viridis_c(limits = c(0, 0.05), breaks = seq(0, 0.05, 100),option = "A", direction = -1)+
#   xlab("lumpiness") + ylab("stability") + theme(legend.position="none")+ggtitle("ARIMA")
# ## ARMA
# int7 <- ggplot(
#   data = lumpiness.stability.y, aes_string(x = lumpiness.stability.y$lumpiness,y = lumpiness.stability.y$stability, z = colNamesls[2], fill = colNamesls[2]
#   ))+geom_tile() + 
#   scale_fill_viridis_c(limits = c(0, 0.005), breaks = seq(0, 0.005, 100),option = "A", direction = -1)+
#   xlab("lumpiness") + ylab("stability") + theme(legend.position="none")+ggtitle("ARMA/AR/MA")
# # wn
# int8 <- ggplot(
#   data = lumpiness.stability.y, aes_string(x = lumpiness.stability.y$lumpiness,y = lumpiness.stability.y$stability, z = colNamesls[10], fill = colNamesls[10]
#   ))+geom_tile() + 
#   scale_fill_viridis_c(limits = c(0, 0.8), breaks = seq(0, 0.8, 100),option = "A", direction = -1)+
#   xlab("lumpiness") + ylab("stability") + theme(legend.position="none")+ggtitle("wn")
# ## theta
# int9 <- ggplot(
#   data = lumpiness.stability.y, aes_string(x = lumpiness.stability.y$lumpiness,y = lumpiness.stability.y$stability, z = colNamesls[9], fill = colNamesls[9]
#   ))+geom_tile() + 
#   scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100),option = "A", direction = -1)+
#   xlab("lumpiness") + ylab("stability") + theme(legend.position="none")+ggtitle("theta")
# # nn
# int10 <- ggplot(
#   data = lumpiness.stability.y, aes_string(x = lumpiness.stability.y$lumpiness,y = lumpiness.stability.y$stability, z = colNamesls[6], fill = colNamesls[6]
#   ))+geom_tile() + 
#   scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100),option = "A", direction = -1)+
#   xlab("lumpiness") + ylab("stability") +ggtitle("nn")

int1+int2+int3+int4+int5+int6+int7+int8+int9+int10+plot_layout(ncol = 5, nrow = 2)

## ---- qtwopdp
load("data/quarterly/diff1y_pacf5.lumpiness.q.rda")
load("data/quarterly/diff1y_acf5.N.q.rda")
load("data/quarterly/diff1y_acf5.beta.q.rda")
load("data/quarterly/diff1y_acf5.stability.q.rda")
colNamesdn <- colnames(diff1y_acf5.N.q)[32:48]
colNamesdb <- colnames(diff1y_acf5.beta.q)[32:48]
colNamesds <- colnames(diff1y_acf5.stability.q)[32:48]

# snaive
int1 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[13], fill = colNamesds[13]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("snaive")

# rwd(42)
int2 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[11], fill = colNamesds[11]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("rwd")


# rw(41)
int3 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[10], fill = colNamesds[10]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("rw")

# ETS-notrendnoseasonal(36)
int4 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[5], fill = colNamesds[5]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.008), breaks = seq(0, 0.008, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("ETS-notrendnoseasonal")

# ETS-dampedtrend(34)
int5 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[3], fill = colNamesds[3]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.01), breaks = seq(0, 0.01, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("ETS-dampedtrend")

# ETS-trend(38)
int6 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[7], fill = colNamesds[7]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.008), breaks = seq(0, 0.008, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("ETS-trend")

# ETS-dampedtrendseasonal(35)
int7 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[4], fill = colNamesds[4]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("ETS-dampedtrendseasonal")

# ETS-trendseasonal(39)
int8 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[8], fill = colNamesds[8]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("ETS-trendseasonal")

# ETS-seasonal(37)
int9 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[6], fill = colNamesds[6]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("ETS-seasonal")

# SARIMA(43)
int10 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[12], fill = colNamesds[12]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("SARIMA")

# ARIMA(32)
int11 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[1], fill = colNamesds[1]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("ARIMA")

# ARMA/AR/MA(33)
int12 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[2], fill = colNamesds[2]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.005), breaks = seq(0, 0.005, 1000), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("ARMA/AR/MA")

# stlar(45)
int13 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[14], fill = colNamesds[14]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("stlar")

# tbats(46)
int14 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[15], fill = colNamesds[15]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("tbats")

# wn(48)
int15 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[17], fill = colNamesds[17]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.8), breaks = seq(0, 0.8, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("wn")

# theta(47)
int16 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[16], fill = colNamesds[16]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("theta")

# nn(40)
int17 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[9], fill = colNamesds[9]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ggtitle("nn")

int1+int2+int3+int4+int5+int6+int7+int8+int9+int10+int11+int12+int13+int14+int15+int16+int17+plot_layout(ncol = 6, nrow = 3)

## ---- mtwopdp
load("data/monthly/sediff_acf5.sediff_seacf1.m.rda")
colNamesds <- colnames(sediff_acf5.sediff_seacf1.m )[32:48]
# snaive
int1 <- ggplot(
  data = sediff_acf5.sediff_seacf1.m,
  aes_string(
    x = sediff_acf5.sediff_seacf1.m$sediff_acf5,
    y = sediff_acf5.sediff_seacf1.m$sediff_seacf1, z = colNamesds[13], fill = colNamesds[13]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.05), breaks = seq(0, 0.05, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("snaive")
# rwd(42)
int2 <- ggplot(
  data = sediff_acf5.sediff_seacf1.m,
  aes_string(
    x = sediff_acf5.sediff_seacf1.m$sediff_acf5,
    y = sediff_acf5.sediff_seacf1.m$sediff_seacf1, z = colNamesds[11], fill = colNamesds[11]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("rwd")
# rw(41)
int3 <- ggplot(
  data = sediff_acf5.sediff_seacf1.m,
  aes_string(
    x = sediff_acf5.sediff_seacf1.m$sediff_acf5,
    y = sediff_acf5.sediff_seacf1.m$sediff_seacf1, z = colNamesds[10], fill = colNamesds[10]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("rw")
# ETS-notrendnoseasonal(36)
int4 <- ggplot(
  data = sediff_acf5.sediff_seacf1.m,
  aes_string(
    x = sediff_acf5.sediff_seacf1.m$sediff_acf5,
    y = sediff_acf5.sediff_seacf1.m$sediff_seacf1, z = colNamesds[5], fill = colNamesds[5]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("ETS-notrendnoseasonal")
# ETS-dampedtrend(34)
int5 <- ggplot(
  data = sediff_acf5.sediff_seacf1.m,
  aes_string(
    x = sediff_acf5.sediff_seacf1.m$sediff_acf5,
    y = sediff_acf5.sediff_seacf1.m$sediff_seacf1, z = colNamesds[3], fill = colNamesds[3]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.005), breaks = seq(0, 0.005, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("ETS-dampedtrend")
# ETS-trend(38)
int6 <- ggplot(
  data = sediff_acf5.sediff_seacf1.m,
  aes_string(
    x = sediff_acf5.sediff_seacf1.m$sediff_acf5,
    y = sediff_acf5.sediff_seacf1.m$sediff_seacf1, z = colNamesds[7], fill = colNamesds[7]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.03), breaks = seq(0, 0.03, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("ETS-trend")
# ETS-dampedtrendseasonal(35)
int7 <- ggplot(
  data = sediff_acf5.sediff_seacf1.m,
  aes_string(
    x = sediff_acf5.sediff_seacf1.m$sediff_acf5,
    y = sediff_acf5.sediff_seacf1.m$sediff_seacf1, z = colNamesds[4], fill = colNamesds[4]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.01), breaks = seq(0, 0.01, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("ETS-dampedtrendseasonal")
# ETS-trendseasonal(39)
int8 <- ggplot(
  data = sediff_acf5.sediff_seacf1.m,
  aes_string(
    x = sediff_acf5.sediff_seacf1.m$sediff_acf5,
    y = sediff_acf5.sediff_seacf1.m$sediff_seacf1, z = colNamesds[8], fill = colNamesds[8]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.01), breaks = seq(0, 0.01, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("ETS-trendseasonal")
# ETS-seasonal(37)
int9 <- ggplot(
  data = sediff_acf5.sediff_seacf1.m,
  aes_string(
    x = sediff_acf5.sediff_seacf1.m$sediff_acf5,
    y = sediff_acf5.sediff_seacf1.m$sediff_seacf1, z = colNamesds[6], fill = colNamesds[6]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.01), breaks = seq(0, 0.01, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("ETS-seasonal")
# SARIMA(43)
int10 <- ggplot(
  data = sediff_acf5.sediff_seacf1.m,
  aes_string(
    x = sediff_acf5.sediff_seacf1.m$sediff_acf5,
    y = sediff_acf5.sediff_seacf1.m$sediff_seacf1, z = colNamesds[12], fill = colNamesds[12]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("SARIMA")
# ARIMA(32)
int11 <- ggplot(
  data = sediff_acf5.sediff_seacf1.m,
  aes_string(
    x = sediff_acf5.sediff_seacf1.m$sediff_acf5,
    y = sediff_acf5.sediff_seacf1.m$sediff_seacf1, z = colNamesds[1], fill = colNamesds[1]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.01), breaks = seq(0, 0.01, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("ARIMA")
# ARMA/AR/MA(33)
int12 <- ggplot(
  data = sediff_acf5.sediff_seacf1.m,
  aes_string(
    x = sediff_acf5.sediff_seacf1.m$sediff_acf5,
    y = sediff_acf5.sediff_seacf1.m$sediff_seacf1, z = colNamesds[2], fill = colNamesds[2]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.005), breaks = seq(0, 0.005, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("ARMA/AR/MA")
# stlar(45)
int13 <- ggplot(
  data = sediff_acf5.sediff_seacf1.m,
  aes_string(
    x = sediff_acf5.sediff_seacf1.m$sediff_acf5,
    y = sediff_acf5.sediff_seacf1.m$sediff_seacf1, z = colNamesds[14], fill = colNamesds[14]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("stlar")
# tbats(46)
int14 <- ggplot(
  data = sediff_acf5.sediff_seacf1.m,
  aes_string(
    x = sediff_acf5.sediff_seacf1.m$sediff_acf5,
    y = sediff_acf5.sediff_seacf1.m$sediff_seacf1, z = colNamesds[15], fill = colNamesds[15]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("tbats")
# wn(48)
int15 <- ggplot(
  data = sediff_acf5.sediff_seacf1.m,
  aes_string(
    x = sediff_acf5.sediff_seacf1.m$sediff_acf5,
    y = sediff_acf5.sediff_seacf1.m$sediff_seacf1, z = colNamesds[17], fill = colNamesds[17]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.8), breaks = seq(0, 0.8, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("wn")
# theta(47)
int16 <- ggplot(
  data = sediff_acf5.sediff_seacf1.m,
  aes_string(
    x = sediff_acf5.sediff_seacf1.m$sediff_acf5,
    y = sediff_acf5.sediff_seacf1.m$sediff_seacf1, z = colNamesds[16], fill = colNamesds[16]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("theta")
# nn(40)
int17 <- ggplot(
  data = sediff_acf5.sediff_seacf1.m,
  aes_string(
    x = sediff_acf5.sediff_seacf1.m$sediff_acf5,
    y = sediff_acf5.sediff_seacf1.m$sediff_seacf1, z = colNamesds[9], fill = colNamesds[9]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("sediff_seacf1")+ggtitle("nn")

int1+int2+int3+int4+int5+int6+int7+int8+int9+int10+int11+int12+int13+int14+int15+int16+int17+plot_layout(ncol = 6, nrow = 3)

## ---- wtwopdp
load("data/weekly/trend.entropy.w.rda")
colNameste <- colnames(trend.entropy.w)[29:40]
#snaive
int1 <- ggplot(
  data = trend.entropy.w,
  aes_string(
    x = trend.entropy.w$trend,
    y = trend.entropy.w$entropy, z = colNameste[8], fill = colNameste[8]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), option = "A", direction = -1) +
  xlab("trend") + ylab("entropy")+ theme(legend.position="none")+ggtitle("snaive")
#rwd
int2 <- ggplot(
  data = trend.entropy.w,
  aes_string(
    x = trend.entropy.w$trend,
    y = trend.entropy.w$entropy, z = colNameste[6], fill = colNameste[6]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("trend") + ylab("entropy")+ theme(legend.position="none")+ggtitle("rwd")
#rw
int3 <- ggplot(
  data = trend.entropy.w,
  aes_string(
    x = trend.entropy.w$trend,
    y = trend.entropy.w$entropy, z = colNameste[5], fill = colNameste[5]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("trend") + ylab("entropy")+ theme(legend.position="none")+ggtitle("rw")
#ARIMA
int4 <- ggplot(
  data = trend.entropy.w,
  aes_string(
    x = trend.entropy.w$trend,
    y = trend.entropy.w$entropy, z = colNameste[1], fill = colNameste[1]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("trend") + ylab("entropy")+ theme(legend.position="none")+ggtitle("ARIMA")
#SARIMA
int5 <- ggplot(
  data = trend.entropy.w,
  aes_string(
    x = trend.entropy.w$trend,
    y = trend.entropy.w$entropy, z = colNameste[7], fill = colNameste[7]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("trend") + ylab("entropy")+ theme(legend.position="none")+ggtitle("SARIMA")
#stlar
int6 <- ggplot(
  data = trend.entropy.w,
  aes_string(
    x = trend.entropy.w$trend,
    y = trend.entropy.w$entropy, z = colNameste[9], fill = colNameste[9]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("trend") + ylab("entropy")+ theme(legend.position="none")+ggtitle("stlar")
#mstlets
int7 <- ggplot(
  data = trend.entropy.w,
  aes_string(
    x = trend.entropy.w$trend,
    y = trend.entropy.w$entropy, z = colNameste[3], fill = colNameste[3]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), option = "A", direction = -1) +
  xlab("trend") + ylab("entropy")+ theme(legend.position="none")+ggtitle("mstlets")
#tbats
int8 <- ggplot(
  data = trend.entropy.w,
  aes_string(
    x = trend.entropy.w$trend,
    y = trend.entropy.w$entropy, z = colNameste[10], fill = colNameste[10]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), option = "A", direction = -1) +
  xlab("trend") + ylab("entropy")+ theme(legend.position="none")+ggtitle("tbats")
#ARMA/AR/MA
int9 <- ggplot(
  data = trend.entropy.w,
  aes_string(
    x = trend.entropy.w$trend,
    y = trend.entropy.w$entropy, z = colNameste[2], fill = colNameste[2]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.05), breaks = seq(0, 0.05, 100), option = "A", direction = -1) +
  xlab("trend") + ylab("entropy")+ theme(legend.position="none")+ggtitle("ARMA/AR/MA")
#wn
int10 <- ggplot(
  data = trend.entropy.w,
  aes_string(
    x = trend.entropy.w$trend,
    y = trend.entropy.w$entropy, z = colNameste[12], fill = colNameste[12]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.8), breaks = seq(0, 0.8, 100), option = "A", direction = -1) +
  xlab("trend") + ylab("entropy")+ theme(legend.position="none")+ggtitle("wn")
#theta
int11 <- ggplot(
  data = trend.entropy.w,
  aes_string(
    x = trend.entropy.w$trend,
    y = trend.entropy.w$entropy, z = colNameste[11], fill = colNameste[11]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), option = "A", direction = -1) +
  xlab("trend") + ylab("entropy")+ theme(legend.position="none")+ggtitle("theta")
#nn
int12 <- ggplot(
  data = trend.entropy.w,
  aes_string(
    x = trend.entropy.w$trend,
    y = trend.entropy.w$entropy, z = colNameste[4], fill = colNameste[4]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), option = "A", direction = -1) +
  xlab("trend") + ylab("entropy")+ggtitle("nn")

int1+int2+int3+int4+int5+int6+int7+int8+int9+int10+int11+int12+plot_layout(ncol = 6, nrow = 2)

## ---- dtwopdp
load("data/daily/sediff_acf5.seasonal_strength2.d.rda")
colNamesss <- colnames(sediff_acf5.seasonal_strength2.d)[28:37]
# snaive
int1 <- ggplot(
  data = sediff_acf5.seasonal_strength2.d,
  aes_string(
    x = sediff_acf5.seasonal_strength2.d$sediff_acf5,
    y = sediff_acf5.seasonal_strength2.d$seasonal_strength2, 
    z = colNamesss[6], fill = colNamesss[6]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("seasonal_strength2")+ theme(legend.position="none")+
  ggtitle("snaive")

## rw
int2 <- ggplot(
  data = sediff_acf5.seasonal_strength2.d,
  aes_string(
    x = sediff_acf5.seasonal_strength2.d$sediff_acf5,
    y = sediff_acf5.seasonal_strength2.d$seasonal_strength2, 
    z = colNamesss[4], fill = colNamesss[4]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("seasonal_strength2")+ theme(legend.position="none")+
  ggtitle("rw")
## rwd
int3 <- ggplot(
  data = sediff_acf5.seasonal_strength2.d,
  aes_string(
    x = sediff_acf5.seasonal_strength2.d$sediff_acf5,
    y = sediff_acf5.seasonal_strength2.d$seasonal_strength2, 
    z = colNamesss[5], fill = colNamesss[5]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("seasonal_strength2")+ theme(legend.position="none")+
  ggtitle("rwd")
## mstlarima
int4 <- ggplot(
  data = sediff_acf5.seasonal_strength2.d,
  aes_string(
    x = sediff_acf5.seasonal_strength2.d$sediff_acf5,
    y = sediff_acf5.seasonal_strength2.d$seasonal_strength2, 
    z = colNamesss[1], fill = colNamesss[1]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("seasonal_strength2")+ theme(legend.position="none")+
  ggtitle("mstlarima")
## mstlets
int5 <- ggplot(
  data = sediff_acf5.seasonal_strength2.d,
  aes_string(
    x = sediff_acf5.seasonal_strength2.d$sediff_acf5,
    y = sediff_acf5.seasonal_strength2.d$seasonal_strength2, 
    z = colNamesss[2], fill = colNamesss[2]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("seasonal_strength2")+ theme(legend.position="none")+
  ggtitle("mstlets")
## tbats
int6 <- ggplot(
  data = sediff_acf5.seasonal_strength2.d,
  aes_string(
    x = sediff_acf5.seasonal_strength2.d$sediff_acf5,
    y = sediff_acf5.seasonal_strength2.d$seasonal_strength2, 
    z = colNamesss[8], fill = colNamesss[8]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("seasonal_strength2")+ theme(legend.position="none")+
  ggtitle("tbats")
## stlar
int7 <- ggplot(
  data = sediff_acf5.seasonal_strength2.d,
  aes_string(
    x = sediff_acf5.seasonal_strength2.d$sediff_acf5,
    y = sediff_acf5.seasonal_strength2.d$seasonal_strength2, 
    z = colNamesss[7], fill = colNamesss[7]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("seasonal_strength2")+ theme(legend.position="none")+
  ggtitle("stlar")
## theta
int8 <- ggplot(
  data = sediff_acf5.seasonal_strength2.d,
  aes_string(
    x = sediff_acf5.seasonal_strength2.d$sediff_acf5,
    y = sediff_acf5.seasonal_strength2.d$seasonal_strength2, 
    z = colNamesss[9], fill = colNamesss[9]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("seasonal_strength2")+ theme(legend.position="none")+
  ggtitle("theta")
## nn
int9 <- ggplot(
  data = sediff_acf5.seasonal_strength2.d,
  aes_string(
    x = sediff_acf5.seasonal_strength2.d$sediff_acf5,
    y = sediff_acf5.seasonal_strength2.d$seasonal_strength2, 
    z = colNamesss[3], fill = colNamesss[3]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("seasonal_strength2")+ theme(legend.position="none")+
  ggtitle("nn")
## wn
int10 <- ggplot(
  data = sediff_acf5.seasonal_strength2.d,
  aes_string(
    x = sediff_acf5.seasonal_strength2.d$sediff_acf5,
    y = sediff_acf5.seasonal_strength2.d$seasonal_strength2, 
    z = colNamesss[10], fill = colNamesss[10]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.9), breaks = seq(0, 0.9, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("seasonal_strength2")+theme(legend.title=element_blank())+
  ggtitle("wn")
int1+int2+int3+int4+int5+int6+int7+int8+int9+int10+plot_layout(ncol = 5, nrow = 2)

## ---- htwopdp
load("data/hourly/linearity.sediff_seacf1.h.rda")
colNamesss <- colnames(linearity.sediff_seacf1.h)[28:37]
# snaive
int1 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[6], fill = colNamesss[6]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("snaive")

## rw
int2 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[4], fill = colNamesss[4]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("rw")
## rwd
int3 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[5], fill = colNamesss[5]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("rwd")
## mstlarima
int4 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[1], fill = colNamesss[1]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("mstlarima")
## mstlets
int5 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[2], fill = colNamesss[2]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("mstlets")
## tbats
int6 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[8], fill = colNamesss[8]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("tbats")
## stlar
int7 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[7], fill = colNamesss[7]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("stlar")
## theta
int8 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[9], fill = colNamesss[9]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("theta")
## nn
int9 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[3], fill = colNamesss[3]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("nn")
## wn
int10 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[10], fill = colNamesss[10]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.8), breaks = seq(0, 0.8, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ggtitle("wn")+theme(legend.title=element_blank())

int1+int2+int3+int4+int5+int6+int7+int8+int9+int10+plot_layout(ncol = 5, nrow = 2)
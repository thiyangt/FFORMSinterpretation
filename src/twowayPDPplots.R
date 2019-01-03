## ---- qpdptwo
load("data/quarterly/diff1y_acf5.N.q.rda")
load("data/quarterly/diff1y_acf5.beta.q.rda")
load("data/quarterly/diff1y_acf5.stability.q.rda")
colNamesdn <- colnames(diff1y_acf5.N.q)[32:48]
colNamesdb <- colnames(diff1y_acf5.beta.q)[32:48]
colNamesds <- colnames(diff1y_acf5.stability.q)[32:48]

# snaive
int1 <- ggplot(
  data = diff1y_acf5.N.q, aes_string(x = diff1y_acf5.N.q$diff1y_acf5, y = diff1y_acf5.N.q$N, z = colNamesdn[13], fill = colNamesdn[13]
  ))+geom_tile() + 
  scale_fill_viridis_c(limits = c(0, 0.05), breaks = seq(0, 0.05, 100),option = "A", direction = -1)+
  xlab("diff1y_acf5") + ylab("N") + theme(legend.position="none")+ggtitle("snaive")
int2 <- ggplot(
  data = diff1y_acf5.beta.q,
  aes_string(
    x = diff1y_acf5.beta.q$diff1y_acf5,
    y = diff1y_acf5.beta.q$beta, z = colNamesdb[13], fill = colNamesdb[13]
  )) +geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("beta")+ theme(legend.position="none")+ggtitle("snaive")
int3 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[13], fill = colNamesds[13]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("snaive")

# rwd(42)

int4 <- ggplot(
  data = diff1y_acf5.N.q, aes_string(x = diff1y_acf5.N.q$diff1y_acf5, y = diff1y_acf5.N.q$N, z = colNamesdn[42], fill = colNamesdn[42]
  ))+geom_tile() + 
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100),option = "A", direction = -1)+
  xlab("diff1y_acf5") + ylab("N") + theme(legend.position="none")+ggtitle("rwd")
int5 <- ggplot(
  data = diff1y_acf5.beta.q,
  aes_string(
    x = diff1y_acf5.beta.q$diff1y_acf5,
    y = diff1y_acf5.beta.q$beta, z = colNamesdb[11], fill = colNamesdb[11]
  )) +geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("beta")+ theme(legend.position="none")+ggtitle("rwd")
int6 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[11], fill = colNamesds[11]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("rwd")


# rw(41)
int7 <- ggplot(
  data = diff1y_acf5.N.q, aes_string(x = diff1y_acf5.N.q$diff1y_acf5, y = diff1y_acf5.N.q$N, z = colNamesdn[10], fill = colNamesdn[10]
  ))+geom_tile() + 
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100),option = "A", direction = -1)+
  xlab("diff1y_acf5") + ylab("N") + theme(legend.position="none")+ggtitle("rw")
int8 <- ggplot(
  data = diff1y_acf5.beta.q,
  aes_string(
    x = diff1y_acf5.beta.q$diff1y_acf5,
    y = diff1y_acf5.beta.q$beta, z = colNamesdb[10], fill = colNamesdb[10]
  )) +geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("beta")+ theme(legend.position="none")+ggtitle("rw")
int9 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[10], fill = colNamesds[10]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("rw")


# ETS-notrendnoseasonal(36)
int10 <- ggplot(
  data = diff1y_acf5.N.q, aes_string(x = diff1y_acf5.N.q$diff1y_acf5, y = diff1y_acf5.N.q$N, z = colNamesdn[5], fill = colNamesdn[5]
  ))+geom_tile() + 
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100),option = "A", direction = -1)+
  xlab("diff1y_acf5") + ylab("N") + theme(legend.position="none")+ggtitle("ETS-notrendnoseasonal")
int11 <- ggplot(
  data = diff1y_acf5.beta.q,
  aes_string(
    x = diff1y_acf5.beta.q$diff1y_acf5,
    y = diff1y_acf5.beta.q$beta, z = colNamesdb[5], fill = colNamesdb[5]
  )) +geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("beta")+ theme(legend.position="none")+ggtitle("ETS-notrendnoseasonal")
int12 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[5], fill = colNamesds[5]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("ETS-notrendnoseasonal")


# ETS-dampedtrend(34)
int13 <- ggplot(
  data = diff1y_acf5.N.q, aes_string(x = diff1y_acf5.N.q$diff1y_acf5, y = diff1y_acf5.N.q$N, z = colNamesdn[3], fill = colNamesdn[3]
  ))+geom_tile() + 
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100),option = "A", direction = -1)+
  xlab("diff1y_acf5") + ylab("N") + theme(legend.position="none")+ggtitle("ETS-dampedtrend")
int14 <- ggplot(
  data = diff1y_acf5.beta.q,
  aes_string(
    x = diff1y_acf5.beta.q$diff1y_acf5,
    y = diff1y_acf5.beta.q$beta, z = colNamesdb[3], fill = colNamesdb[3]
  )) +geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("beta")+ theme(legend.position="none")+ggtitle("ETS-dampedtrend")
int15 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[3], fill = colNamesds[3]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("ETS-dampedtrend")

# ETS-trend(38)
int16 <- ggplot(
  data = diff1y_acf5.N.q, aes_string(x = diff1y_acf5.N.q$diff1y_acf5, y = diff1y_acf5.N.q$N, z = colNamesdn[7], fill = colNamesdn[7]
  ))+geom_tile() + 
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100),option = "A", direction = -1)+
  xlab("diff1y_acf5") + ylab("N") + theme(legend.position="none")+ggtitle("ETS-trend")
int17 <- ggplot(
  data = diff1y_acf5.beta.q,
  aes_string(
    x = diff1y_acf5.beta.q$diff1y_acf5,
    y = diff1y_acf5.beta.q$beta, z = colNamesdb[7], fill = colNamesdb[7]
  )) +geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("beta")+ theme(legend.position="none")+ggtitle("ETS-trend")
int18 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[7], fill = colNamesds[7]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("ETS-trend")

# ETS-dampedtrendseasonal(35)
int19 <- ggplot(
  data = diff1y_acf5.N.q, aes_string(x = diff1y_acf5.N.q$diff1y_acf5, y = diff1y_acf5.N.q$N, z = colNamesdn[4], fill = colNamesdn[4]
  ))+geom_tile() + 
  scale_fill_viridis_c(limits = c(0, 0.05), breaks = seq(0, 0.05, 100),option = "A", direction = -1)+
  xlab("diff1y_acf5") + ylab("N") + theme(legend.position="none")+ggtitle("ETS-dampedtrendseasonal")
int20 <- ggplot(
  data = diff1y_acf5.beta.q,
  aes_string(
    x = diff1y_acf5.beta.q$diff1y_acf5,
    y = diff1y_acf5.beta.q$beta, z = colNamesdb[4], fill = colNamesdb[4]
  )) +geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("beta")+ theme(legend.position="none")+ggtitle("ETS-dampedtrendseasonal")
int21 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[4], fill = colNamesds[4]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("ETS-dampedtrendseasonal")

# ETS-trendseasonal(39)
int22 <- ggplot(
  data = diff1y_acf5.N.q, aes_string(x = diff1y_acf5.N.q$diff1y_acf5, y = diff1y_acf5.N.q$N, z = colNamesdn[8], fill = colNamesdn[8]
  ))+geom_tile() + 
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100),option = "A", direction = -1)+
  xlab("diff1y_acf5") + ylab("N") + theme(legend.position="none")+ggtitle("ETS-trendseasonal")
int23 <- ggplot(
  data = diff1y_acf5.beta.q,
  aes_string(
    x = diff1y_acf5.beta.q$diff1y_acf5,
    y = diff1y_acf5.beta.q$beta, z = colNamesdb[8], fill = colNamesdb[8]
  )) +geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("beta")+ theme(legend.position="none")+ggtitle("ETS-trendseasonal")
int24 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[8], fill = colNamesds[8]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("ETS-trendseasonal")

# ETS-seasonal(37)
int25 <- ggplot(
  data = diff1y_acf5.N.q, aes_string(x = diff1y_acf5.N.q$diff1y_acf5, y = diff1y_acf5.N.q$N, z = colNamesdn[6], fill = colNamesdn[6]
  ))+geom_tile() + 
  scale_fill_viridis_c(limits = c(0, 0.03), breaks = seq(0, 0.03, 100),option = "A", direction = -1)+
  xlab("diff1y_acf5") + ylab("N") + theme(legend.position="none")+ggtitle("ETS-seasonal")
int26 <- ggplot(
  data = diff1y_acf5.beta.q,
  aes_string(
    x = diff1y_acf5.beta.q$diff1y_acf5,
    y = diff1y_acf5.beta.q$beta, z = colNamesdb[6], fill = colNamesdb[6]
  )) +geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("beta")+ theme(legend.position="none")+ggtitle("ETS-seasonal")
int27 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[6], fill = colNamesds[6]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("ETS-seasonal")

# SARIMA(43)
int28 <- ggplot(
  data = diff1y_acf5.N.q, aes_string(x = diff1y_acf5.N.q$diff1y_acf5, y = diff1y_acf5.N.q$N, z = colNamesdn[12], fill = colNamesdn[12]
  ))+geom_tile() + 
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100),option = "A", direction = -1)+
  xlab("diff1y_acf5") + ylab("N") + theme(legend.position="none")+ggtitle("SARIMA")
int29 <- ggplot(
  data = diff1y_acf5.beta.q,
  aes_string(
    x = diff1y_acf5.beta.q$diff1y_acf5,
    y = diff1y_acf5.beta.q$beta, z = colNamesdb[12], fill = colNamesdb[12]
  )) +geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("beta")+ theme(legend.position="none")+ggtitle("SARIMA")
int30 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[12], fill = colNamesds[12]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("SARIMA")

# ARIMA(32)
int31 <- ggplot(
  data = diff1y_acf5.N.q, aes_string(x = diff1y_acf5.N.q$diff1y_acf5, y = diff1y_acf5.N.q$N, z = colNamesdn[1], fill = colNamesdn[1]
  ))+geom_tile() + 
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100),option = "A", direction = -1)+
  xlab("diff1y_acf5") + ylab("N") + theme(legend.position="none")+ggtitle("ARIMA")
int32 <- ggplot(
  data = diff1y_acf5.beta.q,
  aes_string(
    x = diff1y_acf5.beta.q$diff1y_acf5,
    y = diff1y_acf5.beta.q$beta, z = colNamesdb[1], fill = colNamesdb[1]
  )) +geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("beta")+ theme(legend.position="none")+ggtitle("ARIMA")
int33 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[1], fill = colNamesds[1]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("ARIMA")

# ARMA/AR/MA(33)
int34 <- ggplot(
  data = diff1y_acf5.N.q, aes_string(x = diff1y_acf5.N.q$diff1y_acf5, y = diff1y_acf5.N.q$N, z = colNamesdn[2], fill = colNamesdn[2]
  ))+geom_tile() + 
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100),option = "A", direction = -1)+
  xlab("diff1y_acf5") + ylab("N") + theme(legend.position="none")+ggtitle("ARMA/AR/MA")
int35 <- ggplot(
  data = diff1y_acf5.beta.q,
  aes_string(
    x = diff1y_acf5.beta.q$diff1y_acf5,
    y = diff1y_acf5.beta.q$beta, z = colNamesdb[2], fill = colNamesdb[2]
  )) +geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("beta")+ theme(legend.position="none")+ggtitle("ARMA/AR/MA")
int36 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[2], fill = colNamesds[2]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("ARMA/AR/MA")

# stlar(45)
int37 <- ggplot(
  data = diff1y_acf5.N.q, aes_string(x = diff1y_acf5.N.q$diff1y_acf5, y = diff1y_acf5.N.q$N, z = colNamesdn[14], fill = colNamesdn[14]
  ))+geom_tile() + 
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100),option = "A", direction = -1)+
  xlab("diff1y_acf5") + ylab("N") + theme(legend.position="none")+ggtitle("stlar")
int38 <- ggplot(
  data = diff1y_acf5.beta.q,
  aes_string(
    x = diff1y_acf5.beta.q$diff1y_acf5,
    y = diff1y_acf5.beta.q$beta, z = colNamesdb[14], fill = colNamesdb[14]
  )) +geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("beta")+ theme(legend.position="none")+ggtitle("stlar")
int39 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[14], fill = colNamesds[14]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("stlar")

# tbats(46)
int40 <- ggplot(
  data = diff1y_acf5.N.q, aes_string(x = diff1y_acf5.N.q$diff1y_acf5, y = diff1y_acf5.N.q$N, z = colNamesdn[15], fill = colNamesdn[15]
  ))+geom_tile() + 
  scale_fill_viridis_c(limits = c(0, 0.08), breaks = seq(0, 0.08, 100),option = "A", direction = -1)+
  xlab("diff1y_acf5") + ylab("N") + theme(legend.position="none")+ggtitle("tbats")
int41 <- ggplot(
  data = diff1y_acf5.beta.q,
  aes_string(
    x = diff1y_acf5.beta.q$diff1y_acf5,
    y = diff1y_acf5.beta.q$beta, z = colNamesdb[15], fill = colNamesdb[15]
  )) +geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("beta")+ theme(legend.position="none")+ggtitle("tbats")
int42 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[15], fill = colNamesds[15]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("tbats")

# wn(48)
int43 <- ggplot(
  data = diff1y_acf5.N.q, aes_string(x = diff1y_acf5.N.q$diff1y_acf5, y = diff1y_acf5.N.q$N, z = colNamesdn[17], fill = colNamesdn[17]
  ))+geom_tile() + 
  scale_fill_viridis_c(limits = c(0, 0.8), breaks = seq(0, 0.8, 100),option = "A", direction = -1)+
  xlab("diff1y_acf5") + ylab("N") + theme(legend.position="none")+ggtitle("wn")
int44 <- ggplot(
  data = diff1y_acf5.beta.q,
  aes_string(
    x = diff1y_acf5.beta.q$diff1y_acf5,
    y = diff1y_acf5.beta.q$beta, z = colNamesdb[17], fill = colNamesdb[17]
  )) +geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.8), breaks = seq(0, 0.8, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("beta")+ theme(legend.position="none")+ggtitle("wn")
int45 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[17], fill = colNamesds[17]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.8), breaks = seq(0, 0.8, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("wn")

# theta(47)
int46 <- ggplot(
  data = diff1y_acf5.N.q, aes_string(x = diff1y_acf5.N.q$diff1y_acf5, y = diff1y_acf5.N.q$N, z = colNamesdn[16], fill = colNamesdn[16]
  ))+geom_tile() + 
  scale_fill_viridis_c(limits = c(0, 0.5), breaks = seq(0, 0.5, 100),option = "A", direction = -1)+
  xlab("diff1y_acf5") + ylab("N") + theme(legend.position="none")+ggtitle("theta")
int47 <- ggplot(
  data = diff1y_acf5.beta.q,
  aes_string(
    x = diff1y_acf5.beta.q$diff1y_acf5,
    y = diff1y_acf5.beta.q$beta, z = colNamesdb[16], fill = colNamesdb[16]
  )) +geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("beta")+ theme(legend.position="none")+ggtitle("theta")
int48 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[16], fill = colNamesds[16]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("theta")

# nn(40)
int49 <- ggplot(
  data = diff1y_acf5.N.q, aes_string(x = diff1y_acf5.N.q$diff1y_acf5, y = diff1y_acf5.N.q$N, z = colNamesdn[9], fill = colNamesdn[9]
  ))+geom_tile() + 
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100),option = "A", direction = -1)+
  xlab("diff1y_acf5") + ylab("N") + theme(legend.position="none")+ggtitle("nn")
int50 <- ggplot(
  data = diff1y_acf5.beta.q,
  aes_string(
    x = diff1y_acf5.beta.q$diff1y_acf5,
    y = diff1y_acf5.beta.q$beta, z = colNamesdb[9], fill = colNamesdb[9]
  )) +geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("beta")+ theme(legend.position="none")+ggtitle("nn")
int51 <- ggplot(
  data = diff1y_acf5.stability.q,
  aes_string(
    x = diff1y_acf5.stability.q$diff1y_acf5,
    y = diff1y_acf5.stability.q$stability, z = colNamesds[9], fill = colNamesds[9]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), option = "A", direction = -1) +
  xlab("diff1y_acf5") + ylab("stability")+ theme(legend.position="none")+ggtitle("nn")

(int1|int2|int3|int4)/(int5|int6|int7|int8)/(int9|int10|int11|int12)/(int13|int14|int15|int16)/(int17|int18|int19|int20)
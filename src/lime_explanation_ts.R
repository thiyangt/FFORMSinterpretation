## packages
library(Mcomp)
library(seer)
library(forecast)
library(lime)
data(M4)
load("data/limeyts.rda")

## yearly data
ts_lime_ypca <- list(ts1=ts(c(M3[[192]]$x, M3[[192]]$xx)),
                     ts2=ts(limeyts[[1]][[1]]),
                     ts3=limeyts[[2]][[1]],
                     ts4=M4[["Y22891"]]$x,
                     ts5=M4[["Y13190"]]$x,
                     ts6=limeyts[[3]][[1]])
save(ts_lime_ypca, file="data/ts_lime_ypca.rda")


## quarterly series
M1q <- subset(M1, "quarterly")
M3q <- subset(M3, "quarterly")

ts_lime_qpca <- list(ts1=ts(c(M1q[[25]]$x, M1q[[25]]$xx)),
                     ts2=ts(c(M3q[[202]]$x, M3q[[202]]$xx)),
                     ts3=ts(c(M3q[[451]]$x, M3q[[451]]$xx)),
                     ts4=ts(c(M3q[[705]]$x, M3q[[705]]$xx)))
save(ts_lime_qpca, file="data/ts_lime_qpca.rda")

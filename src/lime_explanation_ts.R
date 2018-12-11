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

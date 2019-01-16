## ---- yearlylime
#which.min(m4yPCAresults1$PC1)  #227743
#which.min(m4yPCAresults1$PC2)  #393474
#which.max(m4yPCAresults1$PC1)  # 22719
#which.max(m4yPCAresults1$PC2)  #480357
#which(yearly_training$PC1 < -10 & yearly_training$classlabels=="nn")
# which(yearly_training$PC1 == median(yearly_training$PC1)) ##277166

load("data/yearly/train_votes.rda")
pcaYvariables <- yearly_training[, 1:25]
pcaM4Y <- prcomp(pcaYvariables, center = TRUE, scale = TRUE)
# summary(pcaM1Y)
PC1m4y <- pcaM4Y$x[, 1]
PC2m4y <- pcaM4Y$x[, 2]
PC3m4y <- pcaM4Y$x[, 3]
m4yPCAresults1 <- data.frame(PC1 = PC1m4y, PC2 = PC2m4y, PC3 = PC3m4y, pcaYvariables)
yearly_training$PC1 <- PC1m4y
yearly_training$PC2 <- PC2m4y
pca <- ggplot(m4yPCAresults1, aes(x = PC1, y = PC2)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data =yearly_training[c(373, 227743, 393474, 13442, 480357, 79),], aes(x = PC1, y = PC2), color = "black", size=5) +
 theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))+
  geom_text_repel(
    data = yearly_training[c(373, 227743, 393474, 13442, 480357, 79),],
    aes(label = c("1: ARMA/AR/MA", "2: ETS-trend", "3: ETS-notrendnoseasonal", 
                  "4: nn", "5: rw", "6: rwd")),
    size = 5,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines")
  )
load("data/yearly/explanationy.rda")
load("data/ts_lime_yearpca.rda")
p1 <- autoplot(ts_lime_yearpca[[1]])+theme(legend.position="none")+
  ggtitle("1: ARMA/AR/MA")+xlab("")+theme(axis.title.x=element_blank(),
        #                     axis.text.x=element_blank(),
                             axis.text.y=element_blank())+ylab("")

p2 <- autoplot(ts_lime_yearpca[[2]])+theme(legend.position="none")+
  ggtitle("2: ETS-trend")+xlab("")+theme(axis.title.x=element_blank(),
                      #       axis.text.x=element_blank(),
                             axis.text.y=element_blank())+ylab("")
p3 <- autoplot(ts_lime_yearpca[[3]])+theme(legend.position="none")+
  ggtitle("3: ETS-notrendnoseasonal")+xlab("")+theme(axis.title.x=element_blank(),
                            # axis.text.x=element_blank(),
                             axis.text.y=element_blank())+ylab("")
p4 <- autoplot(ts_lime_yearpca[[4]])+theme(legend.position="none")+
   ggtitle("4: nn")+xlab("")+theme(axis.title.x=element_blank(),
                            # axis.text.x=element_blank(),
                              axis.text.y=element_blank())+ylab("")
p5 <- autoplot(ts_lime_yearpca[[5]])+theme(legend.position="none")+
  ggtitle("5: rw")+xlab("")+theme(axis.title.x=element_blank(),
                             #axis.text.x=element_blank(),
                             axis.text.y=element_blank())+ylab("")
p6 <- autoplot(ts_lime_yearpca[[6]])+theme(legend.position="none")+
  ggtitle("6: rwd")+xlab("")+theme(axis.title.x=element_blank(),
                               #axis.text.x=element_blank(),
                               axis.text.y=element_blank())+ylab("")
pp <- p1 + p2 + p3 + p4+p5+ p6 + plot_layout(ncol = 1)
pca+pp+plot_layout(ncol = 2)

## ---- yearlylime2
load("data/yearly/explanationy.rda")
lime::plot_features(explanationy, ncol = 2)
                       
## ---- quarterlylime
#which.min(m4qPCAresults1$PC1)  #405
#which.min(m4qPCAresults1$PC2)  #653
#which.max(m4qPCAresults1$PC1)  #908
#which.max(m4qPCAresults1$PC2)  #405
# which(0.46 < m4qPCAresults1$PC2 < 0.4692441) ##277166
load("data/quarterly/trainQ_votes.rda")
pcaQvariables <- quarterly_training[, 1:30]
pcaM4Q <- prcomp(pcaQvariables, center = TRUE, scale = TRUE)
PC1m4q <- pcaM4Q$x[, 1]
PC2m4q <- pcaM4Q$x[, 2]
PC3m4q <- pcaM4Q$x[, 3]
m4qPCAresults <- data.frame(PC1 = PC1m4q, PC2 = PC2m4q, PC3 = PC3m4q, pcaQvariables)
quarterly_training$PC1 <- PC1m4q
quarterly_training$PC2 <- PC2m4q

pcaQ <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data =quarterly_training[c(25,178, 653, 182),], aes(x = PC1, y = PC2), color = "black", size=5) +
  theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))+
  geom_text_repel(
    data = quarterly_training[c(25,178, 653, 182),],
    aes(label = c("1: SARIMA", "2: rwd", "3: ETS-trendseasonal", "4: ETS-seasonal")),
    size = 5,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines")
  )


load("data/quarterly/explanationq.rda")
load("data/ts_lime_qpca.rda")
p1 <- autoplot(ts_lime_qpca[[1]])+theme(legend.position="none")+
  ggtitle("1: SARIMA")+xlab("")+theme(axis.title.x=element_blank(),
                                          #                     axis.text.x=element_blank(),
                                          axis.text.y=element_blank())+ylab("")

p2 <- autoplot(ts_lime_qpca[[2]])+theme(legend.position="none")+
  ggtitle("2: rwd")+xlab("")+theme(axis.title.x=element_blank(),
                                         #       axis.text.x=element_blank(),
                                         axis.text.y=element_blank())+ylab("")
p3 <- autoplot(ts_lime_qpca[[3]])+theme(legend.position="none")+
  ggtitle("3: ETS-trendseasonal")+xlab("")+theme(axis.title.x=element_blank(),
                                                     # axis.text.x=element_blank(),
                                                     axis.text.y=element_blank())+ylab("")
p4 <- autoplot(ts_lime_qpca[[4]])+theme(legend.position="none")+
  ggtitle("4: ETS-seasonal")+xlab("")+theme(axis.title.x=element_blank(),
                                  # axis.text.x=element_blank(),
                                  axis.text.y=element_blank())+ylab("")

pp <- p1 + p2 + p3 + p4 + plot_layout(ncol = 1)
pcaQ+pp+plot_layout(ncol = 2)

## ---- quarterlylime2
plot_features(explanationq, ncol = 2)


## ---- hourlylime
load("data/hourly/hourly_training.rda")
pcaHvariables <- hourly_training[, 1:26]
pcaM4H <- prcomp(pcaHvariables, center = TRUE, scale = TRUE)
PC1m4h <- pcaM4H$x[, 1]
PC2m4h <- pcaM4H$x[, 2]
PC3m4h <- pcaM4H$x[, 3]
m4hPCAresults <- data.frame(PC1 = PC1m4h, PC2 = PC2m4h, PC3 = PC3m4h, pcaHvariables)
hourly_training$PC1 <- PC1m4h
hourly_training$PC2 <- PC2m4h

pcaH <- ggplot(m4hPCAresults, aes(x = PC1, y = PC2)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data =hourly_training[c(53,199, 196, 137),], aes(x = PC1, y = PC2), color = "black", size=5) +
  theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))+
  geom_text_repel(
    data = hourly_training[c(53,199, 196, 137),],
    aes(label = c("1: snaive", "2: nn", "3: tbats", "4: mstlarima")),
    size = 5,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines")
  )


load("data/hourly/explanationh.rda")
load("data/ts_lime_hpca.rda")
p1 <- autoplot(ts_lime_hpca[[1]])+theme(legend.position="none")+
  ggtitle("1: snaive")+xlab("")+theme(axis.title.x=element_blank(),
                                      #                     axis.text.x=element_blank(),
                                      axis.text.y=element_blank())+ylab("")

p2 <- autoplot(ts_lime_hpca[[2]])+theme(legend.position="none")+
  ggtitle("2: nn")+xlab("")+theme(axis.title.x=element_blank(),
                                   #       axis.text.x=element_blank(),
                                   axis.text.y=element_blank())+ylab("")
p3 <- autoplot(ts_lime_hpca[[3]])+theme(legend.position="none")+
  ggtitle("3: tbats")+xlab("")+theme(axis.title.x=element_blank(),
                                                 # axis.text.x=element_blank(),
                                                 axis.text.y=element_blank())+ylab("")
p4 <- autoplot(ts_lime_hpca[[4]])+theme(legend.position="none")+
  ggtitle("4: mstlarima")+xlab("")+theme(axis.title.x=element_blank(),
                                            # axis.text.x=element_blank(),
                                            axis.text.y=element_blank())+ylab("")

pp <- p1 + p2 + p3 + p4 + plot_layout(ncol = 1)
pcaH+pp+plot_layout(ncol = 2)

## ---- hourlylime2
plot_features(explanationh, ncol = 2)

## ---- monthlylime
#which.min(m4mPCAresults$PC1)  #632
#which.min(m4mPCAresults$PC2)  #403
#which.max(m4mPCAresults$PC1)  #1810
#which.max(m4mPCAresults$PC2)  #445
# which((-2 > m4mPCAresults$PC2 )==TRUE )##277166
load("data/monthly/trainM_votes.rda")
pcaMvariables <- monthly_training[, 1:30]
pcaM4M <- prcomp(pcaMvariables, center = TRUE, scale = TRUE)
PC1m4m <- pcaM4M$x[, 1]
PC2m4m <- pcaM4M$x[, 2]
PC3m4m <- pcaM4M$x[, 3]
m4mPCAresults <- data.frame(PC1 = PC1m4m, PC2 = PC2m4m, PC3 = PC3m4m, pcaMvariables)
monthly_training$PC1 <- PC1m4m
monthly_training$PC2 <- PC2m4m

pcaM <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data =monthly_training[c(1, 971, 980, 632, 403, 1810, 445, 999, 997, 2036),], aes(x = PC1, y = PC2), color = "black", size=5) +
  theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))+
  geom_text_repel(
    data = monthly_training[c(1, 971, 980, 632, 403, 1810, 445),],
    aes(label = c("1: wn", "2: SARIMA", "3: tbats", "4: theta")),
    size = 5,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines")
  )

## ---- limeweekly 
load("data/monthly/trainM_votes.rda")
pcaMvariables <- monthly_training[, 1:30]
pcaM4M <- prcomp(pcaMvariables, center = TRUE, scale = TRUE)
PC1m4m <- pcaM4M$x[, 1]
PC2m4m <- pcaM4M$x[, 2]
PC3m4m <- pcaM4M$x[, 3]
m4mPCAresults <- data.frame(PC1 = PC1m4m, PC2 = PC2m4m, PC3 = PC3m4m, pcaMvariables)
monthly_training$PC1 <- PC1m4m
monthly_training$PC2 <- PC2m4m

pcaM <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data =monthly_training[c(1, 971, 980, 632, 403, 1810, 445, 999, 997, 2036),], aes(x = PC1, y = PC2), color = "black", size=5) +
  theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))+
  geom_text_repel(
    data = monthly_training[c(1, 971, 980, 632, 403, 1810, 445),],
    aes(label = c("1: wn", "2: SARIMA", "3: tbats", "4: theta")),
    size = 5,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines")
  )
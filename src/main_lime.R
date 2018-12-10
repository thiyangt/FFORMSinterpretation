## ---- yearly_lime
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
  geom_point(data =yearly_training[c(373, 227743, 393474,  22719, 13442, 480357),], aes(x = PC1, y = PC2), color = "black", size=5) +
 theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))+
  geom_text_repel(
    data = yearly_training[c(373, 227743, 393474,  22719, 13442, 480357),],
    aes(label = c("1: ARMA/AR/MA", "2: ETS-trend", "3: ETS-notrendnoseasonal", "4: nn",
                  "5: nn", "6: rw")),
    size = 5,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines")
  )
load("data/yearly/explanationy.rda")
p1 <- autoplot(ts(c(M3[[192]]$x, M3[[192]]$xx)))+theme(legend.position="none")+
  ggtitle("1: ARMA/AR/MA")+xlab("")+theme(axis.title.x=element_blank(),
        #                     axis.text.x=element_blank(),
                             axis.text.y=element_blank())+ylab("")

load("data/limeyts.rda")
data(M4)
p2 <- autoplot(ts(limeyts[[1]][[1]]))+theme(legend.position="none")+
  ggtitle("2: ETS-trend")+xlab("")+theme(axis.title.x=element_blank(),
                      #       axis.text.x=element_blank(),
                             axis.text.y=element_blank())+ylab("")
p3 <- autoplot(limeyts[[2]][[1]])+theme(legend.position="none")+
  ggtitle("3: ETS-notrendnoseasonal")+xlab("")+theme(axis.title.x=element_blank(),
                            # axis.text.x=element_blank(),
                             axis.text.y=element_blank())+ylab("")
p4 <- autoplot(M4[["Y22891"]]$x)+theme(legend.position="none")+
  ggtitle("4: nn")+xlab("")+theme(axis.title.x=element_blank(),
                            # axis.text.x=element_blank(),
                             axis.text.y=element_blank())+ylab("")
p5 <- autoplot(M4[["Y13190"]]$x)+theme(legend.position="none")+
  ggtitle("5: nn")+xlab("")+theme(axis.title.x=element_blank(),
                             #axis.text.x=element_blank(),
                             axis.text.y=element_blank())+ylab("")
p6 <- autoplot(limeyts[[3]][[1]])+theme(legend.position="none")+
  ggtitle("6: rw")+xlab("")+theme(axis.title.x=element_blank(),
                               #axis.text.x=element_blank(),
                               axis.text.y=element_blank())+ylab("")
pp <- p1 + p2 + p3 + p4 + p5+ p6 + plot_layout(ncol = 1)
pca+pp+plot_layout(ncol = 2)
plot_features(explanationy, ncol = 2)
                       
                                                                             

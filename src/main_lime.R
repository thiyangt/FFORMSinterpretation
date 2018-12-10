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
pca1M4Y <- ggplot(m4yPCAresults1, aes(x = PC1, y = PC2)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data =yearly_training[c(373, 227743, 393474,  22719, 13442, 480357),], aes(x = PC1, y = PC2), color = "black", size=5) +
 theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))+
  geom_text_repel(
    data = yearly_training[c(373, 227743, 393474,  22719, 13442, 480357),],
    aes(label = classlabels),
    size = 5,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines")
  )
pca1M4Y
load("data/yearly/explanationy.rda")
plot_features(explanationy, ncol = 2)
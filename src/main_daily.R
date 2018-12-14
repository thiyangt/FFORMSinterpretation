#################################################################
#                  Daily data                                  #
#################################################################
## ---- oob_daily
load("data/daily/trainD_votes.rda") # oob votes from the random forest
load("data/daily/trainD_predictions_oob.rda") # based on oob prediction
load("data/daily/daily_training.rda") # random forest training set
votes_oobD <- data.frame(trainD_votes)
names(votes_oobD) <- names(table(trainD_predictions_oob))
votes_oobD$predicted <- trainD_predictions_oob
votes_oobD$classlabel <- daily_training$classlabels
votes_oobD <- votes_oobD %>%
  mutate(id = seq_len(n())) %>%
  melt(id.var = c("classlabel", "id", "predicted"), na.rm = T) %>%
  select(-id)
# new addition to arrange labels
votes_oobD$classlabel <- factor(votes_oobD$classlabel, levels = rev(c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                                                       "theta","nn","wn")))
oob_boxplot_daily <- ggplot(votes_oobD, aes(x = variable, y = value, fill = classlabel)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.4) +
  ylab("Proportion") +
  xlab("") +
  theme(legend.position = "right", legend.title = element_blank(), legend.text.align = 0, text = element_text(size = 20)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = rev(c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                  "theta","nn","wn"))) +
  coord_flip()
oob_boxplot_daily

## ---- vi_daily
# All variable scores into one dataframe
load("data/daily/trainD_importance.rda")
load(file = "data/daily/sd_pdf_dfD.rda")
load(file = "data/daily/sd_ice_dfD.rda")
## Permutation based
train_imp_dfD <- data.frame(trainD_importance)
train_imp_dfD <- add_rownames(train_imp_dfD, "Feature")
train_imp_dfD <- within(train_imp_dfD, rm("MeanDecreaseAccuracy", "MeanDecreaseGini"))
permutation_impD <- train_imp_dfD %>% melt(id.vars = "Feature")
#dim(permutation_impD) # 260 3
colnames(permutation_impD) <- c("feature", "class", "score")

## PDP-based
sd_pdf_dfD <- add_rownames(sd_pdf_dfD, "class")
pdp_imp <- sd_pdf_dfD %>% melt(id.vars = "class")
colnames(pdp_imp) <- c("class", "feature", "score")

## ICE-based
sd_ice_dfD <- add_rownames(sd_ice_dfD, "class")
ice_imp <- sd_ice_dfD %>% melt(id.vars = "class")
colnames(ice_imp) <- c("class", "feature", "score")

## Combine the data frames
importancescoreD <- bind_rows(permutation_impD, pdp_imp)
importancescoreD <- bind_rows(importancescoreD, ice_imp)
importancescoreD$VI <- rep(c("permutation", "PDP", "ICE"), each = 260)

## rank permutation, sd_pdp, and sd_ice scores for each class
importancescoreD$class <- factor(importancescoreD$class,
                                 levels = c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                           "theta","nn","wn"),
                                 labels = c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                           "theta","nn","wn"))

rank_vi_daily_classes <- importancescoreD %>%
  group_by(VI, class) %>%
  mutate(rank = min_rank(score))

## compute mean rank
meanrank_vid_classes <- rank_vi_daily_classes %>% group_by(feature, class) %>% summarise_at(vars(c(rank)), funs(mean))

## overall importance of features to the forest
train_impforestD <- data.frame(trainD_importance)
train_impforestD <- add_rownames(train_impforestD, "Feature")
train_impforestD <- train_impforestD[, c("Feature", "MeanDecreaseAccuracy", "MeanDecreaseGini")]
train_impforestD <- train_impforestD %>%
  mutate(rank_permu = min_rank(MeanDecreaseAccuracy)) %>%
  mutate(rank_gini = min_rank(MeanDecreaseGini))
train_impforestD$mean_rank <- (train_impforestD$rank_permu + train_impforestD$rank_gini) / 2
meanrank_vid_forest <- data.frame(
  feature = train_impforestD$Feature,
  class = rep("overall", 26),
  rank = train_impforestD$mean_rank
)
## combine mean ranks for overall forest and separate classes
meanrank_daily <- dplyr::bind_rows(meanrank_vid_forest, meanrank_vid_classes)
## create horizontal bar chart for ranks
orderOverall <- filter(meanrank_daily, class == "overall")
meanrank_daily$feature <- factor(meanrank_daily$feature, levels = orderOverall$feature[order(orderOverall$rank)])
meanrank_daily$class <- factor(meanrank_daily$class,
                                   levels = c(
                                     "overall","snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                     "theta","nn","wn"
                                   ),
                                   labels = c(
                                     "overall","snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                     "theta","nn","wn"
                                   )
)

meanrank_daily$rn <- 1:286
topq <- meanrank_daily %>%
  group_by(class) %>%
  top_n(n = 5, wt = rank)
meanrank_daily$istop <- ifelse(meanrank_daily$rn %in% topq$rn, TRUE, FALSE)
feaImp_daily <- ggplot(meanrank_daily, aes(y = rank, x = feature,fill=as.factor(istop))) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~class, ncol = 6, nrow = 2) +
  coord_flip() + ylab("Average rank")+ 
  scale_fill_manual(breaks=c("0","1"), values=c("black","red"), guide="none")
feaImp_daily

## ---- daily_pdp
## load ICE calculation files
## entropy
load("data/daily/pdp_ice_daily/entropyD_includeout.rda")
entropyD_includeout$variable <- rep(1:1000, 20)
## lumpiness
load("data/daily/pdp_ice_daily/lumpinessD_includeout.rda")
lumpinessD_includeout$variable <- rep(1:1000, 20)
## stability
load("data/daily/pdp_ice_daily/stabilityD_includeout.rda")
stabilityD_includeout$variable <- rep(1:1000, 20)
## hurst
load("data/daily/pdp_ice_daily/hurstD_includeout.rda")
hurstD_includeout$variable <- rep(1:1000, 20)
## trend
load("data/daily/pdp_ice_daily/trendD_includeout.rda")
trendD_includeout$variable <- rep(1:1000, 20)
## spikiness
load("data/daily/pdp_ice_daily/spikinessD_includeout.rda")
spikinessD_includeout$variable <- rep(1:1000, 20)
## linearity
load("data/daily/pdp_ice_daily/linearityD_includeout.rda")
linearityD_includeout$variable <- rep(1:1000, 20)
## curvature
load("data/daily/pdp_ice_daily/curvatureD_includeout.rda")
curvatureD_includeout$variable <- rep(1:1000, 20)
## e_acf1
load("data/daily/pdp_ice_daily/e_acf1D_includeout.rda")
e_acf1D_includeout$variable <- rep(1:1000, 20) 
## y_acf1
load("data/daily/pdp_ice_daily/y_acf1D_includeout.rda")
y_acf1D_includeout$variable <- rep(1:1000, 20)
## diff1y_acf1
load("data/daily/pdp_ice_daily/diff1y_acf1D_includeout.rda")
diff1y_acf1D_includeout$variable <- rep(1:1000, 20)
## diff2y_acf1
load("data/daily/pdp_ice_daily/diff2y_acf1D_includeout.rda")
diff2y_acf1D_includeout$variable <- rep(1:1000, 20) 
## y_pacf5
load("data/daily/pdp_ice_daily/y_pacf5D_includeout.rda")
y_pacf5D_includeout$variable <- rep(1:1000, 20)
## diff1y_pacf5
load("data/daily/pdp_ice_daily/diff1y_pacf5D_includeout.rda")
diff1y_pacf5D_includeout$variable <- rep(1:1000, 20) 
## diff2y_pacf5
load("data/daily/pdp_ice_daily/diff2y_pacf5D_includeout.rda")
diff2y_pacf5D_includeout$variable <- rep(1:1000, 20) 
## nonlinearity
load("data/daily/pdp_ice_daily/nonlinearityD_includeout.rda")
nonlinearityD_includeout$variable <- rep(1:1000, 20)
## seas_pacf
load("data/daily/pdp_ice_daily/seas_pacfD_includeout.rda")
seas_pacfD_includeout$variable <- rep(1:1000, 20)
## seasonal_strength1
load("data/daily/pdp_ice_daily/seasonal_strength1D_includeout.rda")
seasonal_strength1D_includeout$variable <- rep(1:1000, 20) 
## seasonal_strength2
load("data/daily/pdp_ice_daily/seasonal_strength2D_includeout.rda")
seasonal_strength2D_includeout$variable <- rep(1:1000, 20)
## sediff_acf1
load("data/daily/pdp_ice_daily/sediff_acf1D_includeout.rda")
sediff_acf1D_includeout$variable <- rep(1:1000, 20)
## sediff_seacf1
load("data/daily/pdp_ice_daily/sediff_seacf1D_includeout.rda")
sediff_seacf1D_includeout$variable <- rep(1:1000, 20)
## sediff_acf5
load("data/daily/pdp_ice_daily/sediff_acf5D_includeout.rda")
sediff_acf5D_includeout$variable <- rep(1:1000, 20)
## N
load("data/daily/pdp_ice_daily/ND_includeout.rda")
ND_includeout$variable <- rep(1:1000, 20)
## y_acf5
load("data/daily/pdp_ice_daily/y_acf5D_includeout.rda")
y_acf5D_includeout$variable <- rep(1:1000, 20)
## diff1y_acf5
load("data/daily/pdp_ice_daily/diff1y_acf5D_includeout.rda")
diff1y_acf5D_includeout$variable <- rep(1:1000, 20)
## diff2y_acf5
load("data/daily/pdp_ice_daily/diff2y_acf5D_includeout.rda")
diff2y_acf5D_includeout$variable <- rep(1:1000, 20)


## snaive
p1 <- ggplot(data = y_pacf5D_includeout, aes_string(x = y_pacf5D_includeout$y_pacf5, y = "snaive")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("y_pacf5") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("snaive")
p2 <- ggplot(data = linearityD_includeout, aes_string(x = linearityD_includeout$linearity, y = "snaive")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p3 <- ggplot(data = trendD_includeout, aes_string(x = trendD_includeout$trend, y = "snaive")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")

## rw
p4 <- ggplot(data = stabilityD_includeout, aes_string(x = stabilityD_includeout$stability, y = "rw")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("rw")
p5 <- ggplot(data = linearityD_includeout, aes_string(x = linearityD_includeout$linearity, y = "rw")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p6 <- ggplot(data = y_pacf5D_includeout, aes_string(x = y_pacf5D_includeout$y_pacf5, y = "rw")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("y_pacf5") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")

## rwd
p7 <- ggplot(data = ND_includeout, aes_string(x = ND_includeout$N, y = "rwd")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("N") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("rwd")
p8 <- ggplot(data = trendD_includeout, aes_string(x = trendD_includeout$trend, y = "rwd")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p9 <- ggplot(data = y_pacf5D_includeout, aes_string(x = y_pacf5D_includeout$y_pacf5, y = "rwd")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("y_pacf5") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")

## mstlarima
p10 <- ggplot(data = stabilityD_includeout, aes_string(x = stabilityD_includeout$stability, y = "mstlarima")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("mstlarima")
p11 <- ggplot(data = trendD_includeout, aes_string(x = trendD_includeout$trend, y = "mstlarima")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p12 <- ggplot(data = y_pacf5D_includeout, aes_string(x = y_pacf5D_includeout$y_pacf5, y = "mstlarima")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("y_pacf5") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")

## mstlets
p13 <- ggplot(data = stabilityD_includeout, aes_string(x = stabilityD_includeout$stability, y = "mstlets")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("mstlets")
p14 <- ggplot(data = linearityD_includeout, aes_string(x = linearityD_includeout$linearity, y = "mstlets")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p15 <- ggplot(data = ND_includeout, aes_string(x = ND_includeout$N, y = "mstlets")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("N") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")

## tbats
p16 <- ggplot(data = diff1y_acf5D_includeout, aes_string(x = diff1y_acf5D_includeout$diff1y_acf5, y = "tbats")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("diff1y_acf5") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("mstlets")
p17 <- ggplot(data = sediff_acf5D_includeout, aes_string(x = sediff_acf5D_includeout$sediff_acf5, y = "tbats")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("sediff_acf5") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p18 <- ggplot(data = sediff_seacf1D_includeout, aes_string(x = sediff_seacf1D_includeout$sediff_seacf1, y = "tbats")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("sediff_seacf1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")

## stlar
p19 <- ggplot(data = ND_includeout, aes_string(x = ND_includeout$N, y = "stlar")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("N") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("theta")
p20 <- ggplot(data = seasonal_strength1D_includeout, aes_string(x = seasonal_strength1D_includeout$seasonal_strength1, y = "stlar")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p21 <- ggplot(data = stabilityD_includeout, aes_string(x = stabilityD_includeout$stability, y = "stlar")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")

## theta
p22 <- ggplot(data = ND_includeout, aes_string(x = ND_includeout$N, y = "theta")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("N") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("theta")
p23 <- ggplot(data = seasonal_strength1D_includeout, aes_string(x = seasonal_strength1D_includeout$seasonal_strength1, y = "theta")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p24 <- ggplot(data = seasonal_strength2D_includeout, aes_string(x = seasonal_strength2D_includeout$seasonal_strength2, y = "theta")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength2") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")

## nn
p25 <- ggplot(data = ND_includeout, aes_string(x = ND_includeout$N, y = "nn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("N") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("nn")
p26 <- ggplot(data = stabilityD_includeout, aes_string(x = stabilityD_includeout$stability, y = "nn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p27 <- ggplot(data = y_pacf5D_includeout, aes_string(x = y_pacf5D_includeout$y_pacf5, y = "nn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength2") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")

## wn
p28 <- ggplot(data = trendD_includeout, aes_string(x = trendD_includeout$trend, y = "wn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("wn")
p29 <- ggplot(data = stabilityD_includeout, aes_string(x = stabilityD_includeout$stability, y = "wn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p30 <- ggplot(data = hurstD_includeout, aes_string(x = hurstD_includeout$hurst, y = "wn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("hurst") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")

(p1|p2|p3)/(p4|p5|p6)/(p7|p8|p9)/(p10|p11|p12)/(p13|p14|p15)/(p16|p17|p18)/(p19|p20|p21)/(p22|p23|p24)/(p25|p26|p27)/(p28|p29|p30)


## ---- pca_daily
load("data/daily/trainD_votes.rda")
pcaDvariables <- daily_training[, 1:26]
pcaM4D <- prcomp(pcaDvariables, center = TRUE, scale = TRUE)
# summary(pcaM4W)
PC1m4d <- pcaM4D$x[, 1]
PC2m4d <- pcaM4D$x[, 2]
PC3m4d <- pcaM4D$x[, 3]
m4dPCAresults1 <- data.frame(PC1 = PC1m4d, PC2 = PC2m4d, PC3 = PC3m4d, pcaDvariables)
m4dPCAresults1$predicted <- trainD_predictions_oob
trainD_votes1 <- data.frame(trainD_votes)
m4dPCAresults <- dplyr::bind_cols(m4dPCAresults1, trainD_votes1)

pca1M4D_rwd <- ggplot(m4dPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4dPCAresults[m4dPCAresults$predicted == "rwd", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "rwd") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4D_rw <- ggplot(m4dPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4dPCAresults[m4dPCAresults$predicted == "rw", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "rw") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4D_wn <- ggplot(m4dPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4dPCAresults[m4dPCAresults$predicted == "wn", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "wn") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4D_theta <- ggplot(m4dPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4dPCAresults[m4dPCAresults$predicted == "theta", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "theta") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4D_nn <- ggplot(m4dPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4dPCAresults[m4dPCAresults$predicted == "nn", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "nn") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4D_tbats <- ggplot(m4dPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4dPCAresults[m4dPCAresults$predicted == "tbats", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "tbats") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4D_mstlets <- ggplot(m4dPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4dPCAresults[m4dPCAresults$predicted == "mstlets", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "mstlets") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4D_mstlarima <- ggplot(m4dPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4dPCAresults[m4dPCAresults$predicted == "mstlarima", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "mstlarima") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))


pca1M4D_stlar <- ggplot(m4dPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4dPCAresults[m4dPCAresults$predicted == "stlar", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "stlar") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))


pca1M4D_snaive <- ggplot(m4dPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4dPCAresults[m4dPCAresults$predicted == "snaive", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "snaive") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4D_snaive+pca1M4D_rwd+pca1M4D_rw + pca1M4D_mstlarima + pca1M4D_mstlets + pca1M4D_tbats+
 pca1M4D_stlar + pca1M4D_theta + pca1M4D_nn+pca1M4D_wn+plot_layout(ncol = 5, nrow = 2)

## ---- friedmandaily
load("data/friedmanHstat_daily.rda")
#friedmanHstat_daily$interaction <- ifelse(friedmanHstat_daily$interaction < 0.5, 0, friedmanHstat_daily$interaction)
## snaive
snaive_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="snaive",]
snaive_DFH_cor <- snaive_DFH %>% select(c("feature1", "feature2", "interaction"))
names(snaive_DFH_cor) <- c("Var1", "Var2", "value")
df1 <- data.frame(Var1=names(table(snaive_DFH_cor$Var1)),
                  Var2=names(table(snaive_DFH_cor$Var1)),
                  value=rep(1.00, 26))

cormat <- dplyr::bind_rows(snaive_DFH_cor, df1)
cormat <- dcast(cormat, Var1 ~ Var2, value.var="value")
colnames(cormat)[1] <- ""
cormat <- data.matrix(cormat)
cormat <- cormat[,-1]
rownames(cormat) <- colnames(cormat)
cormat <- round(cormat,2)
cormat1 <- reorder_cormat(cormat)
p1 <- ggcorrplot(cormat1, hc.order = TRUE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0.5, 1), breaks=seq(0.5,1,100), 
                       low = "#fee8c8", high = "#e34a33",  
                       name="", na.value = "transparent")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+guides(fill=FALSE, color=FALSE)+ggtitle("snaive")


## rw
rw_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="rw",]
rw_DFH_cor <- rw_DFH %>% select(c("feature1", "feature2", "interaction"))
names(rw_DFH_cor) <- c("Var1", "Var2", "value")
df1 <- data.frame(Var1=names(table(rw_DFH_cor$Var1)),
                  Var2=names(table(rw_DFH_cor$Var1)),
                  value=rep(1.00, 26))

cormat <- dplyr::bind_rows(rw_DFH_cor, df1)
cormat <- dcast(cormat, Var1 ~ Var2, value.var="value")
colnames(cormat)[1] <- ""
cormat <- data.matrix(cormat)
cormat <- cormat[,-1]
rownames(cormat) <- colnames(cormat)
cormat <- round(cormat,2)
cormat1 <- reorder_cormat(cormat)
p2 <- ggcorrplot(cormat1, hc.order = TRUE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0.5, 1), breaks=seq(0.5,1,100), 
                       low = "#fee8c8", high = "#e34a33",  
                       name="", na.value = "transparent")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+guides(fill=FALSE, color=FALSE)+ggtitle("rw")

## rwd
rwd_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="rwd",]
rwd_DFH_cor <- rwd_DFH %>% select(c("feature1", "feature2", "interaction"))
names(rwd_DFH_cor) <- c("Var1", "Var2", "value")
df1 <- data.frame(Var1=names(table(rwd_DFH_cor$Var1)),
                  Var2=names(table(rwd_DFH_cor$Var1)),
                  value=rep(1.00, 26))

cormat <- dplyr::bind_rows(rwd_DFH_cor, df1)
cormat <- dcast(cormat, Var1 ~ Var2, value.var="value")
colnames(cormat)[1] <- ""
cormat <- data.matrix(cormat)
cormat <- cormat[,-1]
rownames(cormat) <- colnames(cormat)
cormat <- round(cormat,2)
cormat1 <- reorder_cormat(cormat)
p3 <- ggcorrplot(cormat1, hc.order = TRUE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0.5, 1), breaks=seq(0.5,1,100), 
                       low = "#fee8c8", high = "#e34a33",  
                       name="", na.value = "transparent")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+guides(fill=FALSE, color=FALSE)+ggtitle("rwd")

#mstlarima
mstlarima_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="mstlarima",]
mstlarima_DFH_cor <- mstlarima_DFH %>% select(c("feature1", "feature2", "interaction"))
names(mstlarima_DFH_cor) <- c("Var1", "Var2", "value")
df1 <- data.frame(Var1=names(table(mstlarima_DFH_cor$Var1)),
                  Var2=names(table(mstlarima_DFH_cor$Var1)),
                  value=rep(1.00, 26))

cormat <- dplyr::bind_rows(mstlarima_DFH_cor, df1)
cormat <- dcast(cormat, Var1 ~ Var2, value.var="value")
colnames(cormat)[1] <- ""
cormat <- data.matrix(cormat)
cormat <- cormat[,-1]
rownames(cormat) <- colnames(cormat)
cormat <- round(cormat,2)
cormat1 <- reorder_cormat(cormat)
p4 <- ggcorrplot(cormat1, hc.order = TRUE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0.5, 1), breaks=seq(0.5,1,100), 
                       low = "#fee8c8", high = "#e34a33",  
                       name="", na.value = "transparent")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+guides(fill=FALSE, color=FALSE)+ggtitle("mstlarima")
#mstlets
mstlets_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="mstlets",]
mstlets_DFH_cor <- mstlets_DFH %>% select(c("feature1", "feature2", "interaction"))
names(mstlets_DFH_cor) <- c("Var1", "Var2", "value")
df1 <- data.frame(Var1=names(table(mstlets_DFH_cor$Var1)),
                  Var2=names(table(mstlets_DFH_cor$Var1)),
                  value=rep(1.00, 26))

cormat <- dplyr::bind_rows(mstlets_DFH_cor, df1)
cormat <- dcast(cormat, Var1 ~ Var2, value.var="value")
colnames(cormat)[1] <- ""
cormat <- data.matrix(cormat)
cormat <- cormat[,-1]
rownames(cormat) <- colnames(cormat)
cormat <- round(cormat,2)
cormat1 <- reorder_cormat(cormat)
p5 <- ggcorrplot(cormat1, hc.order = TRUE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0.5, 1), breaks=seq(0.5,1,100), 
                       low = "#fee8c8", high = "#e34a33",  
                       name="", na.value = "transparent")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+guides(fill=FALSE, color=FALSE)+ggtitle("mstlets")

# tbats
tbats_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="tbats",]
tbats_DFH_cor <- tbats_DFH %>% select(c("feature1", "feature2", "interaction"))
names(tbats_DFH_cor) <- c("Var1", "Var2", "value")
df1 <- data.frame(Var1=names(table(tbats_DFH_cor$Var1)),
                  Var2=names(table(tbats_DFH_cor$Var1)),
                  value=rep(1.00, 26))

cormat <- dplyr::bind_rows(tbats_DFH_cor, df1)
cormat <- dcast(cormat, Var1 ~ Var2, value.var="value")
colnames(cormat)[1] <- ""
cormat <- data.matrix(cormat)
cormat <- cormat[,-1]
rownames(cormat) <- colnames(cormat)
cormat <- round(cormat,2)
cormat1 <- reorder_cormat(cormat)
p6 <- ggcorrplot(cormat1, hc.order = TRUE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0.5, 1), breaks=seq(0.5,1,100), 
                       low = "#fee8c8", high = "#e34a33",  
                       name="", na.value = "transparent")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+guides(fill=FALSE, color=FALSE)+ggtitle("tbats")

#stlar
stlar_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="stlar",]
stlar_DFH_cor <- stlar_DFH %>% select(c("feature1", "feature2", "interaction"))
names(stlar_DFH_cor) <- c("Var1", "Var2", "value")
df1 <- data.frame(Var1=names(table(stlar_DFH_cor$Var1)),
                  Var2=names(table(stlar_DFH_cor$Var1)),
                  value=rep(1.00, 26))

cormat <- dplyr::bind_rows(stlar_DFH_cor, df1)
cormat <- dcast(cormat, Var1 ~ Var2, value.var="value")
colnames(cormat)[1] <- ""
cormat <- data.matrix(cormat)
cormat <- cormat[,-1]
rownames(cormat) <- colnames(cormat)
cormat <- round(cormat,2)
cormat1 <- reorder_cormat(cormat)
p7 <- ggcorrplot(cormat1, hc.order = TRUE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0.5, 1), breaks=seq(0.5,1,100), 
                       low = "#fee8c8", high = "#e34a33",  
                       name="", na.value = "transparent")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+guides(fill=FALSE, color=FALSE)+ggtitle("stlar")


#theta
theta_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="theta",]
theta_DFH_cor <- theta_DFH %>% select(c("feature1", "feature2", "interaction"))
names(theta_DFH_cor) <- c("Var1", "Var2", "value")
df1 <- data.frame(Var1=names(table(theta_DFH_cor$Var1)),
                  Var2=names(table(theta_DFH_cor$Var1)),
                  value=rep(1.00, 26))

cormat <- dplyr::bind_rows(theta_DFH_cor, df1)
cormat <- dcast(cormat, Var1 ~ Var2, value.var="value")
colnames(cormat)[1] <- ""
cormat <- data.matrix(cormat)
cormat <- cormat[,-1]
rownames(cormat) <- colnames(cormat)
cormat <- round(cormat,2)
cormat1 <- reorder_cormat(cormat)
p8 <- ggcorrplot(cormat1, hc.order = TRUE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0.5, 1), breaks=seq(0.5,1,100), 
                       low = "#fee8c8", high = "#e34a33",  
                       name="", na.value = "transparent")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+guides(fill=FALSE, color=FALSE)+ggtitle("theta")



#nn
nn_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="nn",]
nn_DFH_cor <- nn_DFH %>% select(c("feature1", "feature2", "interaction"))
names(nn_DFH_cor) <- c("Var1", "Var2", "value")
df1 <- data.frame(Var1=names(table(nn_DFH_cor$Var1)),
                  Var2=names(table(nn_DFH_cor$Var1)),
                  value=rep(1.00, 26))

cormat <- dplyr::bind_rows(nn_DFH_cor, df1)
cormat <- dcast(cormat, Var1 ~ Var2, value.var="value")
colnames(cormat)[1] <- ""
cormat <- data.matrix(cormat)
cormat <- cormat[,-1]
rownames(cormat) <- colnames(cormat)
cormat <- round(cormat,2)
cormat1 <- reorder_cormat(cormat)
p9 <- ggcorrplot(cormat1, hc.order = TRUE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0.5, 1), breaks=seq(0.5,1,100), 
                       low = "#fee8c8", high = "#e34a33",  
                       name="", na.value = "transparent")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+guides(fill=FALSE, color=FALSE)+ggtitle("nn")

#wn
wn_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="wn",]
wn_DFH_cor <- wn_DFH %>% select(c("feature1", "feature2", "interaction"))
names(wn_DFH_cor) <- c("Var1", "Var2", "value")
df1 <- data.frame(Var1=names(table(wn_DFH_cor$Var1)),
                  Var2=names(table(wn_DFH_cor$Var1)),
                  value=rep(1.00, 26))

cormat <- dplyr::bind_rows(wn_DFH_cor, df1)
cormat <- dcast(cormat, Var1 ~ Var2, value.var="value")
colnames(cormat)[1] <- ""
cormat <- data.matrix(cormat)
cormat <- cormat[,-1]
rownames(cormat) <- colnames(cormat)
cormat <- round(cormat,2)
cormat1 <- reorder_cormat(cormat)
p10 <- ggcorrplot(cormat1, hc.order = TRUE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0.5, 1), breaks=seq(0.5,1,100), 
                       low = "#fee8c8", high = "#e34a33",  
                       name="", na.value = "transparent")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+guides(fill=FALSE, color=FALSE)+ggtitle("wn")

p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+plot_layout(ncol = 3, nrow = 4)

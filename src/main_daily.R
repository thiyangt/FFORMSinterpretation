#################################################################
#                  Daily data                                  #
#################################################################
load("data/daily/daily_training.rda") # random forest training set
## ---- vidaily
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
  scale_fill_manual(breaks=c("0","1"), values=c("black","red"), guide="none")+
  theme(text=element_text(size = 20))
feaImp_daily

## ---- dailypdp
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
p1 <- ggplot(data = seasonal_strength1D_includeout, aes_string(x = seasonal_strength1D_includeout$seasonal_strength1, y = "snaive")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("snaive")
p2 <- ggplot(data = trendD_includeout, aes_string(x = trendD_includeout$trend, y = "snaive")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
p3 <- ggplot(data = stabilityD_includeout, aes_string(x = stabilityD_includeout$stability, y = "snaive")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
pss1 <- ggplot(data = seasonal_strength2D_includeout, aes_string(x = seasonal_strength2D_includeout$seasonal_strength2, y = "snaive")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength2") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
pn1 <- ggplot(data = ND_includeout, aes_string(x = ND_includeout$N, y = "snaive")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("N") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
## rw
p4 <- ggplot(data = seasonal_strength1D_includeout, aes_string(x = seasonal_strength1D_includeout$seasonal_strength1, y = "rw")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("rw")
p5 <- ggplot(data = trendD_includeout, aes_string(x = trendD_includeout$trend, y = "rw")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
p6 <- ggplot(data = stabilityD_includeout, aes_string(x = stabilityD_includeout$stability, y = "rw")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
pss2 <- ggplot(data = seasonal_strength2D_includeout, aes_string(x = seasonal_strength2D_includeout$seasonal_strength2, y = "snaive")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength2") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
pn2 <- ggplot(data = ND_includeout, aes_string(x = ND_includeout$N, y = "rw")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("N") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")

## rwd
p7 <- ggplot(data = seasonal_strength1D_includeout, aes_string(x = seasonal_strength1D_includeout$seasonal_strength1, y = "rwd")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("rwd")
p8 <- ggplot(data = trendD_includeout, aes_string(x = trendD_includeout$trend, y = "rwd")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
p9 <- ggplot(data = stabilityD_includeout, aes_string(x = stabilityD_includeout$stability, y = "rwd")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
pss3 <- ggplot(data = seasonal_strength2D_includeout, aes_string(x = seasonal_strength2D_includeout$seasonal_strength2, y = "rwd")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength2") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
pn3 <- ggplot(data = ND_includeout, aes_string(x = ND_includeout$N, y = "rwd")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("N") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")

## mstlarima
p10 <- ggplot(data = seasonal_strength1D_includeout, aes_string(x = seasonal_strength1D_includeout$seasonal_strength1, y = "mstlarima")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("mstlarima")
p11 <- ggplot(data = trendD_includeout, aes_string(x = trendD_includeout$trend, y = "mstlarima")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
p12 <- ggplot(data = stabilityD_includeout, aes_string(x = stabilityD_includeout$stability, y = "mstlarima")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
pss4 <- ggplot(data = seasonal_strength2D_includeout, aes_string(x = seasonal_strength2D_includeout$seasonal_strength2, y = "mstlarima")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength2") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
pn4 <- ggplot(data = ND_includeout, aes_string(x = ND_includeout$N, y = "mstlarima")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("N") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
## mstlets
p13 <- ggplot(data = seasonal_strength1D_includeout, aes_string(x = seasonal_strength1D_includeout$seasonal_strength1, y = "mstlets")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("mstlets")
p14 <- ggplot(data = trendD_includeout, aes_string(x = trendD_includeout$trend, y = "mstlets")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
p15 <- ggplot(data = stabilityD_includeout, aes_string(x = stabilityD_includeout$stability, y = "mstlets")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
pss5 <- ggplot(data = seasonal_strength2D_includeout, aes_string(x = seasonal_strength2D_includeout$seasonal_strength2, y = "mstlets")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength2") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
pn5 <- ggplot(data = ND_includeout, aes_string(x = ND_includeout$N, y = "mstlets")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("N") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")

## tbats
p16 <- ggplot(data = seasonal_strength1D_includeout, aes_string(x = seasonal_strength1D_includeout$seasonal_strength1, y = "tbats")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("tbats")
p17 <- ggplot(data = trendD_includeout, aes_string(x = trendD_includeout$trend, y = "tbats")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
p18 <- ggplot(data = stabilityD_includeout, aes_string(x = stabilityD_includeout$stability, y = "tbats")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
pss6 <- ggplot(data = seasonal_strength2D_includeout, aes_string(x = seasonal_strength2D_includeout$seasonal_strength2, y = "tbats")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength2") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
pn6 <- ggplot(data = ND_includeout, aes_string(x = ND_includeout$N, y = "tbats")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("N") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")

## stlar
p19 <- ggplot(data = seasonal_strength1D_includeout, aes_string(x = seasonal_strength1D_includeout$seasonal_strength1, y = "stlar")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("stlar")
p20 <- ggplot(data = trendD_includeout, aes_string(x = trendD_includeout$trend, y = "stlar")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
p21 <- ggplot(data = stabilityD_includeout, aes_string(x = stabilityD_includeout$stability, y = "stlar")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
pss7 <- ggplot(data = seasonal_strength2D_includeout, aes_string(x = seasonal_strength2D_includeout$seasonal_strength2, y = "stlar")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength2") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
pn7 <- ggplot(data = ND_includeout, aes_string(x = ND_includeout$N, y = "stlar")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("N") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")

## theta
p22 <- ggplot(data = seasonal_strength1D_includeout, aes_string(x = seasonal_strength1D_includeout$seasonal_strength1, y = "theta")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("theta")
p23 <- ggplot(data = trendD_includeout, aes_string(x = trendD_includeout$trend, y = "theta")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
p24 <- ggplot(data = stabilityD_includeout, aes_string(x = stabilityD_includeout$stability, y = "theta")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
pss8 <- ggplot(data = seasonal_strength2D_includeout, aes_string(x = seasonal_strength2D_includeout$seasonal_strength2, y = "theta")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength2") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
pn8 <- ggplot(data = ND_includeout, aes_string(x = ND_includeout$N, y = "theta")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("N") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")

## nn
p25 <- ggplot(data = seasonal_strength1D_includeout, aes_string(x = seasonal_strength1D_includeout$seasonal_strength1, y = "nn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("nn")
p26 <- ggplot(data = trendD_includeout, aes_string(x = trendD_includeout$trend, y = "nn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
p27 <- ggplot(data = stabilityD_includeout, aes_string(x = stabilityD_includeout$stability, y = "nn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
pss9 <- ggplot(data = seasonal_strength2D_includeout, aes_string(x = seasonal_strength2D_includeout$seasonal_strength2, y = "nn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength2") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
pn9 <- ggplot(data = ND_includeout, aes_string(x = ND_includeout$N, y = "nn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("N") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")

## wn
p28 <- ggplot(data = seasonal_strength1D_includeout, aes_string(x = seasonal_strength1D_includeout$seasonal_strength1, y = "wn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("wn")
p29 <- ggplot(data = trendD_includeout, aes_string(x = trendD_includeout$trend, y = "wn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
p30 <- ggplot(data = stabilityD_includeout, aes_string(x = stabilityD_includeout$stability, y = "wn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
pss10 <- ggplot(data = seasonal_strength2D_includeout, aes_string(x = seasonal_strength2D_includeout$seasonal_strength2, y = "wn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_strength2") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")
pn10 <- ggplot(data = ND_includeout, aes_string(x = ND_includeout$N, y = "wn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("N") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none",text = element_text(size=20)) + ylab("")

(p1|pss1|p2|p3|pn1)/(p4|pss2|p5|p6|pn2)/(p7|pss3|p8|p9|pn3)/(p10|pss4|p11|p12|pn4)/(p13|pss5|p14|p15|pn5)/(p16|pss6|p17|p18|pn6)/(p19|pss7|p20|p21|pn7)/(p22|pss8|p23|p24|pn8)/(p25|pss9|p26|p27|pn9)/(p28|pss10|p29|p30|pn10)


## ---- pcadaily
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
col.order <- c("seasonal_strength1", "stability", "trend", "lumpiness",
               "linearity", "nonlinearity", "y_pacf5", "curvature", "e_acf1",
               "spikiness", "sediff_seacf1","seas_pacf", "N", "sediff_acf5",
               "entropy", "y_acf5", "y_acf1", "diff1y_acf5", "sediff_acf1",
               "diff1y_pacf5", "diff1y_acf1", "hurst", "seasonal_strength2",
               "diff2y_acf1", "diff2y_acf5", "diff2y_pacf5")
#friedmanHstat_daily$interaction <- ifelse(friedmanHstat_daily$interaction < 0.5, 0, friedmanHstat_daily$interaction)
## snaive
snaive_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="snaive",]
snaive_DFH_cormat <- friedmanHstat_matrix(snaive_DFH, 26, rev(col.order))
p1 <- ggcorrplot(snaive_DFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("snaive")

## rw
rw_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="rw",]
rw_DFH_cormat <- friedmanHstat_matrix(rw_DFH, 26, rev(col.order))
p2 <- ggcorrplot(rw_DFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("rw")
## rwd
rwd_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="rwd",]
rwd_DFH_cormat <- friedmanHstat_matrix(rwd_DFH, 26, rev(col.order))
p3 <- ggcorrplot(rwd_DFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("rwd")

#mstlarima
mstlarima_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="mstlarima",]
mstlarima_DFH_cormat <- friedmanHstat_matrix(mstlarima_DFH, 26, rev(col.order))
p4 <- ggcorrplot(mstlarima_DFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("mstlarima")

#mstlets
mstlets_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="mstlets",]
mstlets_DFH_cormat <- friedmanHstat_matrix(mstlets_DFH, 26, rev(col.order))
p5 <- ggcorrplot(mstlets_DFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("mstlets")
# tbats
tbats_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="tbats",]
tbats_DFH_cormat <- friedmanHstat_matrix(tbats_DFH, 26, rev(col.order))
p6 <- ggcorrplot(tbats_DFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("tbats")

#stlar
stlar_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="stlar",]
stlar_DFH_cormat <- friedmanHstat_matrix(stlar_DFH, 26, rev(col.order))
p7 <- ggcorrplot(stlar_DFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("stlar")


#theta
theta_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="theta",]
theta_DFH_cormat <- friedmanHstat_matrix(theta_DFH, 26, rev(col.order))
p8 <- ggcorrplot(theta_DFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("theta")


#nn
nn_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="nn",]
nn_DFH_cormat <- friedmanHstat_matrix(nn_DFH, 26, rev(col.order))
p9 <- ggcorrplot(nn_DFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("nn")

#wn
wn_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="wn",]
wn_DFH_cormat <- friedmanHstat_matrix(wn_DFH, 26, rev(col.order))
p10 <- ggcorrplot(wn_DFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("wn")

p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+plot_layout(ncol = 3, nrow = 4)

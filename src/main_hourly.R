################################################################
#              Hourly data                                     #
################################################################
## ---- vihourly
# All variable scores into one dataframe
load("data/hourly/trainH_importance.rda")
load(file = "data/hourly/sd_pdf_dfH.rda")
load(file = "data/hourly/sd_ice_dfH.rda")
## Permutation based
train_imp_dfH <- data.frame(trainH_importance)
train_imp_dfH <- add_rownames(train_imp_dfH, "Feature")
train_imp_dfH <- within(train_imp_dfH, rm("MeanDecreaseAccuracy", "MeanDecreaseGini"))
permutation_impH <- train_imp_dfH %>% melt(id.vars = "Feature")
#dim(permutation_impD) # 260 3
colnames(permutation_impH) <- c("feature", "class", "score")

## PDP-based
sd_pdf_dfH <- add_rownames(sd_pdf_dfH, "class")
pdp_imp <- sd_pdf_dfH %>% melt(id.vars = "class")
colnames(pdp_imp) <- c("class", "feature", "score")

## ICE-based
sd_ice_dfH <- add_rownames(sd_ice_dfH, "class")
ice_imp <- sd_ice_dfH %>% melt(id.vars = "class")
colnames(ice_imp) <- c("class", "feature", "score")

## Combine the data frames
importancescoreH <- bind_rows(permutation_impH, pdp_imp)
importancescoreH <- bind_rows(importancescoreH, ice_imp)
importancescoreH$VI <- rep(c("permutation", "PDP", "ICE"), each = 260)

## rank permutation, sd_pdp, and sd_ice scores for each class
importancescoreH$class <- factor(importancescoreH$class,
                                 levels = c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                            "theta","nn","wn"),
                                 labels = c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                            "theta","nn","wn"))

rank_vi_hourly_classes <- importancescoreH %>%
  group_by(VI, class) %>%
  mutate(rank = min_rank(score))

## compute mean rank
meanrank_vih_classes <- rank_vi_hourly_classes %>% group_by(feature, class) %>% summarise_at(vars(c(rank)), funs(mean))

## overall importance of features to the forest
train_impforestH <- data.frame(trainH_importance)
train_impforestH <- add_rownames(train_impforestH, "Feature")
train_impforestH <- train_impforestH[, c("Feature", "MeanDecreaseAccuracy", "MeanDecreaseGini")]
train_impforestH <- train_impforestH %>%
  mutate(rank_permu = min_rank(MeanDecreaseAccuracy)) %>%
  mutate(rank_gini = min_rank(MeanDecreaseGini))
train_impforestH$mean_rank <- (train_impforestH$rank_permu + train_impforestH$rank_gini) / 2
meanrank_vih_forest <- data.frame(
  feature = train_impforestH$Feature,
  class = rep("overall", 26),
  rank = train_impforestH$mean_rank
)
## combine mean ranks for overall forest and separate classes
meanrank_hourly <- dplyr::bind_rows(meanrank_vih_forest, meanrank_vih_classes)
## create horizontal bar chart for ranks
orderOverall <- filter(meanrank_hourly, class == "overall")
meanrank_hourly$feature <- factor(meanrank_hourly$feature, levels = orderOverall$feature[order(orderOverall$rank)])
meanrank_hourly$class <- factor(meanrank_hourly$class,
                               levels = c(
                                 "overall","snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                 "theta","nn","wn"
                               ),
                               labels = c(
                                 "overall","snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                 "theta","nn","wn"
                               )
)

meanrank_hourly$rn <- 1:286
topq <- meanrank_hourly %>%
  group_by(class) %>%
  top_n(n = 5, wt = rank)
meanrank_hourly$istop <- ifelse(meanrank_hourly$rn %in% topq$rn, TRUE, FALSE)
feaImp_hourly <- ggplot(meanrank_hourly, aes(y = rank, x = feature,fill=as.factor(istop))) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~class, ncol = 6, nrow = 2) +
  coord_flip() + ylab("Average rank")+ 
  scale_fill_manual(breaks=c("0","1"), values=c("black","red"), guide="none")+
  theme(text=element_text(size = 20))
feaImp_hourly

## ----hourlypdp
load("data/hourly/hiceout/curvaturegridH.rda")
curvaturegridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/diff1y_acf1gridH.rda")
diff1y_acf1gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/diff1y_acf5gridH.rda")
diff1y_acf5gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/diff1y_pacf5gridH.rda")
diff1y_pacf5gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/diff2y_acf1gridH.rda")
diff2y_acf1gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/diff2y_acf5gridH.rda")
diff2y_acf5gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/diff2y_pacf5gridH.rda")
diff2y_pacf5gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/e_acf1gridH.rda")
e_acf1gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/entropygridH.rda")
entropygridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/hurstgridH.rda")
hurstgridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/linearitygridH.rda")
linearitygridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/lumpinessgridH.rda")
lumpinessgridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/NgridH.rda")
NgridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/nonlinearitygridH.rda")
nonlinearitygridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/seas_pacfgridH.rda")
seas_pacfgridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/seasonality1gridH.rda")
seasonality1gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/seasonality2gridH.rda")
seasonality2gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/sediff_acf1gridH.rda")
sediff_acf1gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/sediff_acf5gridH.rda")
sediff_acf5gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/sediff_seacf1gridH.rda")
sediff_seacf1gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/spikinessgridH.rda")
spikinessgridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/stabilitygridH.rda")
stabilitygridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/trendgridH.rda")
trendgridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/y_acf1gridH.rda")
y_acf1gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/y_acf5gridH.rda")
y_acf5gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/y_pacf5gridH.rda")
y_pacf5gridH$variable <- rep(1:1000, 20)


# snaive
p1 <- ggplot(data=seasonality1gridH, aes_string(x=seasonality1gridH$seasonal_strength1, y="snaive")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonal_strength1")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position="none",text = element_text(size=20))+ylab("snaive")
p2 <- ggplot(data= entropygridH, aes_string(x=entropygridH$entropy, y="snaive")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("entropy")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +theme(legend.position="none",text = element_text(size=20))+ylab("")
p3 <- ggplot(data=linearitygridH, aes_string(x=linearitygridH$linearity, y="snaive")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("linearity")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
ps1 <- ggplot(data=seasonality2gridH, aes_string(x=seasonality2gridH$seasonal_strength2, y="snaive")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_strength2")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
pc1 <- ggplot(data=curvaturegridH, aes_string(x=curvaturegridH$curvature, y="snaive")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("curvature")+ theme(legend.position="none",text = element_text(size=20))+ylab("")


# rw
p4 <- ggplot(data=seasonality1gridH, aes_string(x=seasonality1gridH$seasonal_strength1, y="rw")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_strength1")+ theme(legend.position="none",text = element_text(size=20))+ylab("rw")
p5 <-  ggplot(data=entropygridH, aes_string(x=entropygridH$entropy, y="rw")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("entropy")+ theme(legend.position="none",text = element_text(size=20))+ylab("rw")
p6 <- ggplot(data=linearitygridH, aes_string(x=linearitygridH$linearity, y="rw")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("linearity")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
ps2 <- ggplot(data=seasonality2gridH, aes_string(x=seasonality2gridH$seasonal_strength2, y="rw")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_strength2")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
pc2 <- ggplot(data=curvaturegridH, aes_string(x=curvaturegridH$curvature, y="rw")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("curvature")+ theme(legend.position="none",text = element_text(size=20))+ylab("")

#rwd
p7 <- ggplot(data=seasonality1gridH, aes_string(x=seasonality1gridH$seasonal_strength1, y="rwd")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_strength1")+ theme(legend.position="none",text = element_text(size=20))+ylab("rwd")
p8 <- ggplot(data=entropygridH, aes_string(x=entropygridH$entropy, y="rwd")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("entropy")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
p9 <- ggplot(data=linearitygridH, aes_string(x=linearitygridH$linearity, y="rwd")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("linearity")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
ps3 <- ggplot(data=seasonality2gridH, aes_string(x=seasonality2gridH$seasonal_strength2, y="rwd")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_strength2")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
pc3 <- ggplot(data=curvaturegridH, aes_string(x=curvaturegridH$curvature, y="rwd")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("curvature")+ theme(legend.position="none",text = element_text(size=20))+ylab("")

#mstlarima
p10 <- ggplot(data=seasonality1gridH, aes_string(x=seasonality1gridH$seasonal_strength1, y="mstlarima")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_strength1")+ theme(legend.position="none",text = element_text(size=20))+ylab("mstlarima")
p11 <- ggplot(data= entropygridH, aes_string(x=entropygridH$entropy, y="mstlarima")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("entropy")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
p12 <- ggplot(data=linearitygridH, aes_string(x=linearitygridH$linearity, y="mstlarima")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("linearity")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
ps4 <- ggplot(data=seasonality2gridH, aes_string(x=seasonality2gridH$seasonal_strength2, y="mstlarima")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_strength2")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
pc4 <- ggplot(data=curvaturegridH, aes_string(x=curvaturegridH$curvature, y="mstlarima")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("curvature")+ theme(legend.position="none",text = element_text(size=20))+ylab("")

# mstlets
p13 <- ggplot(data=seasonality1gridH, aes_string(x=seasonality1gridH$seasonal_strength1, y="mstlets")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_strength1")+ theme(legend.position="none",text = element_text(size=20))+ylab("mstlets")
p14 <- ggplot(data= entropygridH, aes_string(x=entropygridH$entropy, y="mstlets")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("entropy")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
p15 <- ggplot(data=linearitygridH, aes_string(x=linearitygridH$linearity, y="mstlets")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("linearity")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
ps5 <- ggplot(data=seasonality2gridH, aes_string(x=seasonality2gridH$seasonal_strength2, y="mstlets")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_strength2")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
pc5 <- ggplot(data=curvaturegridH, aes_string(x=curvaturegridH$curvature, y="mstlets")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("curvature")+ theme(legend.position="none",text = element_text(size=20))+ylab("")

#tbats
p16 <- ggplot(data=seasonality1gridH, aes_string(x=seasonality1gridH$seasonal_strength1, y="tbats")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_strength1")+ theme(legend.position="none",text = element_text(size=20))+ylab("tbats")
p17 <- ggplot(data= entropygridH, aes_string(x=entropygridH$entropy, y="tbats")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("entropy")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
p18 <- ggplot(data=linearitygridH, aes_string(x=linearitygridH$linearity, y="tbats")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("linearity")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
ps6 <- ggplot(data=seasonality2gridH, aes_string(x=seasonality2gridH$seasonal_strength2, y="tbats")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_strength2")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
pc6 <- ggplot(data=curvaturegridH, aes_string(x=curvaturegridH$curvature, y="tbats")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("curvature")+ theme(legend.position="none",text = element_text(size=20))+ylab("")


#stlar
p19 <- ggplot(data=seasonality1gridH, aes_string(x=seasonality1gridH$seasonal_strength1, y="stlar")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_strength1")+ theme(legend.position="none",text = element_text(size=20))+ylab("stlar")
p20 <- ggplot(data= entropygridH, aes_string(x=entropygridH$entropy, y="stlar")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("entropy")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
p21 <- ggplot(data=linearitygridH, aes_string(x=linearitygridH$linearity, y="stlar")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("linearity")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
ps7 <- ggplot(data=seasonality2gridH, aes_string(x=seasonality2gridH$seasonal_strength2, y="stlar")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_strength2")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
pc7 <- ggplot(data=curvaturegridH, aes_string(x=curvaturegridH$curvature, y="stlar")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("curvature")+ theme(legend.position="none",text = element_text(size=20))+ylab("")


# theta
p22 <- ggplot(data=seasonality1gridH, aes_string(x=seasonality1gridH$seasonal_strength1, y="theta")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_strength1")+ theme(legend.position="none",text = element_text(size=20))+ylab("theta")
p23 <- ggplot(data= entropygridH, aes_string(x=entropygridH$entropy, y="theta")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("entropy")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
p24 <- ggplot(data=linearitygridH, aes_string(x=linearitygridH$linearity, y="theta")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("linearity")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
ps8 <- ggplot(data=seasonality2gridH, aes_string(x=seasonality2gridH$seasonal_strength2, y="theta")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_strength2")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
pc8 <- ggplot(data=curvaturegridH, aes_string(x=curvaturegridH$curvature, y="theta")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("curvature")+ theme(legend.position="none",text = element_text(size=20))+ylab("")

## nn
p25 <- ggplot(data=seasonality1gridH, aes_string(x=seasonality1gridH$seasonal_strength1, y="nn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_strength1")+ theme(legend.position="none",text = element_text(size=20))+ylab("nn")
p26 <- ggplot(data= entropygridH, aes_string(x=entropygridH$entropy, y="nn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("entropy")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
p27 <- ggplot(data= linearitygridH, aes_string(x=linearitygridH$linearity, y="nn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("linearity")+ theme(legend.position="none",text = element_text(size=20))+ylab("") 
ps9 <- ggplot(data=seasonality2gridH, aes_string(x=seasonality2gridH$seasonal_strength2, y="nn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_strength2")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
pc9 <- ggplot(data=curvaturegridH, aes_string(x=curvaturegridH$curvature, y="nn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("curvature")+ theme(legend.position="none",text = element_text(size=20))+ylab("")

## wn
p28 <- ggplot(data=seasonality1gridH, aes_string(x=seasonality1gridH$seasonal_strength1, y="wn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_strength1")+ theme(legend.position="none",text = element_text(size=20))+ylab("wn")
p29 <- ggplot(data=entropygridH, aes_string(x=entropygridH$entropy, y="wn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("entropy")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
p30 <- ggplot(data=linearitygridH, aes_string(x=linearitygridH$linearity, y="wn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("linearity")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
ps10 <- ggplot(data=seasonality2gridH, aes_string(x=seasonality2gridH$seasonal_strength2, y="wn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_strength2")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
pc10 <- ggplot(data=curvaturegridH, aes_string(x=curvaturegridH$curvature, y="wn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("curvature")+ theme(legend.position="none",text = element_text(size=20))+ylab("")


(p1|ps1|p2|p3|pc1)/(p4|ps2|p5|p6|pc2)/(p7|ps3|p8|p9|pc3)/(p10|ps4|p11|p12|pc4)/(p13|ps5|p14|p15|pc5)/(p16|ps6|p17|p18|pc6)/(p19|ps7|p20|p21|pc7)/(p22|ps8|p23|p24|pc8)/(p25|ps9|p26|p27|pc9)/(p28|ps10|p29|p30|pc10)



## ---- pcahourly
load("data/hourly/trainH_votes.rda")
pcaHvariables <- hourly_training[, 1:26]
pcaM4H <- prcomp(pcaHvariables, center = TRUE, scale = TRUE)
# summary(pcaM4W)
PC1m4h <- pcaM4H$x[, 1]
PC2m4h <- pcaM4H$x[, 2]
PC3m4h <- pcaM4H$x[, 3]
m4hPCAresults1 <- data.frame(PC1 = PC1m4h, PC2 = PC2m4h, PC3 = PC3m4h, pcaHvariables)
m4hPCAresults1$predicted <- trainH_predictions_oob
trainH_votes1 <- data.frame(trainH_votes)
m4hPCAresults <- dplyr::bind_cols(m4hPCAresults1, trainH_votes1)

pca1M4H_rwd <- ggplot(m4hPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4hPCAresults[m4hPCAresults$predicted == "rwd", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "rwd") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4H_rw <- ggplot(m4hPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4hPCAresults[m4hPCAresults$predicted == "rw", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "rw") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4H_wn <- ggplot(m4hPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4hPCAresults[m4hPCAresults$predicted == "wn", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "wn") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4H_theta <- ggplot(m4hPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4hPCAresults[m4hPCAresults$predicted == "theta", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "theta") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4H_nn <- ggplot(m4hPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4hPCAresults[m4hPCAresults$predicted == "nn", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "nn") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4H_tbats <- ggplot(m4hPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4hPCAresults[m4hPCAresults$predicted == "tbats", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "tbats") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4H_mstlets <- ggplot(m4hPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4hPCAresults[m4hPCAresults$predicted == "mstlets", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "mstlets") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4H_mstlarima <- ggplot(m4hPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4hPCAresults[m4hPCAresults$predicted == "mstlarima", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "mstlarima") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))


pca1M4H_stlar <- ggplot(m4hPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4hPCAresults[m4hPCAresults$predicted == "stlar", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "stlar") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))


pca1M4H_snaive <- ggplot(m4hPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4hPCAresults[m4hPCAresults$predicted == "snaive", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "snaive") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4H_snaive+pca1M4H_rwd+pca1M4H_rw +pca1M4H_mstlarima+pca1M4H_mstlets + pca1M4H_tbats+
pca1M4H_stlar + pca1M4H_theta + pca1M4H_nn+pca1M4H_wn+plot_layout(ncol = 5, nrow = 2)


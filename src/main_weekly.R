#################################################################
#                  Weekly data                                  #
#################################################################
## ---- oobweekly
load("data/weekly/trainW_votes.rda") # oob votes from the random forest
load("data/weekly/trainW_predictions_oob.rda") # based on oob prediction
load("data/weekly/weekly_training.rda") # random forest training set
votes_oobW <- data.frame(trainW_votes)
names(votes_oobW) <- names(table(trainW_predictions_oob))
votes_oobW$predicted <- trainW_predictions_oob
votes_oobW$classlabel <- weekly_training$classlabels
votes_oobW <- votes_oobW %>%
  mutate(id = seq_len(n())) %>%
  melt(id.var = c("classlabel", "id", "predicted"), na.rm = T) %>%
  select(-id)
# new addition to arrange labels
votes_oobW$classlabel <- factor(votes_oobW$classlabel, levels = rev(c(
  "snaive", "rwd", "rw",
  "ARIMA", "ARMA/AR/MA",  "SARIMA","stlar", "mstlets","tbats", "wn", "theta", "nn"
)))
oob_boxplot_weekly <- ggplot(votes_oobW, aes(x = variable, y = value, fill = classlabel)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.4) +
  ylab("Proportion") +
  xlab("") +
  theme(legend.position = "right", legend.title = element_blank(), legend.text.align = 0, text = element_text(size = 20)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = rev(c(
    "snaive", "rwd", "rw", 
    "ARIMA", "ARMA/AR/MA",  "SARIMA","stlar", "mstlets","tbats", "wn", "theta", "nn"
  ))) +
  coord_flip()
oob_boxplot_weekly

## ---- viweekly
# All variable scores into one dataframe
load("data/weekly/trainW_importance.rda")
load(file = "data/weekly/sd_pdf_dfW.rda")
load(file = "data/weekly/sd_ice_dfW.rda")
## Permutation based
train_imp_dfW <- data.frame(trainW_importance)
train_imp_dfW <- add_rownames(train_imp_dfW, "Feature")
train_imp_dfW <- within(train_imp_dfW, rm("MeanDecreaseAccuracy", "MeanDecreaseGini"))
permutation_impW <- train_imp_dfW %>% melt(id.vars = "Feature")
#dim(permutation_impW) # 324 3
colnames(permutation_impW) <- c("feature", "class", "score")

## PDP-based
sd_pdf_dfW <- add_rownames(sd_pdf_dfW, "class")
pdp_imp <- sd_pdf_dfW %>% melt(id.vars = "class")
colnames(pdp_imp) <- c("class", "feature", "score")

## ICE-based
sd_ice_dfW <- add_rownames(sd_ice_dfW, "class")
ice_imp <- sd_ice_dfW %>% melt(id.vars = "class")
colnames(ice_imp) <- c("class", "feature", "score")

## Combine the data frames
importancescoreW <- bind_rows(permutation_impW, pdp_imp)
importancescoreW <- bind_rows(importancescoreW, ice_imp)
importancescoreW$VI <- rep(c("permutation", "PDP", "ICE"), each = 324)

## rank permutation, sd_pdp, and sd_ice scores for each class
importancescoreW$class <- factor(importancescoreW$class,
                                 levels = c(
                                   "snaive", "rwd", "rw", 
                                   "ARIMA", "SARIMA", "stlar","mstlets", "tbats","ARMA.AR.MA", "wn", "theta", "nn"
                                 ),
                                 labels = c(
                                   "snaive", "rwd", "rw", 
                                   "ARIMA", "SARIMA", "stlar","mstlets", "tbats","ARMA.AR.MA", "wn", "theta", "nn"
                                 )
)

rank_vi_weekly_classes <- importancescoreW %>%
  group_by(VI, class) %>%
  mutate(rank = min_rank(score))

## compute mean rank
meanrank_viw_classes <- rank_vi_weekly_classes %>% group_by(feature, class) %>% summarise_at(vars(c(rank)), funs(mean))

## overall importance of features to the forest
train_impforestW <- data.frame(trainW_importance)
train_impforestW <- add_rownames(train_impforestW, "Feature")
train_impforestW <- train_impforestW[, c("Feature", "MeanDecreaseAccuracy", "MeanDecreaseGini")]
train_impforestW <- train_impforestW %>%
  mutate(rank_permu = min_rank(MeanDecreaseAccuracy)) %>%
  mutate(rank_gini = min_rank(MeanDecreaseGini))
train_impforestW$mean_rank <- (train_impforestW$rank_permu + train_impforestW$rank_gini) / 2
meanrank_viw_forest <- data.frame(
  feature = train_impforestW$Feature,
  class = rep("overall", 27),
  rank = train_impforestW$mean_rank
)
## combine mean ranks for overall forest and separate classes
meanrank_weekly <- dplyr::bind_rows(meanrank_viw_forest, meanrank_viw_classes)
## create horizontal bar chart for ranks
orderOverall <- filter(meanrank_weekly, class == "overall")
meanrank_weekly$feature <- factor(meanrank_weekly$feature, levels = orderOverall$feature[order(orderOverall$rank)])
meanrank_weekly$class <- factor(meanrank_weekly$class,
                                   levels = c(
                                     "overall",  "snaive", "rwd", "rw", 
                                     "ARIMA", "SARIMA", "stlar","mstlets", "tbats","ARMA.AR.MA", "wn", "theta", "nn"
                                   ),
                                   labels = c(
                                     "overall",  "snaive", "rwd", "rw", 
                                     "ARIMA", "SARIMA", "stlar","mstlets", "tbats","ARMA.AR.MA", "wn", "theta", "nn"
                                   )
)

meanrank_weekly$rn <- 1:351
topq <- meanrank_weekly %>%
  group_by(class) %>%
  top_n(n = 5, wt = rank)
meanrank_weekly$istop <- ifelse(meanrank_weekly$rn %in% topq$rn, TRUE, FALSE)
feaImp_weekly <- ggplot(meanrank_weekly, aes(y = rank, x = feature,fill=as.factor(istop))) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~class, ncol = 7, nrow = 2) +
  coord_flip() + ylab("Average rank")+ 
  scale_fill_manual(breaks=c("0","1"), values=c("black","red"), guide="none")+
  theme(text=element_text(size = 20))
feaImp_weekly

## ---- weeklypdp
## load ICE calculation files
## entropy
load("data/weekly/pdp_ice/entropyW_includeout.rda")
entropyW_includeout$variable <- rep(1:1111, 20)
## lumpiness
load("data/weekly/pdp_ice/lumpinessW_includeout.rda")
lumpinessW_includeout$variable <- rep(1:1111, 20)
## stability
load("data/weekly/pdp_ice/stabilityW_includeout.rda")
stabilityW_includeout$variable <- rep(1:1111, 20)
## hurst
load("data/weekly/pdp_ice/hurstW_includeout.rda")
hurstW_includeout$variable <- rep(1:1111, 20)
## trend
load("data/weekly/pdp_ice/trendW_includeout.rda")
trendW_includeout$variable <- rep(1:1111, 20)
## spikines
load("data/weekly/pdp_ice/spikinessW_includeout.rda")
spikinessW_includeout$variable <- rep(1:1111, 20)
## linearity
load("data/weekly/pdp_ice/linearityW_includeout.rda")
linearityW_includeout$variable <- rep(1:1111, 20)
## curvature
load("data/weekly/pdp_ice/curvatureW_includeout.rda")
curvatureW_includeout$variable <- rep(1:1111, 20)
## e_acf1
load("data/weekly/pdp_ice/e_acf1W_includeout.rda")
e_acf1W_includeout$variable <- rep(1:1111, 20)
## y_acf1
load("data/weekly/pdp_ice/y_acf1W_includeout.rda")
y_acf1W_includeout$variable <- rep(1:1111, 20)
## diff1y_acf1
load("data/weekly/pdp_ice/diff1y_acf1W_includeout.rda")
diff1y_acf1W_includeout$variable <- rep(1:1111, 20)
## diff2y_acf1
load("data/weekly/pdp_ice/diff2y_acf1W_includeout.rda")
diff2y_acf1W_includeout$variable <- rep(1:1111, 20)
## y_pacf5
load("data/weekly/pdp_ice/y_pacf5W_includeout.rda")
y_pacf5W_includeout$variable <- rep(1:1111, 20)
## diff1y_pacf5
load("data/weekly/pdp_ice/diff1y_pacf5W_includeout.rda")
diff1y_pacf5W_includeout$variable <- rep(1:1111, 20)
## diff2y_pacf5
load("data/weekly/pdp_ice/diff2y_pacf5W_includeout.rda")
diff2y_pacf5W_includeout$variable <- rep(1:1111, 20)
## nonlinearity
load("data/weekly/pdp_ice/nonlinearityW_includeout.rda")
nonlinearityW_includeout$variable <- rep(1:1111, 20)
## seasonality
load("data/weekly/pdp_ice/seasonalityW_includeout.rda")
seasonalityW_includeout$variable <- rep(1:1111, 20)
## seas_pacf
load("data/weekly/pdp_ice/seas_pacfW_includeout.rda")
seas_pacfW_includeout$variable <- rep(1:1111, 20)
## sediff_acf1
load("data/weekly/pdp_ice/sediff_acf1W_includeout.rda")
sediff_acf1W_includeout$variable <- rep(1:1111, 20)
## sediff_acf5
load("data/weekly/pdp_ice/sediff_acf5W_includeout.rda")
sediff_acf5W_includeout$variable <- rep(1:1111, 20)
## N
load("data/weekly/pdp_ice/NW_includeout.rda")
NW_includeout$variable <- rep(1:1111, 20)
## y_acf5
load("data/weekly/pdp_ice/y_acf5W_includeout.rda")
y_acf5W_includeout$variable <- rep(1:1111, 20)
## diff1y_acf5
load("data/weekly/pdp_ice/diff1y_acf5W_includeout.rda")
diff1y_acf5W_includeout$variable <- rep(1:1111, 20)
## diff2y_acf5
load("data/weekly/pdp_ice/diff2y_acf5W_includeout.rda")
diff2y_acf5W_includeout$variable <- rep(1:1111, 20)
## alpha
load("data/weekly/pdp_ice/alphaW_includeout.rda")
alphaW_includeout$variable <- rep(1:1111, 20)
## beta
load("data/weekly/pdp_ice/betaW_includeout.rda")
betaW_includeout$variable <- rep(1:1111, 20)
## sediff_seacf1
load("data/weekly/pdp_ice/sediff_seacf1W_includeout.rda")
sediff_seacf1W_includeout$variable <- rep(1:1111, 20)

## snaive
p1 <- ggplot(data = seasonalityW_includeout, aes_string(x = seasonalityW_includeout$seasonality, y = "snaive")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonality") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("snaive")
p2 <- ggplot(data = trendW_includeout, aes_string(x = trendW_includeout$trend, y = "snaive")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")
p3 <- ggplot(data = linearityW_includeout, aes_string(x = linearityW_includeout$linearity, y = "snaive")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")
p1s <- ggplot(data = stabilityW_includeout, aes_string(x = stabilityW_includeout$stability, y = "snaive")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")
p1sp <- ggplot(data = spikinessW_includeout, aes_string(x = spikinessW_includeout$spikiness, y = "snaive")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("spikiness") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")

## rwd
p4 <- ggplot(data = seasonalityW_includeout, aes_string(x = seasonalityW_includeout$seasonality, y = "rwd")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonality") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("rwd")
p5 <- ggplot(data = trendW_includeout, aes_string(x = trendW_includeout$trend, y = "rwd")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p6 <- ggplot(data = linearityW_includeout, aes_string(x = linearityW_includeout$linearity, y = "rwd")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p2s <- ggplot(data = stabilityW_includeout, aes_string(x = stabilityW_includeout$stability, y = "rwd")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")
p2sp <- ggplot(data = spikinessW_includeout, aes_string(x = spikinessW_includeout$spikiness, y = "rwd")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("spikiness") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")
## rw
p7 <- ggplot(data = seasonalityW_includeout, aes_string(x = seasonalityW_includeout$seasonality, y = "rw")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonality") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("rw")
p8 <- ggplot(data = trendW_includeout, aes_string(x = trendW_includeout$trend, y = "rw")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p9 <- ggplot(data = linearityW_includeout, aes_string(x = linearityW_includeout$linearity, y = "rw")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p3s <- ggplot(data = stabilityW_includeout, aes_string(x = stabilityW_includeout$stability, y = "rw")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")
p3sp <- ggplot(data = spikinessW_includeout, aes_string(x = spikinessW_includeout$spikiness, y = "rw")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("spikiness") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")
## ARIMA
p10 <- ggplot(data = seasonalityW_includeout, aes_string(x = seasonalityW_includeout$seasonality, y = "ARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonality") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("ARIMA")
p11 <- ggplot(data = trendW_includeout, aes_string(x = trendW_includeout$trend, y = "ARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p12 <- ggplot(data = linearityW_includeout, aes_string(x = linearityW_includeout$linearity, y = "ARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p4s <- ggplot(data = stabilityW_includeout, aes_string(x = stabilityW_includeout$stability, y = "ARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")
p4sp <- ggplot(data = spikinessW_includeout, aes_string(x = spikinessW_includeout$spikiness, y = "ARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("spikiness") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")

## SARIMA
p13 <- ggplot(data = seasonalityW_includeout, aes_string(x = seasonalityW_includeout$seasonality, y = "SARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonality") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("SARIMA")
p14 <- ggplot(data = trendW_includeout, aes_string(x = trendW_includeout$trend, y = "SARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p15 <- ggplot(data = linearityW_includeout, aes_string(x = linearityW_includeout$linearity, y = "SARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p5s <- ggplot(data = stabilityW_includeout, aes_string(x = stabilityW_includeout$stability, y = "SARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")
p5sp <- ggplot(data = spikinessW_includeout, aes_string(x = spikinessW_includeout$spikiness, y = "SARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("spikiness") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")
## stlar
p16 <- ggplot(data = seasonalityW_includeout, aes_string(x = seasonalityW_includeout$seasonality, y = "stlar")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonality") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("stlar")
p17 <- ggplot(data = trendW_includeout, aes_string(x = trendW_includeout$trend, y = "stlar")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p18 <- ggplot(data = linearityW_includeout, aes_string(x = linearityW_includeout$linearity, y = "stlar")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p6s <- ggplot(data = stabilityW_includeout, aes_string(x = stabilityW_includeout$stability, y = "stlar")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")
p6sp <- ggplot(data = spikinessW_includeout, aes_string(x = spikinessW_includeout$spikiness, y = "stlar")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("spikiness") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")

## mstlets
p19 <- ggplot(data = seasonalityW_includeout, aes_string(x = seasonalityW_includeout$seasonality, y = "mstlets")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonality") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("mstlets")
p20 <- ggplot(data = trendW_includeout, aes_string(x = trendW_includeout$trend, y = "mstlets")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p21 <- ggplot(data = linearityW_includeout, aes_string(x = linearityW_includeout$linearity, y = "mstlets")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p7s <- ggplot(data = stabilityW_includeout, aes_string(x = stabilityW_includeout$stability, y = "mstlets")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")
p7sp <- ggplot(data = spikinessW_includeout, aes_string(x = spikinessW_includeout$spikiness, y = "mstlets")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("spikiness") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")

## tbats
p22 <- ggplot(data = seasonalityW_includeout, aes_string(x = seasonalityW_includeout$seasonality, y = "tbats")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonality") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("tbats")
p23 <- ggplot(data = trendW_includeout, aes_string(x = trendW_includeout$trend, y = "tbats")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p24 <- ggplot(data =linearityW_includeout, aes_string(x = linearityW_includeout$linearity, y = "tbats")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p8s <- ggplot(data = stabilityW_includeout, aes_string(x = stabilityW_includeout$stability, y = "tbats")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")
p8sp <- ggplot(data = spikinessW_includeout, aes_string(x = spikinessW_includeout$spikiness, y = "tbats")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("spikiness") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")

## ARMA
p25 <- ggplot(data = seasonalityW_includeout, aes_string(x = seasonalityW_includeout$seasonality, y = "ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonality") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("ARMA.AR.MA")
p26 <- ggplot(data = trendW_includeout, aes_string(x = trendW_includeout$trend, y = "ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p27 <- ggplot(data = linearityW_includeout, aes_string(x = linearityW_includeout$linearity, y = "ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p9s <- ggplot(data = stabilityW_includeout, aes_string(x = stabilityW_includeout$stability, y = "ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")
p9sp <- ggplot(data = spikinessW_includeout, aes_string(x = spikinessW_includeout$spikiness, y = "ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("spikiness") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")

## wn
p28 <- ggplot(data = seasonalityW_includeout, aes_string(x = seasonalityW_includeout$seasonality, y = "wn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonality") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("wn")
p29 <- ggplot(data = trendW_includeout, aes_string(x = trendW_includeout$trend, y = "wn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p30 <- ggplot(data = linearityW_includeout, aes_string(x = linearityW_includeout$linearity, y = "wn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p10s <- ggplot(data = stabilityW_includeout, aes_string(x = stabilityW_includeout$stability, y = "wn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")
p10sp <- ggplot(data = spikinessW_includeout, aes_string(x = spikinessW_includeout$spikiness, y = "wn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("spikiness") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")

## theta
p31 <- ggplot(data = seasonalityW_includeout, aes_string(x = seasonalityW_includeout$seasonality, y = "theta")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonality") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("theta")
p32 <- ggplot(data = trendW_includeout, aes_string(x = trendW_includeout$trend, y = "theta")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p33 <- ggplot(data = linearityW_includeout, aes_string(x = linearityW_includeout$linearity, y = "theta")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p11s <- ggplot(data = stabilityW_includeout, aes_string(x = stabilityW_includeout$stability, y = "theta")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")
p11sp <- ggplot(data = spikinessW_includeout, aes_string(x = spikinessW_includeout$spikiness, y = "theta")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("spikiness") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")

## nn
p34 <- ggplot(data = seasonalityW_includeout, aes_string(x = seasonalityW_includeout$seasonality, y = "nn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonality") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("nn")
p35 <- ggplot(data = trendW_includeout, aes_string(x = trendW_includeout$trend, y = "nn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p36 <- ggplot(data = linearityW_includeout, aes_string(x = linearityW_includeout$linearity, y = "nn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p12s <- ggplot(data = stabilityW_includeout, aes_string(x = stabilityW_includeout$stability, y = "nn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("stability") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")
p12sp <- ggplot(data = spikinessW_includeout, aes_string(x = spikinessW_includeout$spikiness, y = "nn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("spikiness") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") +ylab("")

(p1|p2|p3|p1s|p1sp)/(p4|p5|p6|p2s|p2sp)/(p7|p8|p9|p3s|p3sp)/(p10|p11|p12|p4s|p4sp)/(p13|p14|p15|p5s|p5sp)/(p16|p17|p18|p6s|p6sp)/(p19|p20|p21|p7s|p7sp)/(p22|p23|p24|p8s|p8sp)/(p25|p26|p27|p9s|p9sp)/(p28|p29|p30|p10s|p10sp)/(p31|p32|p33|p11s|p11sp)/(p34|p35|p36|p12s|p12sp)

## ---- friedmanHW
load("data/friedmanHstat_weekly.rda")
col.order <- c("spikiness", "linearity", "trend", "seasonality", "stability",
               "lumpiness", "curvature", "sediff_acf5", "entropy", "beta","y_pacf5",
               "seas_pacf", "N", "diff2y_acf5", "nonlinearity", "sediff_seacf1",
               "y_acf5", "diff1y_pacf5", "y_acf1", "diff1y_acf5", "diff2y_pacf5", 
               "e_acf1", "sediff_acf1", "diff2y_acf1", "hurst", "alpha", "diff1y_acf1")
  
## snaive
snaive_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="snaive",]
snaive_WFH_cormat <- friedmanHstat_matrix(snaive_WFH, 27, rev(col.order))
p1 <- ggcorrplot(snaive_WFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("snaive")

## rwd
rwd_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="rwd",]
rwd_WFH_cormat <- friedmanHstat_matrix(rwd_WFH, 27, rev(col.order))
p2 <- ggcorrplot(rwd_WFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("rwd")

## rw
rw_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="rw",]
rw_WFH_cormat <- friedmanHstat_matrix(rw_WFH, 27, rev(col.order))
p3 <- ggcorrplot(rw_WFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("rw")

## ARIMA
arima_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="ARIMA",]
arima_WFH_cormat <- friedmanHstat_matrix(arima_WFH, 27, rev(col.order))
p4 <- ggcorrplot(arima_WFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ARIMA")

## SARIMA
sarima_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="SARIMA",]
sarima_WFH_cormat <- friedmanHstat_matrix(sarima_WFH, 27, rev(col.order))
p5 <- ggcorrplot(sarima_WFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("SARIMA")

## stlar
stlar_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="stlar",]
stlar_WFH_cormat <- friedmanHstat_matrix(stlar_WFH, 27, rev(col.order))
p6 <- ggcorrplot(stlar_WFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("stlar")

## mstlets
mstlets_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="mstlets",]
mstlets_WFH_cormat <- friedmanHstat_matrix(mstlets_WFH, 27, rev(col.order))
p7 <- ggcorrplot(mstlets_WFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("mstlets")


## tbats
tbats_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="tbats",]
tbats_WFH_cormat <- friedmanHstat_matrix(tbats_WFH, 27, rev(col.order))
p8 <- ggcorrplot(tbats_WFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("tbats")


## ARMA
arma_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="ARMA.AR.MA",]
arma_WFH_cormat <- friedmanHstat_matrix(arma_WFH, 27, rev(col.order))
p9 <- ggcorrplot(arma_WFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ARMA/AR/MA")

## wn
wn_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="wn",]
wn_WFH_cormat <- friedmanHstat_matrix(wn_WFH, 27, rev(col.order))
p10 <- ggcorrplot(wn_WFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("wn")

## theta
theta_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="theta",]
theta_WFH_cormat <- friedmanHstat_matrix(theta_WFH, 27, rev(col.order))
p11 <- ggcorrplot(theta_WFH_cormat, hc.order = FALSE, type = "upper",
                  outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("theta")

## nn
nn_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="nn",]
nn_WFH_cormat <- friedmanHstat_matrix(nn_WFH, 27, rev(col.order))
p12 <- ggcorrplot(nn_WFH_cormat, hc.order = FALSE, type = "upper",
                  outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("nn")


p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+p11+p12+plot_layout(ncol = 4, nrow = 3)

## ---- pcaweekly
load("data/weekly/trainW_votes.rda")
pcaWvariables <- weekly_training[, c(1:26, 28)]
pcaM4W <- prcomp(pcaWvariables, center = TRUE, scale = TRUE)
# summary(pcaM4W)
PC1m4w <- pcaM4W$x[, 1]
PC2m4w <- pcaM4W$x[, 2]
PC3m4w <- pcaM4W$x[, 3]
m4wPCAresults1 <- data.frame(PC1 = PC1m4w, PC2 = PC2m4w, PC3 = PC3m4w, pcaWvariables)
m4wPCAresults1$predicted <- trainW_predictions_oob
trainW_votes1 <- data.frame(trainW_votes)
m4wPCAresults <- dplyr::bind_cols(m4wPCAresults1, trainW_votes1)

pca1M4W_rwd <- ggplot(m4wPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4wPCAresults[m4wPCAresults$predicted == "rwd", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "rwd") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4W_rw <- ggplot(m4wPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4wPCAresults[m4wPCAresults$predicted == "rw", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "rw") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))


pca1M4W_ARIMA <- ggplot(m4wPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4wPCAresults[m4wPCAresults$predicted == "ARIMA", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ARIMA") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4W_ARMA <- ggplot(m4wPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4wPCAresults[m4wPCAresults$predicted == "ARMA/AR/MA", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ARMA/AR/MA") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4W_wn <- ggplot(m4wPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4wPCAresults[m4wPCAresults$predicted == "wn", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "wn") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4W_theta <- ggplot(m4wPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4wPCAresults[m4wPCAresults$predicted == "theta", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "theta") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4W_nn <- ggplot(m4wPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4wPCAresults[m4wPCAresults$predicted == "nn", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "nn") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4W_tbats <- ggplot(m4wPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4wPCAresults[m4wPCAresults$predicted == "tbats", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "tbats") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4W_mstlets <- ggplot(m4wPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4wPCAresults[m4wPCAresults$predicted == "mstlets", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "mstlets") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4W_SARIMA <- ggplot(m4wPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4wPCAresults[m4wPCAresults$predicted == "SARIMA", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "SARIMA") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4W_stlar <- ggplot(m4wPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4wPCAresults[m4wPCAresults$predicted == "stlar", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "stlar") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))


pca1M4W_snaive <- ggplot(m4wPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4wPCAresults[m4wPCAresults$predicted == "snaive", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "snaive") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4W_snaive+pca1M4W_rwd+pca1M4W_rw+pca1M4W_ARIMA + pca1M4W_SARIMA + pca1M4W_stlar+
  pca1M4W_mstlets + pca1M4W_tbats + pca1M4W_wn + pca1M4W_theta+ pca1M4W_nn+plot_layout(ncol = 5, nrow = 3)

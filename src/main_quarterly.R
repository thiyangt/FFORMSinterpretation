## ---- oobquarterly
load("data/quarterly/trainQ_votes.rda") # oob votes from the random forest
load("data/quarterly/trainQ_predictions_oob.rda") # based on oob prediction
load("data/quarterly/quarterly_training.rda") # random forest training set
votes_oobQ <- data.frame(trainQ_votes)
names(votes_oobQ) <- names(table(trainQ_predictions_oob))
votes_oobQ$predicted <- trainQ_predictions_oob
votes_oobQ$classlabel <- quarterly_training$classlabels
votes_oobQ <- votes_oobQ %>%
  mutate(id = seq_len(n())) %>%
  melt(id.var = c("classlabel", "id", "predicted"), na.rm = T) %>%
  select(-id)


votes_oobQ <- votes_oobQ %>% mutate(classlabel = recode(classlabel,
  "snaive"="snaive", "rwd"="rwd", "rw"="rw", "ETS-notrendnoseasonal"="ETS_NTNS",
  "ETS-dampedtrend"="ETS_DT", "ETS-trend"="ETS_T", 
  "ETS-dampedtrendseasonal"="ETS_DTS", "ETS-trendseasonal"="ETS_TS", 
  "ETS-seasonal"="ETS_S", "SARIMA"="SARIMA", "ARIMA"="ARIMA",
  "ARMA/AR/MA"="ARMA", "stlar"="stlar", 
  "tbats"="tbats", "wn"="wn", "theta"="theta", "nn"="nn"))

votes_oobQ <- votes_oobQ %>% mutate(predicted = recode(predicted,
            "snaive"="snaive", "rwd"="rwd", "rw"="rw", "ETS-notrendnoseasonal"="ETS_NTNS", 
            "ETS-dampedtrend"="ETS_DT", "ETS-trend"="ETS_T", "ETS-dampedtrendseasonal"="ETS_DTS",
            "ETS-trendseasonal"="ETS_TS", "ETS-seasonal"="ETS_S", "SARIMA"="SARIMA",
            "ARIMA"="ARIMA", "ARMA/AR/MA"="ARMA", "stlar"="stlar", 
            "tbats"="tbats", "wn"="wn", "theta"="theta", "nn"="nn"))

votes_oobQ <- votes_oobQ %>% mutate(variable= recode(variable,
              "snaive"="snaive", "rwd"="rwd", "rw"="rw", "ETS-notrendnoseasonal"="ETS_NTNS", 
              "ETS-dampedtrend"="ETS_DT", "ETS-trend"="ETS_T", "ETS-dampedtrendseasonal"="ETS_DTS",
              "ETS-trendseasonal"="ETS_TS", "ETS-seasonal"="ETS_S", "SARIMA"="SARIMA",
              "ARIMA"="ARIMA", "ARMA/AR/MA"="ARMA", "stlar"="stlar", 
              "tbats"="tbats", "wn"="wn", "theta"="theta", "nn"="nn"))

# new addition to arrange labels
votes_oobQ$variable <- factor(votes_oobQ$variable, levels = c(
  "snaive", "rwd", "rw", "ETS_NTNS", "ETS_DT", "ETS_T", "ETS_DTS", "ETS_TS", "ETS_S", "SARIMA",
  "ARIMA", "ARMA", "stlar", "tbats", "wn", "theta", "nn"
))

oob_boxplot_quarterly <- ggplot(votes_oobQ, aes(x = classlabel, y = value, fill = classlabel)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.4) +
  ylab("Proportion") +
  xlab("") +
  theme(legend.position = "none", legend.title = element_blank(), 
        legend.text.align = 0, text = element_text(size = 30), axis.text.x = element_text(angle = 90),
        strip.text = element_text(size = 40),
        strip.background = element_rect(size=4)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = rev(c(
    "snaive", "rwd", "rw", "ETS_NTNS", "ETS_DT", "ETS_T", "ETS_DTS", "ETS_TS", "ETS_S", "SARIMA",
    "ARIMA", "ARMA", "stlar", "tbats", "wn", "theta", "nn"
  ))) +
  coord_flip() + facet_wrap(. ~ variable, ncol=4)
oob_boxplot_quarterly



## ---- viquarterly
# All variable scores into one dataframe
load("data/quarterly/trainQ_importance.rda")
load(file = "data/quarterly/sd_pdf_dfQ.rda")
load(file = "data/quarterly/sd_ice_dfQ.rda")
## Permutation based
train_imp_dfQ <- data.frame(trainQ_importance)
train_imp_dfQ <- add_rownames(train_imp_dfQ, "Feature")
train_imp_dfQ <- within(train_imp_dfQ, rm("MeanDecreaseAccuracy", "MeanDecreaseGini"))
permutation_impQ <- train_imp_dfQ %>% melt(id.vars = "Feature")
# dim(permutation_impQ) # 510 3
colnames(permutation_impQ) <- c("feature", "class", "score")
## PDP-based
sd_pdf_dfQ <- add_rownames(sd_pdf_dfQ, "class")
pdp_imp <- sd_pdf_dfQ %>% melt(id.vars = "class")
colnames(pdp_imp) <- c("class", "feature", "score")
## ICE-based
sd_ice_dfQ <- add_rownames(sd_ice_dfQ, "class")
ice_imp <- sd_ice_dfQ %>% melt(id.vars = "class")
colnames(ice_imp) <- c("class", "feature", "score")
## Combine the data frames
importancescoreQ <- bind_rows(permutation_impQ, pdp_imp)
importancescoreQ <- bind_rows(importancescoreQ, ice_imp)
importancescoreQ$VI <- rep(c("permutation", "PDP", "ICE"), each = 510)
## rank permutation, sd_pdp, and sd_ice scores for each class
importancescoreQ$class <- factor(importancescoreQ$class,
                                 levels = c(
                                   "snaive", "rwd", "rw", "ETS.notrendnoseasonal", "ETS.dampedtrend", "ETS.trend", "ETS.dampedtrendseasonal", "ETS.trendseasonal", "ETS.seasonal", "SARIMA",
                                   "ARIMA", "ARMA.AR.MA", "stlar", "tbats", "wn", "theta", "nn"
                                 ),
                                 labels = c(
                                   "snaive", "rwd", "rw", "ETS.NTNS", "ETS.DT", "ETS.T", "ETS.DTS", "ETS.TS", "ETS.S", "SARIMA",
                                   "ARIMA", "ARMA.AR.MA", "stlar", "tbats", "wn", "theta", "nn"
                                 )
)
rank_vi_quarterly_classes <- importancescoreQ %>%
  group_by(VI, class) %>%
  mutate(rank = min_rank(score))
## compute mean rank
meanrank_viq_classes <- rank_vi_quarterly_classes %>% group_by(feature, class) %>% summarise_at(vars(c(rank)), funs(mean))
## overall importance of features to the forest
train_impforestQ <- data.frame(trainQ_importance)
train_impforestQ <- add_rownames(train_impforestQ, "Feature")
train_impforestQ <- train_impforestQ[, c("Feature", "MeanDecreaseAccuracy", "MeanDecreaseGini")]
train_impforestQ <- train_impforestQ %>%
  mutate(rank_permu = min_rank(MeanDecreaseAccuracy)) %>%
  mutate(rank_gini = min_rank(MeanDecreaseGini))
train_impforestQ$mean_rank <- (train_impforestQ$rank_permu + train_impforestQ$rank_gini) / 2
meanrank_viq_forest <- data.frame(
  feature = train_impforestQ$Feature,
  class = rep("overall", 30),
  rank = train_impforestQ$mean_rank
)
## combine mean ranks for overall forest and separate classes
meanrank_quarterly <- dplyr::bind_rows(meanrank_viq_forest, meanrank_viq_classes)
## create horizontal bar chart for ranks
orderOverall <- filter(meanrank_quarterly, class == "overall")
meanrank_quarterly$feature <- factor(meanrank_quarterly$feature, levels = orderOverall$feature[order(orderOverall$rank)])
meanrank_quarterly$class <- factor(meanrank_quarterly$class,
                                   levels = c(
                                     "overall", "snaive", "rwd", "rw", "ETS.NTNS", "ETS.DT", "ETS.T", "ETS.DTS", "ETS.TS", "ETS.S", "SARIMA",
                                     "ARIMA", "ARMA.AR.MA", "stlar", "tbats", "wn", "theta", "nn"
                                   ),
                                   labels = c(
                                     "overall", "snaive", "rwd", "rw", "ETS.NTNS", "ETS.DT", "ETS.T", "ETS.DTS", "ETS.TS", "ETS.S", "SARIMA",
                                     "ARIMA", "ARMA", "stlar", "tbats", "wn", "theta", "nn"
                                   )
)
meanrank_quarterly$rn <- 1:540
topq <- meanrank_quarterly %>%
  group_by(class) %>%
  top_n(n = 5, wt = rank)
meanrank_quarterly$istop <- ifelse(meanrank_quarterly$rn %in% topq$rn, TRUE, FALSE)
levels(meanrank_quarterly$feature)[levels(meanrank_quarterly$feature)=="N"] <- "T"
levels(meanrank_quarterly$feature)[levels(meanrank_quarterly$feature)=="seasonality"] <- "seasonality_q"
feaImp_quarterly <- ggplot(meanrank_quarterly, aes(y = rank, x = feature,fill=as.factor(istop)), width=0.1) +
  geom_bar(position = "dodge", stat = "identity", width=0.3) +
  facet_wrap(~class, ncol = 9, nrow = 2) +
  coord_flip() + ylab("Average rank")+ 
  scale_fill_manual(breaks=c("0","1"), values=c("#f1a340","#998ec3"), guide="none")+
  theme(text=element_text(size = 18))
feaImp_quarterly


## ---- pdpquarterlyseasonality
load("data/quarterly/pdp_quarterly/seasonalitygridQ.rda")
keep.modelnamesq <- c("ARIMA", "ARMA.AR.MA", "ETS.dampedtrend", "ETS.dampedtrendseasonal",
                      "ETS.notrendnoseasonal", "ETS.seasonal", 
                     "ETS.trend","ETS.trendseasonal"  ,"nn", "rw",
                     "rwd", "SARIMA","snaive","stlar","tbats","theta", "wn")
keepseasonal <- c(keep.modelnamesq, "seasonality")
seasonalitygridQ <- seasonalitygridQ[, names(seasonalitygridQ) %in% keepseasonal]
seasonalitygridQ_long <- gather(seasonalitygridQ, class, probability, "ARIMA":"wn", factor_key = TRUE)

seasonalitygridQ_long <- seasonalitygridQ_long %>%
  mutate(class = recode(class, "ARIMA"="ARIMA", "ARMA.AR.MA"="ARMA", 
                        "ETS.dampedtrend"="ETS_DT", "ETS.dampedtrendseasonal"="ETS_DTS",
                        "ETS.notrendnoseasonal"="ETS_NTNS", "ETS.seasonal"="ETS_S", 
                        "ETS.trend"="ETS_T","ETS.trendseasonal"="ETS_TS"  ,"nn"="nn", "rw"="rw",
                        "rwd"="rwd", "SARIMA"="SARIMA","snaive"="snaive","stlar"="stlar","tbats"="tbats","theta"="theta", "wn"="wn"))
seasonalitygridQ_long <- seasonalitygridQ_long %>% rename("seasonality_Q"="seasonality")

seasonalitygridQ_long$class <- factor(seasonalitygridQ_long$class,
                               levels = c("snaive","rw", "rwd", "ETS_NTNS","ETS_DT", "ETS_T", "ETS_DTS",
                                          "ETS_TS", "ETS_S","tbats","stlar", "SARIMA",
                                          "ARIMA", "ARMA", "wn", "theta", "nn" ))

plot_pdp_quarterlyS <- ggplot(data = seasonalitygridQ_long, aes_string(x = seasonalitygridQ_long$"seasonality_Q", y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=18), axis.title = element_text(size = 16))+
  facet_wrap(. ~ class, ncol=9)+theme(strip.text.x = element_text(size = 10))+xlab("strength of seasonality (seasonality_q)")+ylab("probability of selecting forecast-models")
plot_pdp_quarterlyS



## ---- pdpquarterlydiff1ypacf5
load("data/quarterly/pdp_quarterly/diff1y_pacf5gridQ.rda")
keep.modelnamedq <- c("ARIMA", "ARMA.AR.MA", "ETS.dampedtrend", "ETS.dampedtrendseasonal",
                      "ETS.notrendnoseasonal", "ETS.seasonal", 
                      "ETS.trend","ETS.trendseasonal"  ,"nn", "rw",
                      "rwd", "SARIMA","snaive","stlar","tbats","theta", "wn")
keepdiffpacf <- c(keep.modelnamesq, "diff1y_pacf5")
diff1y_pacf5gridQ <- diff1y_pacf5gridQ[, names(diff1y_pacf5gridQ) %in% keepdiffpacf]
diff1y_pacf5gridQ_long <- gather(diff1y_pacf5gridQ, class, probability, "ARIMA":"wn", factor_key = TRUE)
diff1y_pacf5gridQ_long <- diff1y_pacf5gridQ_long %>%
  mutate(class = recode(class, "ARIMA"="ARIMA", "ARMA.AR.MA"="ARMA", 
                        "ETS.dampedtrend"="ETS_DT", "ETS.dampedtrendseasonal"="ETS_DTS",
                        "ETS.notrendnoseasonal"="ETS_NTNS", "ETS.seasonal"="ETS_S", 
                        "ETS.trend"="ETS_T","ETS.trendseasonal"="ETS_TS"  ,"nn"="nn", "rw"="rw",
                        "rwd"="rwd", "SARIMA"="SARIMA","snaive"="snaive","stlar"="stlar","tbats"="tbats","theta"="theta", "wn"="wn"))
diff1y_pacf5gridQ_long$class <- factor(diff1y_pacf5gridQ_long$class,
                                      levels = c("snaive","rw", "rwd", "ETS_NTNS","ETS_DT", "ETS_T", "ETS_DTS",
                                                 "ETS_TS", "ETS_S","tbats","stlar", "SARIMA",
                                                 "ARIMA", "ARMA", "wn", "theta", "nn" ))

plot_pdp_quarterlyDP <- ggplot(data = diff1y_pacf5gridQ_long, aes_string(x = diff1y_pacf5gridQ_long$"diff1y_pacf5", y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=18), axis.title = element_text(size = 16))+
  facet_wrap(. ~ class, ncol=9)+theme(strip.text.x = element_text(size = 10))+xlab("sum of squares of first 5 PACF values of differenced series (diff1y_pacf5)")+ylab("probability of selecting forecast-models")
plot_pdp_quarterlyDP



## ---- pdpquarterlytrend
load("data/quarterly/pdp_quarterly/trendgridQ.rda")
keep.modelnamet <- c("ARIMA", "ARMA.AR.MA", "ETS.dampedtrend", "ETS.dampedtrendseasonal",
                      "ETS.notrendnoseasonal", "ETS.seasonal", 
                      "ETS.trend","ETS.trendseasonal"  ,"nn", "rw",
                      "rwd", "SARIMA","snaive","stlar","tbats","theta", "wn")
keeptrend <- c(keep.modelnamet, "trend")
trendgridQ <- trendgridQ[, names(trendgridQ) %in% keeptrend]
trendgridQ_long <- gather(trendgridQ, class, probability, "ARIMA":"wn", factor_key = TRUE)
trendgridQ_long <- trendgridQ_long %>%
  mutate(class = recode(class, "ARIMA"="ARIMA", "ARMA.AR.MA"="ARMA", 
                        "ETS.dampedtrend"="ETS_DT", "ETS.dampedtrendseasonal"="ETS_DTS",
                        "ETS.notrendnoseasonal"="ETS_NTNS", "ETS.seasonal"="ETS_S", 
                        "ETS.trend"="ETS_T","ETS.trendseasonal"="ETS_TS"  ,"nn"="nn", "rw"="rw",
                        "rwd"="rwd", "SARIMA"="SARIMA","snaive"="snaive","stlar"="stlar","tbats"="tbats","theta"="theta", "wn"="wn"))
trendgridQ_long$class <- factor(trendgridQ_long$class,
                                       levels = c("snaive","rw", "rwd", "ETS_NTNS","ETS_DT", "ETS_T", "ETS_DTS",
                                                  "ETS_TS", "ETS_S","tbats","stlar", "SARIMA",
                                                  "ARIMA", "ARMA", "wn", "theta", "nn" ))

plot_pdp_quarterlyT <- ggplot(data = trendgridQ_long, aes_string(x = trendgridQ_long$trend, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=18), axis.title = element_text(size = 16))+
  facet_wrap(. ~ class, ncol=9)+theme(strip.text.x = element_text(size = 10))+xlab("strength of trend (trend)")+ylab("probability of selecting forecast-models")
plot_pdp_quarterlyT

## ---- friedmanq
## Overall interaction plot
load("data/quarterly/overall_interactions_q.rda")
## adjust rounding error 
overall_interactions_q$.interaction[overall_interactions_q$.interaction > 1.0] <- 0
overall_interactions_q <- overall_interactions_q %>% mutate(.class = recode(.class,
                                                        "snaive"="snaive", "rwd"="rwd", "rw"="rw", "ETS.notrendnoseasonal"="ETS_NTNS",
                                                        "ETS.dampedtrend"="ETS_DT", "ETS.trend"="ETS_T", 
                                                        "ETS.dampedtrendseasonal"="ETS_DTS", "ETS.trendseasonal"="ETS_TS", 
                                                        "ETS.seasonal"="ETS_S", "SARIMA"="SARIMA", "ARIMA"="ARIMA",
                                                        "ARMA.AR.MA"="ARMA", "stlar"="stlar", 
                                                        "tbats"="tbats", "wn"="wn", "theta"="theta", "nn"="nn"))
# new addition to arrange labels
overall_interactions_q$.class <- factor(overall_interactions_q$.class, levels = c(
  "snaive", "rwd", "rw", "ETS_NTNS", "ETS_DT", "ETS_T", "ETS_DTS", "ETS_TS", "ETS_S", "SARIMA",
  "ARIMA", "ARMA", "stlar", "tbats", "wn", "theta", "nn"
))

# arrange features according to the order of rw class
orderSNAIVE <- filter(overall_interactions_q, .class == "snaive")
overall_interactions_q$.feature <- factor(overall_interactions_q$.feature,
                                          levels = orderSNAIVE$.feature[order(orderSNAIVE$.interaction)])
top <- overall_interactions_q %>%
  group_by(.class) %>%
  top_n(n = 5, wt = .interaction)

overall_interactions_q$istop <- ifelse(overall_interactions_q$.interaction%in%top$.interaction, TRUE, FALSE)


colnames(overall_interactions_q) <- c("feature", "class", "interaction", "istop")
levels(overall_interactions_q$feature)[levels(overall_interactions_q$feature)=="N"] <- "T"
levels(overall_interactions_q$feature)[levels(overall_interactions_q$feature)=="seasonality"] <- "seasonality_q" 
FHinteraction_quarterly <- ggplot(overall_interactions_q, 
                               aes(y = interaction, x = feature, fill=as.factor(istop))) +
  geom_bar(position = "dodge", stat = "identity", width=0.3) +
  facet_wrap(~ class, ncol = 9, nrow = 2) +
  coord_flip() + ylab("Overall interaction strength(Friedman's H-Statistic)")+
  scale_fill_manual(breaks=c("0","1"), values=c("#7fbf7b","#af8dc3"), guide="none")+
  theme(text=element_text(size = 20), axis.text.x = element_text(angle = 90, hjust = 1))
FHinteraction_quarterly


## ---- intquarterly
load("data/quarterly/e_acf1.curvature.q.rda")
colNamesds <- colnames(e_acf1.curvature.q)[32:48]

keep.modelnames <- c("ARIMA", "ARMA.AR.MA", "ETS.dampedtrend", "ETS.dampedtrendseasonal",
                     "ETS.notrendnoseasonal", "ETS.seasonal", 
                     "ETS.trend","ETS.trendseasonal"  ,"nn", "rw",
                     "rwd", "SARIMA","snaive","stlar","tbats","theta", "wn")
keepq <- c(keep.modelnames, c("e_acf1", "curvature"))
e_acf1.curvature.q <- e_acf1.curvature.q[, names(e_acf1.curvature.q) %in% keepq]
e_acf1.curvature.q.long <- gather(e_acf1.curvature.q, class, probability, "ARIMA":"wn", factor_key = TRUE)
e_acf1.curvature.q.long <- e_acf1.curvature.q.long %>%
  mutate(class = recode(class, "ARIMA"="ARIMA", "ARMA.AR.MA"="ARMA", 
                        "ETS.dampedtrend"="ETS_DT", "ETS.dampedtrendseasonal"="ETS_DTS",
                        "ETS.notrendnoseasonal"="ETS_NTNS", "ETS.seasonal"="ETS_S", 
                        "ETS.trend"="ETS_T","ETS.trendseasonal"="ETS_TS"  ,"nn"="nn", "rw"="rw",
                        "rwd"="rwd", "SARIMA"="SARIMA","snaive"="snaive","stlar"="stlar","tbats"="tbats","theta"="theta", "wn"="wn"))
e_acf1.curvature.q.long$class <- factor(e_acf1.curvature.q.long$class,
                                           levels = c("snaive","rw", "rwd", "ETS_NTNS","ETS_DT", "ETS_T", "ETS_DTS",
                                                      "ETS_TS", "ETS_S","tbats","stlar", "SARIMA",
                                                      "ARIMA", "ARMA", "wn", "theta", "nn" ))


e_acf1.curvature.q.long %>%
  ggplot(aes(x = e_acf1, y = curvature)) +
  geom_raster(aes(fill = probability)) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~class, ncol=6) +
  scale_fill_viridis_c(breaks=c(0,0.125,100),
                       limits=c(0,0.125), option = "A", direction = -1) +
  theme(strip.text.x = element_text(size = 12),legend.position="right", legend.direction='vertical')  


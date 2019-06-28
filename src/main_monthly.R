## ---- oobmonthly
load("data/monthly/trainM_votes.rda") #oob votes from the random forest
load("data/monthly/trainM_predictions_oob.rda") # based on oob prediction
load("data/monthly/monthly_training.rda") # random forest training set
votes_oobM <- data.frame(trainM_votes)
names(votes_oobM) <- names(table(trainM_predictions_oob))
votes_oobM$predicted <- trainM_predictions_oob
votes_oobM$classlabel <- monthly_training$classlabels
votes_oobM <- votes_oobM %>% mutate(id=seq_len(n())) %>%
  melt(id.var=c('classlabel','id','predicted'), na.rm=T) %>%
  select(-id)
votes_oobM$classlabel <- factor(votes_oobM$classlabel, levels=rev(c("snaive","rwd", "rw", "ETS-notrendnoseasonal","ETS-dampedtrend", "ETS-trend", "ETS-dampedtrendseasonal", "ETS-trendseasonal","ETS-seasonal","SARIMA",
                                                                    "ARIMA", "ARMA/AR/MA","stlar" ,"tbats","wn", "theta","nn"))
)


votes_oobM <- votes_oobM %>% mutate(classlabel = recode(classlabel,
                                                        "snaive"="snaive", "rwd"="rwd", "rw"="rw", "ETS-notrendnoseasonal"="ETS_NTNS",
                                                        "ETS-dampedtrend"="ETS_DT", "ETS-trend"="ETS_T", 
                                                        "ETS-dampedtrendseasonal"="ETS_DTS", "ETS-trendseasonal"="ETS_TS", 
                                                        "ETS-seasonal"="ETS_S", "SARIMA"="SARIMA", "ARIMA"="ARIMA",
                                                        "ARMA/AR/MA"="ARMA", "stlar"="stlar", 
                                                        "tbats"="tbats", "wn"="wn", "theta"="theta", "nn"="nn"))

votes_oobM<- votes_oobM %>% mutate(predicted = recode(predicted,
                                                       "snaive"="snaive", "rwd"="rwd", "rw"="rw", "ETS-notrendnoseasonal"="ETS_NTNS", 
                                                       "ETS-dampedtrend"="ETS_DT", "ETS-trend"="ETS_T", "ETS-dampedtrendseasonal"="ETS_DTS",
                                                       "ETS-trendseasonal"="ETS_TS", "ETS-seasonal"="ETS_S", "SARIMA"="SARIMA",
                                                       "ARIMA"="ARIMA", "ARMA/AR/MA"="ARMA", "stlar"="stlar", 
                                                       "tbats"="tbats", "wn"="wn", "theta"="theta", "nn"="nn"))

votes_oobM <- votes_oobM %>% mutate(variable= recode(variable,
                                                     "snaive"="snaive", "rwd"="rwd", "rw"="rw", "ETS-notrendnoseasonal"="ETS_NTNS", 
                                                     "ETS-dampedtrend"="ETS_DT", "ETS-trend"="ETS_T", "ETS-dampedtrendseasonal"="ETS_DTS",
                                                     "ETS-trendseasonal"="ETS_TS", "ETS-seasonal"="ETS_S", "SARIMA"="SARIMA",
                                                     "ARIMA"="ARIMA", "ARMA/AR/MA"="ARMA", "stlar"="stlar", 
                                                     "tbats"="tbats", "wn"="wn", "theta"="theta", "nn"="nn"))
# new addition to arrange labels
votes_oobM$variable <- factor(votes_oobM$variable, levels = c(
  "snaive", "rwd", "rw", "ETS_NTNS", "ETS_DT", "ETS_T", "ETS_DTS", "ETS_TS", "ETS_S", "SARIMA",
  "ARIMA", "ARMA", "stlar", "tbats", "wn", "theta", "nn"
))

oob_boxplot_monthly <- ggplot(votes_oobM, aes(x = classlabel, y = value, fill = classlabel)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.4) +
  ylab("log(Proportion)") +
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
oob_boxplot_monthly


## ---- vimonthly
# monthly feature importance
load("data/monthly/trainM_importance.rda")
load(file="data/monthly/sd_pdf_dfM.rda")
load(file="data/monthly/sd_ice_dfM.rda")
## Permutation based
train_imp_dfM <- data.frame(trainM_importance)
train_imp_dfM <- add_rownames(train_imp_dfM, "Feature")
train_imp_dfM <- within(train_imp_dfM, rm("MeanDecreaseAccuracy", "MeanDecreaseGini"))
permutation_impM <- train_imp_dfM %>% melt(id.vars = "Feature")
colnames(permutation_impM) <- c("feature", "class", "score")
## PDP-based
sd_pdf_dfM <- add_rownames(sd_pdf_dfM, "class")
pdp_impM <- sd_pdf_dfM %>% melt(id.vars = "class")
colnames(pdp_impM) <- c("class", "feature", "score")
## ICE-based
sd_ice_dfM <- add_rownames(sd_ice_dfM, "class")
ice_impM <- sd_ice_dfM %>% melt(id.vars = "class")
colnames(ice_impM) <- c("class", "feature", "score")
## Combine the data frames
importancescoreM <- bind_rows(permutation_impM, pdp_impM)
importancescoreM <- bind_rows(importancescoreM, ice_impM)
importancescoreM$VI <- rep(c("permutation", "PDP", "ICE"), each = 510)
## rank permutation, sd_pdp, and sd_ice scores for each class
importancescoreM$class <- factor(importancescoreM$class,
                                 levels = c(
                                   "snaive", "rwd", "rw", "ETS.notrendnoseasonal", "ETS.dampedtrend", "ETS.trend", "ETS.dampedtrendseasonal", "ETS.trendseasonal", "ETS.seasonal", "SARIMA",
                                   "ARIMA", "ARMA.AR.MA", "stlar", "tbats", "wn", "theta", "nn"
                                 ),
                                 labels = c(
                                   "snaive", "rwd", "rw", "ETS.NTNS", "ETS.DT", "ETS.T", "ETS.DTS", "ETS.TS", "ETS.S", "SARIMA",
                                   "ARIMA", "ARMA.AR.MA", "stlar", "tbats", "wn", "theta", "nn"
                                 )
)
rank_vi_monthly_classes <- importancescoreM %>%
  group_by(VI, class) %>%
  mutate(rank = min_rank(score))
## compute mean rank
meanrank_vim_classes <- rank_vi_monthly_classes %>% group_by(feature, class) %>% summarise_at(vars(c(rank)), funs(mean))
## overall importance of features to the forest
train_impforestM <- data.frame(trainM_importance)
train_impforestM <- add_rownames(train_impforestM, "Feature")
train_impforestM <- train_impforestM[, c("Feature", "MeanDecreaseAccuracy", "MeanDecreaseGini")]
train_impforestM <- train_impforestM %>%
  mutate(rank_permu = min_rank(MeanDecreaseAccuracy)) %>%
  mutate(rank_gini = min_rank(MeanDecreaseGini))
train_impforestM$mean_rank <- (train_impforestM$rank_permu + train_impforestM$rank_gini) / 2
meanrank_vim_forest <- data.frame(
  feature = train_impforestM$Feature,
  class = rep("overall", 30),
  rank = train_impforestM$mean_rank
)
## combine mean ranks for overall forest and separate classes
meanrank_monthly <- dplyr::bind_rows(meanrank_vim_forest, meanrank_vim_classes)
## create horizontal bar chart for ranks
orderOverall <- filter(meanrank_monthly, class == "overall")
meanrank_monthly$feature <- factor(meanrank_monthly$feature, levels = orderOverall$feature[order(orderOverall$rank)])
meanrank_monthly$class <- factor(meanrank_monthly$class,
                                 levels = c(
                                   "overall", "snaive", "rwd", "rw", "ETS.NTNS", "ETS.DT", "ETS.T", "ETS.DTS", "ETS.TS", "ETS.S", "SARIMA",
                                   "ARIMA", "ARMA.AR.MA", "stlar", "tbats", "wn", "theta", "nn"
                                 ),
                                 labels = c(
                                   "overall", "snaive", "rwd", "rw", "ETS.NTNS", "ETS.DT", "ETS.T", "ETS.DTS", "ETS.TS", "ETS.S", "SARIMA",
                                   "ARIMA", "ARMA", "stlar", "tbats", "wn", "theta", "nn"
                                 )
)
meanrank_monthly$rn <- 1:540
topq <- meanrank_monthly %>%
  group_by(class) %>%
  top_n(n = 5, wt = rank)
meanrank_monthly$istop <- ifelse(meanrank_monthly$rn %in% topq$rn, TRUE, FALSE)
levels(meanrank_monthly$feature)[levels(meanrank_monthly$feature)=="N"] <- "T"
levels(meanrank_monthly$feature)[levels(meanrank_monthly$feature)=="seasonality"] <- "seasonality_m"
feaImp_monthly <- ggplot(meanrank_monthly, aes(y = rank, x = feature,fill=as.factor(istop))) +
  geom_bar(position = "dodge", stat = "identity", width=0.3) +
  facet_wrap(~class, ncol = 9, nrow = 2) +
  coord_flip() + ylab("Average rank")+ 
  scale_fill_manual(breaks=c("0","1"), values=c("#f1a340","#998ec3"), guide="none")+
  theme(text=element_text(size = 18))
feaImp_monthly



## ---- pdpmonthlylinearity
load("data/monthly/pdp_monthly/linearitygridM.rda")
keep.modelnamesM <- c("ARIMA", "ARMA.AR.MA", "ETS.dampedtrend", "ETS.dampedtrendseasonal",
                      "ETS.notrendnoseasonal", "ETS.seasonal", 
                      "ETS.trend","ETS.trendseasonal"  ,"nn", "rw",
                      "rwd", "SARIMA","snaive","stlar","tbats","theta", "wn")
keeplinearity <- c(keep.modelnamesM, "linearity")
linearitygridM <- linearitygridM[, names(linearitygridM) %in% keeplinearity]
linearitygridM_long <- gather(linearitygridM, class, probability, "ARIMA":"wn", factor_key = TRUE)

linearitygridM_long <- linearitygridM_long %>%
  mutate(class = recode(class, "ARIMA"="ARIMA", "ARMA.AR.MA"="ARMA", 
                        "ETS.dampedtrend"="ETS_DT", "ETS.dampedtrendseasonal"="ETS_DTS",
                        "ETS.notrendnoseasonal"="ETS_NTNS", "ETS.seasonal"="ETS_S", 
                        "ETS.trend"="ETS_T","ETS.trendseasonal"="ETS_TS"  ,"nn"="nn", "rw"="rw",
                        "rwd"="rwd", "SARIMA"="SARIMA","snaive"="snaive","stlar"="stlar","tbats"="tbats","theta"="theta", "wn"="wn"))

linearitygridM_long$class <- factor(linearitygridM_long$class,
                                      levels = c("snaive","rw", "rwd", "ETS_NTNS","ETS_DT", "ETS_T", "ETS_DTS",
                                                 "ETS_TS", "ETS_S","tbats","stlar", "SARIMA",
                                                 "ARIMA", "ARMA", "wn", "theta", "nn" ))

plot_pdp_monthlyL <- ggplot(data = linearitygridM_long, aes_string(x = linearitygridM_long$"linearity", y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=18), axis.title = element_text(size = 16))+
  facet_wrap(. ~ class, ncol=9)+theme(strip.text.x = element_text(size = 10))+xlab("linearity")+ylab("probability of selecting forecast-models")
plot_pdp_monthlyL

## ---- pdpmonthlyN
load("data/monthly/pdp_monthly/NgridM.rda")
keep.modelnamesM <- c("ARIMA", "ARMA.AR.MA", "ETS.dampedtrend", "ETS.dampedtrendseasonal",
                      "ETS.notrendnoseasonal", "ETS.seasonal", 
                      "ETS.trend","ETS.trendseasonal"  ,"nn", "rw",
                      "rwd", "SARIMA","snaive","stlar","tbats","theta", "wn")
keepN <- c(keep.modelnamesM, "N")
NgridM <- NgridM[, names(NgridM) %in% keepN]
NgridM_long <- gather(NgridM, class, probability, "ARIMA":"wn", factor_key = TRUE)

NgridM_long <- NgridM_long %>%
  mutate(class = recode(class, "ARIMA"="ARIMA", "ARMA.AR.MA"="ARMA", 
                        "ETS.dampedtrend"="ETS_DT", "ETS.dampedtrendseasonal"="ETS_DTS",
                        "ETS.notrendnoseasonal"="ETS_NTNS", "ETS.seasonal"="ETS_S", 
                        "ETS.trend"="ETS_T","ETS.trendseasonal"="ETS_TS"  ,"nn"="nn", "rw"="rw",
                        "rwd"="rwd", "SARIMA"="SARIMA","snaive"="snaive","stlar"="stlar","tbats"="tbats","theta"="theta", "wn"="wn"))

NgridM_long$class <- factor(linearitygridM_long$class,
                                    levels = c("snaive","rw", "rwd", "ETS_NTNS","ETS_DT", "ETS_T", "ETS_DTS",
                                               "ETS_TS", "ETS_S","tbats","stlar", "SARIMA",
                                               "ARIMA", "ARMA", "wn", "theta", "nn" ))
NgridM_long <- NgridM_long %>% rename("T"="N")

plot_pdp_monthlyN <- ggplot(data = NgridM_long, aes_string(x = NgridM_long$"T", y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=18), axis.title = element_text(size = 16))+
  facet_wrap(. ~ class, ncol=9)+theme(strip.text.x = element_text(size = 10))+xlab("length of time series (T)")+ylab("probability of selecting forecast-models")
plot_pdp_monthlyN


## ---- friedmanm
## Overall interaction plot
load("data/monthly/overall_interactions_m.rda")
## adjust rounding error
overall_interactions_m$.interaction[overall_interactions_m$.interaction > 1.0] <- 0
overall_interactions_m <- overall_interactions_m %>% mutate(.class = recode(.class,
                                                                            "snaive"="snaive", "rwd"="rwd", "rw"="rw", "ETS.notrendnoseasonal"="ETS_NTNS",
                                                                            "ETS.dampedtrend"="ETS_DT", "ETS.trend"="ETS_T", 
                                                                            "ETS.dampedtrendseasonal"="ETS_DTS", "ETS.trendseasonal"="ETS_TS", 
                                                                            "ETS.seasonal"="ETS_S", "SARIMA"="SARIMA", "ARIMA"="ARIMA",
                                                                            "ARMA.AR.MA"="ARMA", "stlar"="stlar", 
                                                                            "tbats"="tbats", "wn"="wn", "theta"="theta", "nn"="nn"))
# new addition to arrange labels
overall_interactions_m$.class <- factor(overall_interactions_m$.class, levels = c(
  "snaive", "rwd", "rw", "ETS_NTNS", "ETS_DT", "ETS_T", "ETS_DTS", "ETS_TS", "ETS_S", "SARIMA",
  "ARIMA", "ARMA", "stlar", "tbats", "wn", "theta", "nn"
))

# arrange features according to the order of rw class
orderSNAIVE <- filter(overall_interactions_m, .class == "snaive")
overall_interactions_m$.feature <- factor(overall_interactions_m$.feature,
                                          levels = orderSNAIVE$.feature[order(orderSNAIVE$.interaction)])
top <- overall_interactions_m %>%
  group_by(.class) %>%
  top_n(n = 5, wt = .interaction)

overall_interactions_m$istop <- ifelse(overall_interactions_m$.interaction%in%top$.interaction, TRUE, FALSE)



colnames(overall_interactions_m) <- c("feature", "class", "interaction", "istop")

levels(overall_interactions_m$feature)[levels(overall_interactions_m$feature)=="N"] <- "T"
levels(overall_interactions_m$feature)[levels(overall_interactions_m$feature)=="seasonality"] <- "seasonality_m"
FHinteraction_monthly <- ggplot(overall_interactions_m, 
                                  aes(y = interaction, x = feature, fill=as.factor(istop))) +
  geom_bar(position = "dodge", stat = "identity", width=0.3) +
  facet_wrap(~ class, ncol = 9, nrow = 2) +
  coord_flip() + ylab("Overall inteaction strength (Friedman's H-Statistic)")+
  scale_fill_manual(breaks=c("0","1"), values=c("#7fbf7b","#af8dc3"), guide="none")+
  theme(text=element_text(size = 20), axis.text.x = element_text(angle = 90, hjust = 1))
FHinteraction_monthly


## ---- intmonthly
load("data/monthly/sediff_seacf1.hwalpha.m.rda")
colNamesds <-colnames(sediff_seacf1.hwalpha.m)[32:48]
keep.modelnames <- c("ARIMA", "ARMA.AR.MA", "ETS.dampedtrend", "ETS.dampedtrendseasonal",
                     "ETS.notrendnoseasonal", "ETS.seasonal", 
                     "ETS.trend","ETS.trendseasonal"  ,"nn", "rw",
                     "rwd", "SARIMA","snaive","stlar","tbats","theta", "wn")
keepm <- c(keep.modelnames, c("sediff_seacf1", "hwalpha"))
sediff_seacf1.hwalpha.m <- sediff_seacf1.hwalpha.m[, names(sediff_seacf1.hwalpha.m) %in% keepm]
sediff_seacf1.hwalpha.m.long <- gather(sediff_seacf1.hwalpha.m, class, probability, "ARIMA":"wn", factor_key = TRUE)
sediff_seacf1.hwalpha.m.long <- sediff_seacf1.hwalpha.m.long %>%
  mutate(class = recode(class, "ARIMA"="ARIMA", "ARMA.AR.MA"="ARMA", 
                        "ETS.dampedtrend"="ETS_DT", "ETS.dampedtrendseasonal"="ETS_DTS",
                        "ETS.notrendnoseasonal"="ETS_NTNS", "ETS.seasonal"="ETS_S", 
                        "ETS.trend"="ETS_T","ETS.trendseasonal"="ETS_TS"  ,"nn"="nn", "rw"="rw",
                        "rwd"="rwd", "SARIMA"="SARIMA","snaive"="snaive","stlar"="stlar","tbats"="tbats","theta"="theta", "wn"="wn"))
sediff_seacf1.hwalpha.m.long$class <- factor(sediff_seacf1.hwalpha.m.long$class,
                                             levels = c("snaive","rw", "rwd", "ETS_NTNS","ETS_DT", "ETS_T", "ETS_DTS",
                                                        "ETS_TS", "ETS_S","tbats","stlar", "SARIMA",
                                                        "ARIMA", "ARMA", "wn", "theta", "nn" ))




sediff_seacf1.hwalpha.m.long %>%
  ggplot(aes(x = sediff_seacf1, y = hwalpha, fill = probability)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~class, ncol=6) +
  scale_fill_viridis_c(breaks=c(0,0.17,100),
                       limits=c(0,0.17), option = "A", direction = -1)+
  theme(strip.text.x = element_text(size = 12))


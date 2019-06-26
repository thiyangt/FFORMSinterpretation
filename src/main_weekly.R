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
votes_oobW <- votes_oobW %>%
  mutate(classlabel = recode(classlabel, snaive="snaive",
                             rwd = "rwd", rw = "rw", "ARIMA" = "ARIMA", "ARMA/AR/MA" = "ARMA", "SARIMA" = "SARIMA",
                             "stlar" = "stlar", "mstlets" = "mstlets", "tbats" = "tbats", "wn" = "wn",
                             "theta"="theta", "nn"="nn"))

votes_oobW <- votes_oobW %>%
  mutate(predicted = recode(predicted, snaive="snaive",
                            rwd = "rwd", rw = "rw", "ARIMA" = "ARIMA", "ARMA/AR/MA" = "ARMA", "SARIMA" = "SARIMA",
                            "stlar" = "stlar", "mstlets" = "mstlets", "tbats" = "tbats", "wn" = "wn",
                            "theta"="theta", "nn"="nn" ))

votes_oobW <- votes_oobW %>%
  mutate(variable = recode(variable,snaive="snaive",
                           rwd = "rwd", rw = "rw", "ARIMA" = "ARIMA", "ARMA/AR/MA" = "ARMA", "SARIMA" = "SARIMA",
                           "stlar" = "stlar", "mstlets" = "mstlets", "tbats" = "tbats", "wn" = "wn",
                           "theta"="theta", "nn"="nn"))
# arrange labels
votes_oobW$variable <- factor(votes_oobW$variable,
levels = c( "snaive", "rw", "rwd", "ARMA","ARIMA", "SARIMA",
"stlar", "mstlets", "tbats", "theta", "nn", "wn"))

oob_boxplot_weekly <- ggplot(votes_oobW, aes(x = classlabel, y = value, fill = classlabel)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.4) +
  ylab("Proportion") +
  xlab("") +
  theme(legend.position = "none", legend.title = element_blank(), 
        legend.text.align = 0, text = element_text(size = 20), axis.text.x = element_text(angle = 90),
        strip.text = element_text(size = 20)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = rev(c("snaive", "rw", "rwd", "ARMA","ARIMA", "SARIMA",
                              "stlar", "mstlets", "tbats", "theta", "nn", "wn" ))) +
  coord_flip() + facet_wrap(. ~ variable, ncol=6)
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
                                   "snaive", "rw", "rwd", "ARMA.AR.MA","ARIMA", "SARIMA",
                                   "stlar", "mstlets", "tbats", "theta", "nn", "wn"),
                                 labels = c(
                                   "snaive", "rw", "rwd", "ARMA.AR.MA","ARIMA", "SARIMA",
                                   "stlar", "mstlets", "tbats", "theta", "nn", "wn"
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
levels = c("overall",  "snaive", "rw", "rwd", "ARMA.AR.MA","ARIMA", "SARIMA",
            "stlar", "mstlets", "tbats", "theta", "nn", "wn"),
labels = c( "overall", "snaive", "rw", "rwd", "ARMA.AR.MA","ARIMA", "SARIMA",
            "stlar", "mstlets", "tbats", "theta", "nn", "wn"))

meanrank_weekly <- meanrank_weekly %>%
  mutate(class=recode(class, "overall"="overall", "snaive"="snaive", "rw"="rw",
                      "rwd"="rwd", "ARMA.AR.MA"="ARMA", "ARIMA"="ARIMA", "SARIMA"="SARIMA",
                      "stlar"="stlar", "mstlets"="mstlets", "tbats"="tbats", "theta"="theta", "nn"="nn", "wn"="wn"))

meanrank_weekly$rn <- 1:351
topq <- meanrank_weekly %>%
  group_by(class) %>%
  top_n(n = 5, wt = rank)
meanrank_weekly$istop <- ifelse(meanrank_weekly$rn %in% topq$rn, TRUE, FALSE)
feaImp_weekly <- ggplot(meanrank_weekly, aes(y = rank, x = feature,fill=as.factor(istop))) +
  geom_bar(position = "dodge", stat = "identity", width=0.3) +
  facet_wrap(~class, ncol = 7, nrow = 2) +
  coord_flip() + ylab("Average rank")+ 
  scale_fill_manual(breaks=c("0","1"), values=c("#f1a340","#998ec3"), guide="none")+
  theme(text=element_text(size = 20))
feaImp_weekly

## ---- weeklypdp
## load ICE calculation files
## spikines
load("data/weekly/pdp_ice/spikinessW_includeout.rda")
## Arrange graphs for faceting
keep.modelnames <- c("snaive", "rw", "rwd", "ARMA.AR.MA","ARIMA", "SARIMA",
                     "stlar", "mstlets", "tbats", "theta", "nn", "wn")
keepspikiness <- c(keep.modelnames, "spikiness")
spikinessW_includeout <- spikinessW_includeout[, names(spikinessW_includeout) %in% keepspikiness]
spikiness_long <- gather(spikinessW_includeout, class, probability, "ARIMA":"wn", factor_key = TRUE)

spikiness_long <- spikiness_long %>%
  mutate(class = recode(class, "snaive"="snaive", "rw"="rw",
                        "rwd"="rwd", "ARMA.AR.MA"="ARMA", "ARIMA"="ARIMA", "SARIMA"="SARIMA",
                        "stlar"="stlar", "mstlets"="mstlets", "tbats"="tbats", "theta"="theta", "nn"="nn", "wn"="wn"))
spikiness_long$class <- factor(spikiness_long$class,
                               levels = c("snaive", "rw", "rwd", "ARMA","ARIMA", "SARIMA",
                                          "stlar", "mstlets", "tbats", "theta", "nn", "wn"))

plot_pdp_weekly_spikiness <- ggplot(data = spikiness_long, aes_string(x = spikiness_long$spikiness, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=18), axis.title = element_text(size = 16))+
  facet_grid(. ~ class)+theme(strip.text.x = element_text(size = 10))+xlab("spikiness")+ylab("probability of selecting forecast-models")
plot_pdp_weekly_spikiness


# ## ---- friedmanHW
# load("data/friedmanHstat_weekly.rda")
# col.order <- c("spikiness", "linearity", "trend", "seasonality", "stability",
#                "lumpiness", "curvature", "sediff_acf5", "entropy", "beta","y_pacf5",
#                "seas_pacf", "N", "diff2y_acf5", "nonlinearity", "sediff_seacf1",
#                "y_acf5", "diff1y_pacf5", "y_acf1", "diff1y_acf5", "diff2y_pacf5", 
#                "e_acf1", "sediff_acf1", "diff2y_acf1", "hurst", "alpha", "diff1y_acf1")
#   
# ## snaive
# snaive_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="snaive",]
# snaive_WFH_cormat <- friedmanHstat_matrix(snaive_WFH, 27, rev(col.order))
# ## rwd
# rwd_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="rwd",]
# rwd_WFH_cormat <- friedmanHstat_matrix(rwd_WFH, 27, rev(col.order))
# ## rw
# rw_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="rw",]
# rw_WFH_cormat <- friedmanHstat_matrix(rw_WFH, 27, rev(col.order))
# ## ARIMA
# arima_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="ARIMA",]
# arima_WFH_cormat <- friedmanHstat_matrix(arima_WFH, 27, rev(col.order))
# ## SARIMA
# sarima_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="SARIMA",]
# sarima_WFH_cormat <- friedmanHstat_matrix(sarima_WFH, 27, rev(col.order))
# ## stlar
# stlar_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="stlar",]
# stlar_WFH_cormat <- friedmanHstat_matrix(stlar_WFH, 27, rev(col.order))
# ## mstlets
# mstlets_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="mstlets",]
# mstlets_WFH_cormat <- friedmanHstat_matrix(mstlets_WFH, 27, rev(col.order))
# ## tbats
# tbats_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="tbats",]
# tbats_WFH_cormat <- friedmanHstat_matrix(tbats_WFH, 27, rev(col.order))
# ## ARMA
# arma_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="ARMA.AR.MA",]
# arma_WFH_cormat <- friedmanHstat_matrix(arma_WFH, 27, rev(col.order))
# ## wn
# wn_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="wn",]
# wn_WFH_cormat <- friedmanHstat_matrix(wn_WFH, 27, rev(col.order))
# ## theta
# theta_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="theta",]
# theta_WFH_cormat <- friedmanHstat_matrix(theta_WFH, 27, rev(col.order))
# ## nn
# nn_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="nn",]
# nn_WFH_cormat <- friedmanHstat_matrix(nn_WFH, 27, rev(col.order))
# friedman.weekly.mean <- (snaive_WFH_cormat + rwd_WFH_cormat + rw_WFH_cormat +
#                   arima_WFH_cormat + sarima_WFH_cormat + stlar_WFH_cormat+
#                   mstlets_WFH_cormat + tbats_WFH_cormat + arma_WFH_cormat +
#                   wn_WFH_cormat + theta_WFH_cormat + nn_WFH_cormat)/12
# fried.mat.weekly <- ggcorrplot(friedman.weekly.mean, hc.order = TRUE, type = "upper",
#                                 outline.col = "white")+
#   scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
#                        high = "#ef8a62", low = "#f7f7f7",
#                        name = "Friedman's H-statistic")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 1, 
#                                    size = 12, hjust = 1),
#         panel.margin=unit(c(0,0,0,0), "null"),
#         plot.margin=unit(c(0,0,0,0), "null"))
# fried.mat.weekly


## ---- friedmanw
## Overall interaction plot
load("data/weekly/overall_interactions_w.rda")
overall_interactions_w$.interaction[overall_interactions_w$.interaction > 1.0] <- 0
overall_interactions_w <- overall_interactions_w %>% mutate(.class = recode(.class,
                                                                            "snaive"="snaive", "rw"="rw",
                                                                            "rwd"="rwd", "ARMA.AR.MA"="ARMA", "ARIMA"="ARIMA", "SARIMA"="SARIMA",
                                                                            "stlar"="stlar", "mstlets"="mstlets", "tbats"="tbats", "theta"="theta", "nn"="nn", "wn"="wn"))
# new addition to arrange labels
overall_interactions_w$.class <- factor(overall_interactions_w$.class, levels = c(
  "snaive", "rw", "rwd", "ARMA","ARIMA", "SARIMA",
  "stlar", "mstlets", "tbats", "theta", "nn", "wn"
))

# arrange features according to the order of rw class
orderSNAIVE <- filter(overall_interactions_w, .class == "snaive")
overall_interactions_w$.feature <- factor(overall_interactions_w$.feature,
                                          levels = orderSNAIVE$.feature[order(orderSNAIVE$.interaction)])
top <- overall_interactions_w %>%
  group_by(.class) %>%
  top_n(n = 5, wt = .interaction)

overall_interactions_w$istop <- ifelse(overall_interactions_w$.interaction%in%top$.interaction, TRUE, FALSE)


colnames(overall_interactions_w) <- c("feature", "class", "interaction", "istop")

FHinteraction_weekly <- ggplot(overall_interactions_w, 
                                  aes(y = interaction, x = feature, fill=as.factor(istop))) +
  geom_bar(position = "dodge", stat = "identity", width=0.3) +
  facet_wrap(~ class, ncol = 6, nrow = 2) +
  coord_flip() + ylab("Overall interaction strength (Friedman's H-Statistic)")+
  scale_fill_manual(breaks=c("0","1"), values=c("#7fbf7b","#af8dc3"), guide="none")+
  theme(text=element_text(size = 20), axis.text.x = element_text(angle = 90, hjust = 1))
FHinteraction_weekly




## ---- intweekly
load("data/weekly/curvature.linearity.w.rda")
colNameste <- colnames(curvature.linearity.w)[29:40]

keep.modelnames <- c("snaive", "rw", "rwd", "ARMA.AR.MA","ARIMA", "SARIMA",
                     "stlar", "mstlets", "tbats", "theta", "nn", "wn")
keepw <- c(keep.modelnames, c("curvature", "linearity"))
curvature.linearity.w <- curvature.linearity.w[, names(curvature.linearity.w) %in% keepw]
curvature.linearity.w.long <- gather(curvature.linearity.w, class, probability, "ARIMA":"wn", factor_key = TRUE)
curvature.linearity.w.long <- curvature.linearity.w.long %>%
  mutate(class = recode(class, "snaive"="snaive", "rw"="rw",
                        "rwd"="rwd", "ARMA.AR.MA"="ARMA", "ARIMA"="ARIMA", "SARIMA"="SARIMA",
                        "stlar"="stlar", "mstlets"="mstlets", "tbats"="tbats", "theta"="theta", "nn"="nn", "wn"="wn"))
curvature.linearity.w.long$class <- factor(curvature.linearity.w.long$class,
                                           levels = c("snaive", "rw", "rwd", "ARMA","ARIMA", "SARIMA",
                                                      "stlar", "mstlets", "tbats", "theta", "nn", "wn"))


curvature.linearity.w.long %>%
  ggplot(aes(x = curvature, y = linearity, fill = probability)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~class, ncol=6) +
  scale_fill_viridis_c(option = "A", direction = -1, breaks=c(0,0.15,100),
                       limits=c(0,0.15))+
  theme(strip.text.x = element_text(size = 18))



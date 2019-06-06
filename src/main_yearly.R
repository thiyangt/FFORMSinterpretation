## codes to reproduce yearly data results
## ---- yearlyoob
load("data/yearly/yearly_training.rda") # random forest training set 
load("data/yearly/train_votes.rda") # oob votes from the random forest (see: yearly_cluster_results for more info)
load("data/yearly/train_predictions_oob.rda") # based on oob prediction (see: yearly_cluster_results for more info)
votes_oob <- data.frame(train_votes)
names(votes_oob) <- names(table(train_predictions_oob))
votes_oob$predicted <- train_predictions_oob
votes_oob$classlabel <- yearly_training$classlabels
votes_oob <- votes_oob %>%
  mutate(id = seq_len(n())) %>%
  melt(id.var = c("classlabel", "id", "predicted"), na.rm = T) %>%
  select(-id)
votes_oob <- votes_oob %>%
  mutate(classlabel = recode(classlabel, nn="nn",
    theta = "theta", wn = "wn", "ARMA/AR/MA" = "ARMA", ARIMA = "ARIMA", "ETS-notrendnoseasonal" = "ETS_NTNS",
    "ETS-dampedtrend" = "ETS_DT", "ETS-trend" = "ETS_T", "rwd" = "rwd", "rw" = "rw" ))

votes_oob <- votes_oob %>%
  mutate(predicted = recode(predicted, nn="nn", theta = "theta",
                             wn = "wn", "ARMA/AR/MA" = "ARMA", ARIMA = "ARIMA",
                             "ETS-notrendnoseasonal" = "ETS_NTNS", "ETS-dampedtrend" = "ETS_DT",
                             "ETS-trend" = "ETS_T","rwd" = "rwd", "rw" = "rw" ))

votes_oob <- votes_oob %>%
  mutate(variable = recode(variable, nn="nn", theta = "theta", wn = "wn", "ARMA/AR/MA" = "ARMA",
                            ARIMA = "ARIMA", "ETS-notrendnoseasonal" = "ETS_NTNS", "ETS-dampedtrend" = "ETS_DT",
                            "ETS-trend" = "ETS_T", "rwd" = "rwd", "rw" = "rw" ))
# arrange labels
votes_oob$variable <- factor(votes_oob$variable,
                               levels = rev(c(
                                 "nn",
                                 "theta",
                                 "wn",
                                 "ARMA",
                                 "ARIMA",
                                 "ETS_NTNS",
                                 "ETS_DT",
                                 "ETS_T",
                                 "rwd",
                                 "rw" )))

oob_boxplot_yearly <- ggplot(votes_oob, aes(x = classlabel, y = log(value), fill = classlabel)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.4) +
  ylab("Proportion") +
  xlab("") +
  theme(legend.position = "none", legend.title = element_blank(), 
        legend.text.align = 0, text = element_text(size = 25), axis.text.x = element_text(angle = 90),
        strip.text = element_text(size = 20)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = c("nn", "theta", "wn", "ARMA", "ARIMA", "ETS_NTNS", "ETS_DT", "ETS_T", "rwd", "rw" )) +
  coord_flip() + facet_wrap(. ~ variable, ncol=5)
oob_boxplot_yearly

## ---- viyearly
# All variable scores into one dataframe
load("data/yearly/train_importance.rda")
load(file = "data/yearly/sd_pdf_df.rda")
load(file = "data/yearly/sd_ice_df.rda")
## Permutation based
# head(train_importance)
# class(train_importance) #matrix
train_imp_df <- data.frame(train_importance)
train_imp_df <- add_rownames(train_imp_df, "Feature")
# names(train_imp_df)
train_imp_df <- within(train_imp_df, rm("MeanDecreaseAccuracy", "MeanDecreaseGini"))
permutation_imp <- train_imp_df %>% melt(id.vars = "Feature")
# head(permutation_imp)
# dim(permutation_imp) # 250 3
colnames(permutation_imp) <- c("feature", "class", "score")

## PDP-based
# head(sd_pdf_df)
sd_pdf_df <- add_rownames(sd_pdf_df, "class")
# head(sd_pdf_df) %>% data.frame()
pdp_imp <- sd_pdf_df %>% melt(id.vars = "class")
# head(pdp_imp)
colnames(pdp_imp) <- c("class", "feature", "score")
# dim(pdp_imp) # 250 3

## ICE-based
# head(sd_ice_df)
sd_ice_df <- add_rownames(sd_ice_df, "class")
# head(sd_ice_df) %>% data.frame()
ice_imp <- sd_ice_df %>% melt(id.vars = "class")
# head(ice_imp)
colnames(ice_imp) <- c("class", "feature", "score")
# dim(ice_imp) # 250 3

## Combine the data frames
importancescoreY <- bind_rows(permutation_imp, pdp_imp)
importancescoreY <- bind_rows(importancescoreY, ice_imp)
importancescoreY$VI <- rep(c("permutation", "PDP", "ICE"), each = 250)

## rank permutation, sd_pdp, and sd_ice scores for each class
importancescoreY$class <- factor(importancescoreY$class,
                                 levels = c("rw", "rwd", "ETS.trend", "ETS.dampedtrend", "ETS.notrendnoseasonal", "ARIMA", "ARMA.AR.MA", "wn", "theta", "nn"),
                                 labels = c("rw", "rwd", "ETS.trend", "ETS.dampedtrend", "ETS.notrendnoseasonal", "ARIMA", "ARMA.AR.MA", "wn", "theta", "nn")
)
rank_vi_yearly_classes <- importancescoreY %>%
  group_by(VI, class) %>%
  mutate(rank = min_rank(score))
## compute mean rank
meanrank_viy_classes <- rank_vi_yearly_classes %>% group_by(feature, class) %>% summarise_at(vars(c(rank)), funs(mean))

## overal importance of features to the forest
train_impforest <- data.frame(train_importance)
train_impforest <- add_rownames(train_impforest, "Feature")
train_impforest <- train_impforest[, c("Feature", "MeanDecreaseAccuracy", "MeanDecreaseGini")]
# head(train_impforest)
train_impforest <- train_impforest %>%
  mutate(rank_permu = min_rank(MeanDecreaseAccuracy)) %>%
  mutate(rank_gini = min_rank(MeanDecreaseGini))
train_impforest$mean_rank <- (train_impforest$rank_permu + train_impforest$rank_gini) / 2
meanrank_viy_forest <- data.frame(
  feature = train_impforest$Feature,
  class = rep("overall", 25),
  rank = train_impforest$mean_rank
)
## combine mean ranks for overall forest and separate classes
meanrank_yearly <- dplyr::bind_rows(meanrank_viy_forest, meanrank_viy_classes)
## create horizontal bar chart for ranks
orderOverall <- filter(meanrank_yearly, class == "overall")
meanrank_yearly$feature <- factor(meanrank_yearly$feature, levels = orderOverall$feature[order(orderOverall$rank)])
meanrank_yearly$class <- factor(meanrank_yearly$class,
                                levels = c(
                                  "overall", "rw",
                                  "rwd",
                                  "ETS.trend",
                                  "ETS.dampedtrend",
                                  "ETS.notrendnoseasonal",
                                  "ARIMA",
                                  "ARMA.AR.MA",
                                  "wn",
                                  "theta",
                                  "nn" ))

meanrank_yearly <- meanrank_yearly %>%
  mutate(class = recode(class, nn="nn",
                           theta = "theta",
                           wn = "wn",
                           "ARMA.AR.MA" = "ARMA",
                           ARIMA = "ARIMA",
                           "ETS.notrendnoseasonal" = "ETS_NTNS",
                           "ETS.dampedtrend" = "ETS_DT",
                           "ETS.trend" = "ETS_T",
                           "rwd" = "rwd",
                           "rw" = "rw" ))

meanrank_yearly$rn <- 1:275

top <- meanrank_yearly %>%
  group_by(class) %>%
  top_n(n = 5, wt = rank)

meanrank_yearly$istop <- ifelse(meanrank_yearly$rn%in%top$rn, TRUE, FALSE)

feaImp_yearly <- ggplot(meanrank_yearly, aes(y = rank, x = feature, fill=as.factor(istop))) +
  geom_bar(position = "dodge", stat = "identity", width=0.3) +
  facet_wrap(~ class, ncol = 6, nrow = 2) +
  coord_flip() + ylab("Average rank")+ 
  scale_fill_manual(breaks=c("0","1"), values=c("#f1a340","#998ec3"), guide="none")+theme(text=element_text(size = 20))+
  theme(strip.text.x = element_text(size = 18))
feaImp_yearly

## ---- pdpyearlyurpp
load("data/yearly/pdp_yearly/ur_ppgrid_rmout.rda")
## Arrange graphs for faceting
keep.modelnames <- c("ARIMA", "ARMA.AR.MA", "ETS.dampedtrend", "ETS.notrendnoseasonal",
                     "ETS.trend", "nn", "rw", "rwd", "theta", "wn")
keepur <- c(keep.modelnames, "ur_pp")
ur_ppgrid_rmout <- ur_ppgrid_rmout[, names(ur_ppgrid_rmout) %in% keepur]
ur_ppgrid_long <- gather(ur_ppgrid_rmout, class, probability, "ARIMA":"wn", factor_key = TRUE)

ur_ppgrid_long <- ur_ppgrid_long %>%
   mutate(class = recode(class, nn="nn",
                         theta = "theta",
                        wn = "wn",
                        "ARMA.AR.MA" = "ARMA",
                        ARIMA = "ARIMA",
                        "ETS.notrendnoseasonal" = "ETS_NTNS",
                        "ETS.dampedtrend" = "ETS_DT",
                         "ETS.trend" = "ETS_T",
                         "rwd" = "rwd",
                         "rw" = "rw" ))
ur_ppgrid_long$class <- factor(ur_ppgrid_long$class,
                                levels = c("rw", "rwd", "ETS_T", "ETS_DT", "ETS_NTNS",
                                  "ARIMA", "ARMA", "wn", "theta", "nn" ))

plot_pdp_yearly <- ggplot(data = ur_ppgrid_long, aes_string(x = ur_ppgrid_long$ur_pp, y = "probability")) +
   stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) +
   stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=18), axis.title = element_text(size = 16))+
   facet_grid(. ~ class)+theme(strip.text.x = element_text(size = 10))+xlab("test statistic based on Phillips-Perron unit root test (ur_pp)")+ylab("probability of selecting forecast-models")
plot_pdp_yearly

## ---- pdpyearlytrend
load("data/yearly/pdp_yearly/trendgrid.rda")
keeptrend <- c(keep.modelnames, "trend")
trendgrid <- trendgrid[, names(trendgrid) %in% keeptrend]
trendgrid_long <- gather(trendgrid, class, probability, "ARIMA":"wn", factor_key = TRUE)
trendgrid_long <- trendgrid_long %>%
  mutate(class = recode(class, nn="nn",
                        theta = "theta",
                        wn = "wn",
                        "ARMA.AR.MA" = "ARMA",
                        ARIMA = "ARIMA",
                        "ETS.notrendnoseasonal" = "ETS_NTNS",
                        "ETS.dampedtrend" = "ETS_DT",
                        "ETS.trend" = "ETS_T",
                        "rwd" = "rwd",
                        "rw" = "rw" ))
trendgrid_long$class <- factor(trendgrid_long$class,
                               levels = c("rw", "rwd", "ETS_T", "ETS_DT", "ETS_NTNS",
                                          "ARIMA", "ARMA", "wn", "theta", "nn" ))

plot_pdp_yearly_trend <- ggplot(data = trendgrid_long, aes_string(x = trendgrid_long$trend, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=18), axis.title = element_text(size = 16))+
  facet_grid(. ~ class)+theme(strip.text.x = element_text(size = 10))+xlab("strength of trend")+ylab("probability of selecting forecast-models")
plot_pdp_yearly_trend

## ---- pdpyearlylinearity
load("data/yearly/pdp_yearly/linearitygrid.rda")
keeplinearity <- c(keep.modelnames, "linearity")
linearitygrid <- linearitygrid[, names(linearitygrid) %in% keeplinearity]
linearitygrid_long <- gather(linearitygrid, class, probability, "ARIMA":"wn", factor_key = TRUE)
linearitygrid_long <- linearitygrid_long %>%
  mutate(class = recode(class, nn="nn",
                        theta = "theta",
                        wn = "wn",
                        "ARMA.AR.MA" = "ARMA",
                        ARIMA = "ARIMA",
                        "ETS.notrendnoseasonal" = "ETS_NTNS",
                        "ETS.dampedtrend" = "ETS_DT",
                        "ETS.trend" = "ETS_T",
                        "rwd" = "rwd",
                        "rw" = "rw" ))

linearitygrid_long$class <- factor(linearitygrid_long$class,
                               levels = c("rw", "rwd", "ETS_T", "ETS_DT", "ETS_NTNS",
                                          "ARIMA", "ARMA", "wn", "theta", "nn" ))

plot_pdp_yearly_linearity <- ggplot(data = linearitygrid_long, aes_string(x = linearitygrid_long$linearity, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=18), axis.title = element_text(size = 16))+
  facet_grid(. ~ class)+theme(strip.text.x = element_text(size = 10))+xlab("strength of linearity")+ylab("probability of selecting forecast-models")
plot_pdp_yearly_linearity+xlim(-8,8)


## ---- friedmany
# Two-way interaction between each combination
# load("data/friedmanHstat_yearly.rda")
# col.order <- c("trend", "ur_pp","spikiness", "beta",
#                "diff1y_acf1", "linearity", "diff1y_acf5", "curvature",
#                "lmres_acf1","y_pacf5", "ur_kpss", "y_acf1", "nonlinearity",
#                "alpha", "diff1y_pacf5", "hurst", "entropy", "e_acf1", "y_acf5",
#                "diff2y_pacf5",
#                "diff2y_acf1", "N", "diff2y_acf5", "lumpiness", "stability")
# 
# ## random walk with drift
# rwd_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="rwd",]
# rwd_YFH_cormat <- friedmanHstat_matrix(rwd_YFH, 25, rev(col.order))
# ## random walk
# rw_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="rw",]
# rw_YFH_cormat <- friedmanHstat_matrix(rw_YFH, 25, rev(col.order))
# ## ETS-trend
# etst_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="ETS.trend",]
# etst_YFH_cormat <- friedmanHstat_matrix(etst_YFH, 25, rev(col.order))
# ## ETS-dampedtrend
# etsdt_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="ETS.dampedtrend",]
# etsdt_YFH_cormat <- friedmanHstat_matrix(etsdt_YFH, 25, rev(col.order))
# ## ETS-notrendnoseasonal
# etsntns_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="ETS.notrendnoseasonal",]
# etsntns_YFH_cormat <- friedmanHstat_matrix(etsntns_YFH, 25, rev(col.order))
# ## ARIMA
# arima_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="ARIMA",]
# arima_YFH_cormat <- friedmanHstat_matrix(arima_YFH, 25, rev(col.order))
# ##  ARMA.AR.MA 
# arma_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="ARMA.AR.MA",]
# arma_YFH_cormat <- friedmanHstat_matrix(arma_YFH, 25, rev(col.order))
# ##  wn 
# wn_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="wn",]
# wn_YFH_cormat <- friedmanHstat_matrix(wn_YFH, 25, rev(col.order))
# ## theta
# theta_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="theta",]
# theta_YFH_cormat <- friedmanHstat_matrix(theta_YFH, 25, rev(col.order))
# ## nn
# nn_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="nn",]
# nn_YFH_cormat <- friedmanHstat_matrix(nn_YFH, 25, rev(col.order))
# ##visualize friedman matrix plot
# friedman.yearly.mean <- (rwd_YFH_cormat + rw_YFH_cormat + etst_YFH_cormat + 
#                            etsdt_YFH_cormat + etsntns_YFH_cormat + arima_YFH_cormat +
#                            arma_YFH_cormat + wn_YFH_cormat + theta_YFH_cormat + nn_YFH_cormat)/10
# 
# # friedman.yearly.mean2 <- friedman.yearly.mean[ c("ur_pp", "linearity", "trend", "lmres_acf1", "N",
# #                                                   "ur_kpss", "stability", "lumpiness", "curvature", "diff1y_acf5",
# #                                                   "spikiness", "entropy", "diff1y_pacf5", "diff2y_acf1", "e_acf1",
# #                                                   "diff2y_pacf5", "diff2y_acf5", "nonlinearity", "y_acf5", "hurst",
# #                                                   "alpha", "y_acf1", "y_pacf5", "diff1y_acf1", "beta"), ]
# fried.mat.yearly <- ggcorrplot(friedman.yearly.mean, hc.order = TRUE, type = "lower",
#                                 outline.col = "white")+
#    scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,100), 
#                         high = "#ef8a62", low = "#f7f7f7",
#                         name = "Friedman's H-statistic")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 1, 
#                                     size = 12, hjust = 1))
# fried.mat.yearly

## Overall interaction plot
load("data/yearly/overall_interactions_y.rda")
overall_interactions_y$".class" <- factor(overall_interactions_y$.class,
                                levels = c(
                                  "rw",
                                  "rwd",
                                  "ETS.trend",
                                  "ETS.dampedtrend",
                                  "ETS.notrendnoseasonal",
                                  "ARIMA",
                                  "ARMA.AR.MA",
                                  "wn",
                                  "theta",
                                  "nn" ))

overall_interactions_y <- overall_interactions_y %>%
  mutate(.class = recode(.class, nn="nn",
                        theta = "theta",
                        wn = "wn",
                        "ARMA.AR.MA" = "ARMA",
                        ARIMA = "ARIMA",
                        "ETS.notrendnoseasonal" = "ETS_NTNS",
                        "ETS.dampedtrend" = "ETS_DT",
                        "ETS.trend" = "ETS_T",
                        "rwd" = "rwd",
                        "rw" = "rw" ))
# arrange features according to the order of rw class
orderRW <- filter(overall_interactions_y, .class == "rw")
overall_interactions_y$.feature <- factor(overall_interactions_y$.feature,
              levels = orderRW$.feature[order(orderRW$.interaction)])
top <- overall_interactions_y %>%
  group_by(.class) %>%
  top_n(n = 5, wt = .interaction)

overall_interactions_y$istop <- ifelse(overall_interactions_y$.interaction%in%top$.interaction, TRUE, FALSE)

FHinteraction_yearly <- ggplot(overall_interactions_y, 
                        aes(y = .interaction, x = .feature, fill=as.factor(istop))) +
  geom_bar(position = "dodge", stat = "identity", width=0.3) +
  facet_wrap(~ .class, ncol = 5, nrow = 2) +
  coord_flip() + ylab("Overall interaction strength(Friedman's H-Statistic)")+
  scale_fill_manual(breaks=c("0","1"), values=c("#7fbf7b","#af8dc3"), guide="none")+
  theme(text=element_text(size = 20), axis.text.x = element_text(angle = 90, hjust = 1))
FHinteraction_yearly


## ---- intyearly
load("data/yearly/lmres_acf1.diff1y_acf1.y.rda")
colNamesls <- colnames(lmres_acf1.diff1y_acf1.y)[27:36]

keep.modelnames <- c("ARIMA", "ARMA.AR.MA", "ETS.dampedtrend", "ETS.notrendnoseasonal",
                     "ETS.trend", "nn", "rw", "rwd", "theta", "wn")
keepy <- c(keep.modelnames, c("lmres_acf1", "diff1y_acf1"))
lmres_acf1.diff1y_acf1.y <- lmres_acf1.diff1y_acf1.y[, names(lmres_acf1.diff1y_acf1.y) %in% keepy]
lmres_acf1.diff1y_acf1.y.long <- gather(lmres_acf1.diff1y_acf1.y, class, probability, "ARIMA":"wn", factor_key = TRUE)
lmres_acf1.diff1y_acf1.y.long <- lmres_acf1.diff1y_acf1.y.long %>%
  mutate(class = recode(class, nn="nn",
                        theta = "theta",
                        wn = "wn",
                        "ARMA.AR.MA" = "ARMA",
                        ARIMA = "ARIMA",
                        "ETS.notrendnoseasonal" = "ETS_NTNS",
                        "ETS.dampedtrend" = "ETS_DT",
                        "ETS.trend" = "ETS_T",
                        "rwd" = "rwd",
                        "rw" = "rw" ))
lmres_acf1.diff1y_acf1.y.long$class <- factor(lmres_acf1.diff1y_acf1.y.long$class,
                                            levels = c("rw", "rwd", "ETS_T", "ETS_DT", "ETS_NTNS",
                                                       "ARIMA", "ARMA", "wn", "theta", "nn" ))

lmres_acf1.diff1y_acf1.y.long %>%
  ggplot(aes(x = lmres_acf1, y = diff1y_acf1, fill = probability)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~class, ncol=5) +
  scale_fill_viridis_c(option = "A", direction = -1, breaks=c(0,0.21,100),
                       limits=c(0,0.21))+
  theme(strip.text.x = element_text(size = 10))




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
## rwd
rwd_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="rwd",]
rwd_WFH_cormat <- friedmanHstat_matrix(rwd_WFH, 27, rev(col.order))
## rw
rw_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="rw",]
rw_WFH_cormat <- friedmanHstat_matrix(rw_WFH, 27, rev(col.order))
## ARIMA
arima_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="ARIMA",]
arima_WFH_cormat <- friedmanHstat_matrix(arima_WFH, 27, rev(col.order))
## SARIMA
sarima_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="SARIMA",]
sarima_WFH_cormat <- friedmanHstat_matrix(sarima_WFH, 27, rev(col.order))
## stlar
stlar_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="stlar",]
stlar_WFH_cormat <- friedmanHstat_matrix(stlar_WFH, 27, rev(col.order))
## mstlets
mstlets_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="mstlets",]
mstlets_WFH_cormat <- friedmanHstat_matrix(mstlets_WFH, 27, rev(col.order))
## tbats
tbats_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="tbats",]
tbats_WFH_cormat <- friedmanHstat_matrix(tbats_WFH, 27, rev(col.order))
## ARMA
arma_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="ARMA.AR.MA",]
arma_WFH_cormat <- friedmanHstat_matrix(arma_WFH, 27, rev(col.order))
## wn
wn_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="wn",]
wn_WFH_cormat <- friedmanHstat_matrix(wn_WFH, 27, rev(col.order))
## theta
theta_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="theta",]
theta_WFH_cormat <- friedmanHstat_matrix(theta_WFH, 27, rev(col.order))
## nn
nn_WFH <- friedmanHstat_weekly[friedmanHstat_weekly$class=="nn",]
nn_WFH_cormat <- friedmanHstat_matrix(nn_WFH, 27, rev(col.order))
friedman.weekly.mean <- (snaive_WFH_cormat + rwd_WFH_cormat + rw_WFH_cormat +
                  arima_WFH_cormat + sarima_WFH_cormat + stlar_WFH_cormat+
                  mstlets_WFH_cormat + tbats_WFH_cormat + arma_WFH_cormat +
                  wn_WFH_cormat + theta_WFH_cormat + nn_WFH_cormat)/12
fried.mat.weekly <- ggcorrplot(friedman.weekly.mean, hc.order = TRUE, type = "upper",
                                outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7",
                       name = "Friedman's H-statistic")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1),
        panel.margin=unit(c(0,0,0,0), "null"),
        plot.margin=unit(c(0,0,0,0), "null"))
fried.mat.weekly


## ---- intweekly
load("data/weekly/trend.entropy.w.rda")
colNameste <- colnames(trend.entropy.w)[29:40]

keep.modelnames <- c("snaive", "rw", "rwd", "ARMA.AR.MA","ARIMA", "SARIMA",
                     "stlar", "mstlets", "tbats", "theta", "nn", "wn")
keepw <- c(keep.modelnames, c("trend", "entropy"))
trend.entropy.w <- trend.entropy.w[, names(trend.entropy.w) %in% keepw]
trend.entropy.w.long <- gather(trend.entropy.w, class, probability, "ARIMA":"wn", factor_key = TRUE)
trend.entropy.w.long <- trend.entropy.w.long %>%
  mutate(class = recode(class, "snaive"="snaive", "rw"="rw",
                        "rwd"="rwd", "ARMA.AR.MA"="ARMA", "ARIMA"="ARIMA", "SARIMA"="SARIMA",
                        "stlar"="stlar", "mstlets"="mstlets", "tbats"="tbats", "theta"="theta", "nn"="nn", "wn"="wn"))
trend.entropy.w.long$class <- factor(trend.entropy.w.long$class,
                                           levels = c("snaive", "rw", "rwd", "ARMA","ARIMA", "SARIMA",
                                                      "stlar", "mstlets", "tbats", "theta", "nn", "wn"))


trend.entropy.w.long %>%
  ggplot(aes(x = trend, y = entropy, fill = probability)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~class, ncol=6) +
  scale_fill_viridis_c(option = "A", direction = -1)+
  theme(strip.text.x = element_text(size = 18))




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

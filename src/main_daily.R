## ---- oobdaily
load("data/daily/trainD_votes.rda") #oob votes from the random forest
load("data/daily/trainD_predictions_oob.rda") # based on oob prediction
load("data/daily/daily_training.rda") # random forest training set
votes_oobD <- data.frame(trainD_votes)
names(votes_oobD) <- names(table(trainD_predictions_oob))
votes_oobD$predicted <- trainD_predictions_oob
votes_oobD$classlabel <- daily_training$classlabels
votes_oobD <- votes_oobD %>% mutate(id=seq_len(n())) %>%
  melt(id.var=c('classlabel','id','predicted'), na.rm=T) %>%
  select(-id)
#new addition to arrange labels 
votes_oobD$variable <- factor(votes_oobD$variable, 
                              levels=c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                       "theta","nn","wn")
)
oob_boxplot_daily <- ggplot(votes_oobD, aes(x = classlabel, y = log(value), fill = classlabel)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.4) +
  ylab("Proportion") +
  xlab("") +
  theme(legend.position = "none", legend.title = element_blank(), 
        legend.text.align = 0, text = element_text(size = 25), axis.text.x = element_text(angle = 90),
        strip.text = element_text(size = 20)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = rev(c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                  "theta","nn","wn" ))) +
  coord_flip() + facet_wrap(. ~ variable, ncol=5)
oob_boxplot_daily


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
  geom_bar(position = "dodge", stat = "identity", width=0.3) +
  facet_wrap(~class, ncol = 6, nrow = 2) +
  coord_flip() + ylab("Average rank")+ 
  scale_fill_manual(breaks=c("0","1"), values=c("#f1a340","#998ec3"), guide="none")+
  theme(text=element_text(size = 20))
feaImp_daily

## ---- dailypdpstability
## stability
load("data/daily/pdp_ice_daily/stabilityD_includeout.rda")
## Arrange graphs for faceting
keep.modelnames <- c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                     "theta","nn","wn")
keepstability <- c(keep.modelnames, "stability")
stabilitydaily <- stabilityD_includeout[, names(stabilityD_includeout) %in% keepstability]
stabilitydaily_long <- gather(stabilitydaily , class, probability,  "mstlarima":"wn", factor_key = TRUE)
stabilitydaily_long$class <- factor(stabilitydaily_long$class,
                                      levels = c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                                 "theta","nn","wn"))

plot_pdp_hourly_stability <- ggplot(data = stabilitydaily_long, aes_string(x = stabilitydaily_long$stability, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=16), axis.title = element_text(size = 16))+
  facet_grid(. ~ class)+theme(strip.text.x = element_text(size = 16))+xlab("stability")+ylab("probability of selecting forecast-models")
plot_pdp_hourly_stability


## ---- dailypdpN
## N
load("data/daily/pdp_ice_daily/ND_includeout.rda")
## Arrange graphs for faceting
keep.modelnames <- c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                     "theta","nn","wn")
keepN <- c(keep.modelnames, "N")
ND_includeout1 <- ND_includeout[, names(ND_includeout) %in% keepN]
ND_includeout1_long <- gather(ND_includeout1 , class, probability,  "mstlarima":"wn", factor_key = TRUE)
ND_includeout1_long$class <- factor(ND_includeout1_long$class,
                                      levels = c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                                 "theta","nn","wn"))

plot_pdp_hourly_N <- ggplot(data = ND_includeout1_long, aes_string(x = ND_includeout1_long$N, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=16), axis.title = element_text(size = 16))+
  facet_grid(. ~ class)+theme(strip.text.x = element_text(size = 16))+xlab("length (T)")+ylab("probability of selecting forecast-models")
plot_pdp_hourly_N


## ---- intdaily
load("data/daily/sediff_acf5.seasonal_strength2.d.rda")
colNamesss <- colnames(sediff_acf5.seasonal_strength2.d)[28:37]

keep.modelnames <- c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                     "theta","nn","wn")
keepd <- c(keep.modelnames, c("sediff_acf5", "seasonal_strength2"))
sediff_acf5.seasonal_strength2.d <- sediff_acf5.seasonal_strength2.d[, names(sediff_acf5.seasonal_strength2.d) %in% keepd]
sediff_acf5.seasonal_strength2.d.long <- gather(sediff_acf5.seasonal_strength2.d, class, probability, "mstlarima":"wn", factor_key = TRUE)
sediff_acf5.seasonal_strength2.d.long$class <- factor(sediff_acf5.seasonal_strength2.d.long$class,
                                               levels = c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                                          "theta","nn","wn"))

sediff_acf5.seasonal_strength2.d.long %>%
  ggplot(aes(x = sediff_acf5, y = seasonal_strength2, fill = probability)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~class, ncol=5) +
  scale_fill_viridis_c(option = "A", direction = -1)+
  theme(strip.text.x = element_text(size = 18))


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

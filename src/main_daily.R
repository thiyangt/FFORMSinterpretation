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
oob_boxplot_daily <- ggplot(votes_oobD, aes(x = classlabel, y = value, fill = classlabel)) +
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
levels(meanrank_daily$feature)[levels(meanrank_daily$feature)=="N"] <- "T"
levels(meanrank_daily$feature)[levels(meanrank_daily$feature)=="seasonal_strength1"] <- "seasonality_w"
levels(meanrank_daily$feature)[levels(meanrank_daily$feature)=="seasonal_strength2"] <- "seasonality_y"
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


## ---- friedmand
## Overall interaction plot
load("data/daily/overall_interactions_d.rda")
overall_interactions_d$.class <- factor(overall_interactions_d$.class, levels = c(
  "snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
  "theta","nn","wn"
))

# arrange features according to the order of rw class
orderSNAIVE <- filter(overall_interactions_d, .class == "snaive")
overall_interactions_d$.feature <- factor(overall_interactions_d$.feature,
                                          levels = orderSNAIVE$.feature[order(orderSNAIVE$.interaction)])
top <- overall_interactions_d %>%
  group_by(.class) %>%
  top_n(n = 5, wt = .interaction)

overall_interactions_d$istop <- ifelse(overall_interactions_d$.interaction%in%top$.interaction, TRUE, FALSE)
overall_interactions_d$.interaction[overall_interactions_d$.interaction > 1.0] <- 1
colnames(overall_interactions_d) <- c("feature", "class", "interaction", "istop")
levels(overall_interactions_d$feature)[levels(overall_interactions_d$feature)=="N"] <- "T"
levels(overall_interactions_d$feature)[levels(overall_interactions_d$feature)=="seasonal_strength1"] <- "seasonality_w"
levels(overall_interactions_d$feature)[levels(overall_interactions_d$feature)=="seasonal_strength2"] <- "seasonality_y"
FHinteraction_daily <- ggplot(overall_interactions_d, 
                               aes(y = interaction, x = feature, fill=as.factor(istop))) +
  geom_bar(position = "dodge", stat = "identity", width=0.3) +
  facet_wrap(~ class, ncol = 5, nrow = 2) +
  coord_flip() + ylab("Overall interaction strength (Friedman's H-Statistic)")+
  scale_fill_manual(breaks=c("0","1"), values=c("#7fbf7b","#af8dc3"), guide="none")+
  theme(text=element_text(size = 20), axis.text.x = element_text(angle = 90, hjust = 1))
FHinteraction_daily



## ---- intdaily
load("data/daily/stability.seasonal_strength2.d.rda")
colNamesss <- colnames(stability.seasonal_strength2.d)[28:37]

keep.modelnames <- c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                     "theta","nn","wn")
keepd <- c(keep.modelnames, c("stability", "seasonal_strength2"))
stability.seasonal_strength2.d <- stability.seasonal_strength2.d[, names(stability.seasonal_strength2.d) %in% keepd]
stability.seasonal_strength2.d.long <- gather(stability.seasonal_strength2.d, class, probability, "mstlarima":"wn", factor_key = TRUE)
stability.seasonal_strength2.d.long$class <- factor(stability.seasonal_strength2.d.long$class,
                                               levels = c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                                          "theta","nn","wn"))

stability.seasonal_strength2.d.long %>%
  ggplot(aes(x = stability, y = seasonal_strength2, fill = probability)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~class, ncol=5) +
  scale_fill_viridis_c(option = "A", direction = -1, breaks=c(0,0.2,100),
                       limits=c(0,0.2))+
  theme(strip.text.x = element_text(size = 10))+ylab("seasonal_w")


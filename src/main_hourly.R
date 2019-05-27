## ---- oobhourly
load("data/hourly/trainH_votes.rda") #oob votes from the random forest
load("data/hourly/trainH_predictions_oob.rda") # based on oob prediction
load("data/hourly/hourly_training.rda") # random forest training set
votes_oobH <- data.frame(trainH_votes)
names(votes_oobH) <- names(table(trainH_predictions_oob))
votes_oobH$predicted <- trainH_predictions_oob
votes_oobH$classlabel <- hourly_training$classlabels
votes_oobH <- votes_oobH %>% mutate(id=seq_len(n())) %>%
  melt(id.var=c('classlabel','id','predicted'), na.rm=T) %>%
  select(-id)
#new addition to arrange labels 
votes_oobH$variable <- factor(votes_oobH$variable, 
                                levels=c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                             "theta","nn","wn")
)
oob_boxplot_hourly <- ggplot(votes_oobH, aes(x = classlabel, y = value, fill = classlabel)) +
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
oob_boxplot_hourly


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
                               levels = c("overall","snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                 "theta","nn","wn"),
                               labels = c("overall","snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                 "theta","nn","wn"))

meanrank_hourly$rn <- 1:286
topq <- meanrank_hourly %>%
  group_by(class) %>%
  top_n(n = 5, wt = rank)
meanrank_hourly$istop <- ifelse(meanrank_hourly$rn %in% topq$rn, TRUE, FALSE)
feaImp_hourly <- ggplot(meanrank_hourly, aes(y = rank, x = feature,fill=as.factor(istop))) +
  geom_bar(position = "dodge", stat = "identity", width=0.3) +
  facet_wrap(~class, ncol = 6, nrow = 2) +
  coord_flip() + ylab("Average rank")+ 
  scale_fill_manual(breaks=c("0","1"), values=c("#f1a340","#998ec3"), guide="none")+
  theme(text=element_text(size = 20))
feaImp_hourly


## ---- seasonalityhourly
load("data/hourly/hiceout/seasonality1gridH.rda")
seasonality1gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/seasonality2gridH.rda")
seasonality2gridH$variable <- rep(1:1000, 20)
## Arrange graphs for faceting
keep.modelnames <- c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                     "theta","nn","wn")
keeps1 <- c(keep.modelnames, "seasonal_strength1")
keeps2 <- c(keep.modelnames, "seasonal_strength2")
seasonal1 <- seasonality1gridH[, names(seasonality1gridH) %in% keeps1]
seasonal1 <- rename(seasonal1, seasonal = seasonal_strength1) 
seasonal2 <- seasonality2gridH[, names(seasonality2gridH) %in% keeps2]
seasonal2 <- rename(seasonal2, seasonal = seasonal_strength2) 
seasonal1_long <- gather(seasonal1, class, probability, "mstlarima":"wn", factor_key = TRUE)
seasonal2_long <- gather(seasonal2, class, probability, "mstlarima":"wn", factor_key = TRUE)
seasonal1_long_mean <- seasonal1_long %>%
  group_by(seasonal, class) %>%
  summarise(n=n(), mean=mean(probability), sd=sd(probability)) %>%
  mutate(sem = sd/sqrt(n-1),
  CI_lower = mean+qt((1-0.95)/2, n-1)*sem,
  CI_upper = mean - qt((1-0.95)/2, n-1)*sem)
seasonal2_long_mean <- seasonal2_long %>%
  group_by(seasonal, class) %>%
  summarise(n=n(), mean=mean(probability), sd=sd(probability)) %>%
  mutate(sem = sd/sqrt(n-1),
         CI_lower = mean+qt((1-0.95)/2, n-1)*sem,
         CI_upper = mean - qt((1-0.95)/2, n-1)*sem)

seasonal_DW <- dplyr::bind_rows(seasonal1_long_mean, seasonal2_long_mean)
seasonal_DW$feature <- c(rep("seasonal_D (24)", 200), rep("seasonal_W (168)", 200))
seasonal_DW$class <- factor(seasonal_DW$class,
                               levels = c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                          "theta","nn","wn"))

plot_pdp_hourly_seasonal <- ggplot(seasonal_DW, aes(x=seasonal, y=mean, color=feature))+
  geom_line(aes(x=seasonal, y=mean, color=feature), size = 1)+
  geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper, fill=feature),alpha=0.4, colour = NA)+
  facet_grid(. ~ class)+
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=16), axis.title = element_text(size = 14))+
  theme(strip.text.x = element_text(size = 16))+xlab("strength of seasonality")+
  ylab("probability of selecting forecast-models")+
  theme(legend.position="bottom", legend.title=element_blank())+
  scale_colour_manual("",values=c("red", "blue"))+
  scale_fill_manual("",values=c("red", "blue"))
plot_pdp_hourly_seasonal

## ---- entropyhourly
load("data/hourly/hiceout/entropygridH.rda")
## Arrange graphs for faceting
keep.modelnames <- c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                     "theta","nn","wn")
keepentropy <- c(keep.modelnames, "entropy")
entropy1 <- entropygridH[, names(entropygridH) %in% keepentropy]
entropy1_long <- gather(entropy1, class, probability,  "mstlarima":"wn", factor_key = TRUE)
entropy1_long$class <- factor(entropy1_long$class,
                            levels = c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                       "theta","nn","wn"))

plot_pdp_hourly_entropy <- ggplot(data = entropy1_long, aes_string(x = entropy1_long$entropy, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=16), axis.title = element_text(size = 16))+
  facet_grid(. ~ class)+theme(strip.text.x = element_text(size = 16))+xlab("entropy")+ylab("probability of selecting forecast-models")
plot_pdp_hourly_entropy

## ---- linearityhourly
load("data/hourly/hiceout/linearitygridH.rda")
## Arrange graphs for faceting
keep.modelnames <- c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                     "theta","nn","wn")
keeplinearity <- c(keep.modelnames, "linearity")
linearityhourly1 <- linearitygridH[, names(linearitygridH) %in% keeplinearity]
linearityhourly1_long <- gather(linearityhourly1 , class, probability,  "mstlarima":"wn", factor_key = TRUE)
linearityhourly1_long$class <- factor(linearityhourly1_long$class,
                              levels = c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                         "theta","nn","wn"))

plot_pdp_hourly_linearity <- ggplot(data = linearityhourly1_long, aes_string(x = linearityhourly1_long$linearity, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=16), axis.title = element_text(size = 16))+
  facet_grid(. ~ class)+theme(strip.text.x = element_text(size = 16))+xlab("linearity")+ylab("probability of selecting forecast-models")
plot_pdp_hourly_linearity

## ---- curvaturehourly
load("data/hourly/hiceout/curvaturegridH.rda")
## Arrange graphs for faceting
keep.modelnames <- c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                     "theta","nn","wn")
keepcurvature <- c(keep.modelnames, "curvature")
curvaturehourly1 <- curvaturegridH[, names(curvaturegridH) %in% keepcurvature]
curvaturehourly1_long <- gather(curvaturehourly1 , class, probability,  "mstlarima":"wn", factor_key = TRUE)
curvaturehourly1_long$class <- factor(curvaturehourly1_long$class,
                                      levels = c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                                 "theta","nn","wn"))

plot_pdp_hourly_curvature <- ggplot(data = curvaturehourly1_long, aes_string(x = curvaturehourly1_long$curvature, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=16), axis.title = element_text(size = 16))+
  facet_grid(. ~ class)+theme(strip.text.x = element_text(size = 16))+xlab("curvature")+ylab("probability of selecting forecast-models")
plot_pdp_hourly_curvature


## ---- friedmanh
## Overall interaction plot
load("data/hourly/overall_interactions_h.rda")
overall_interactions_h$.class <- factor(overall_interactions_h$.class, levels = c(
  "snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
  "theta","nn","wn"
))

# arrange features according to the order of rw class
orderSNAIVE <- filter(overall_interactions_h, .class == "snaive")
overall_interactions_h$.feature <- factor(overall_interactions_h$.feature,
                                          levels = orderSNAIVE$.feature[order(orderSNAIVE$.interaction)])
top <- overall_interactions_h %>%
  group_by(.class) %>%
  top_n(n = 5, wt = .interaction)

overall_interactions_h$istop <- ifelse(overall_interactions_h$.interaction%in%top$.interaction, TRUE, FALSE)
overall_interactions_h$.interaction[overall_interactions_h$.interaction > 1.0] <- 1

colnames(overall_interactions_h) <- c("feature", "class", "interaction", "istop")

FHinteraction_hourly <- ggplot(overall_interactions_h, 
                               aes(y = interaction, x = feature, fill=as.factor(istop))) +
  geom_bar(position = "dodge", stat = "identity", width=0.3) +
  facet_wrap(~ class, ncol = 5, nrow = 2) +
  coord_flip() + ylab("Friedman's H-Statistic")+
  scale_fill_manual(breaks=c("0","1"), values=c("#7fbf7b","#af8dc3"), guide="none")+
  theme(text=element_text(size = 20), axis.text.x = element_text(angle = 90, hjust = 1))
FHinteraction_hourly


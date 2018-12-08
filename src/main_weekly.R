#################################################################
#                  Weekly data                                  #
#################################################################
## ---- oob_weekly
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

## ---- vi_weekly
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
  scale_fill_manual(breaks=c("0","1"), values=c("black","red"), guide="none")
feaImp_weekly

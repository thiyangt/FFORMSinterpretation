## ---- load_packages
library(ggplot2)
library(patchwork)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggrepel)
library(png)
library(tsfeatures)
library(tidyverse)
library(ggpubr)

## ---- yearly_oob
load("data/yearly/train_votes.rda") #oob votes from the random forest
load("data/yearly/train_predictions_oob.rda") # based on oob prediction
load("data/yearly/yearly_training.rda") # random forest training set
votes_oob <- data.frame(train_votes)
names(votes_oob) <- names(table(train_predictions_oob))
votes_oob$predicted <- train_predictions_oob
votes_oob$classlabel <- yearly_training$classlabels
votes_oob <- votes_oob %>% mutate(id=seq_len(n())) %>%
  melt(id.var=c('classlabel','id','predicted'), na.rm=T) %>%
  select(-id)
#arrange labels 
votes_oob$classlabel <- factor(votes_oob$classlabel, 
                               levels=c("nn", 
                                        "theta",
                                        "wn",
                                        "ARMA/AR/MA",
                                        "ARIMA",
                                        "ETS-notrendnoseasonal",
                                        "ETS-dampedtrend",
                                        "ETS-trend", 
                                        "rw",
                                        "rwd")
)

oob_boxplot_yearly <- ggplot(votes_oob, aes(x = variable, 
                                            y = value, fill = classlabel)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.4) +
  scale_fill_manual(values = c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090",
                               "#e0f3f8", "#abd9e9", "#74add1", "#4575b4",
                               "#313695")) +
  ylab("Classification error based on OOB error") +
  xlab("") + 
  theme(legend.position = "right", legend.title = element_blank(), legend.text.align = 0) + 
  guides(fill=guide_legend(reverse=TRUE)) +
  scale_x_discrete(limits=c("nn", 
                            "theta", 
                            "wn", 
                            "ARMA/AR/MA",
                            "ARIMA", 
                            "ETS-notrendnoseasonal", 
                            "ETS-dampedtrend",
                            "ETS-trend",
                            "rw", 
                            "rwd")) +
  coord_flip() 
oob_boxplot_yearly

## ---- vi_yearly
# All variable scores into one dataframe
load("data/yearly/train_importance.rda")
load(file="data/yearly/sd_pdf_df.rda")
load(file="data/yearly/sd_ice_df.rda")
## Permutation based
#head(train_importance)
#class(train_importance) #matrix
train_imp_df <- data.frame(train_importance)
train_imp_df <- add_rownames(train_imp_df, "Feature")
#names(train_imp_df)
train_imp_df <- within(train_imp_df, rm("MeanDecreaseAccuracy", "MeanDecreaseGini"))
permutation_imp <- train_imp_df %>% melt(id.vars="Feature")
#head(permutation_imp)
#dim(permutation_imp) # 250 3
colnames(permutation_imp) <- c("feature", "class", "score")

## PDP-based
#head(sd_pdf_df)
sd_pdf_df <- add_rownames(sd_pdf_df, "class")
#head(sd_pdf_df) %>% data.frame()
pdp_imp <- sd_pdf_df %>% melt(id.vars="class")
#head(pdp_imp)
colnames(pdp_imp) <- c("class", "feature", "score")
#dim(pdp_imp) # 250 3

## ICE-based
#head(sd_ice_df)
sd_ice_df <- add_rownames(sd_ice_df, "class")
#head(sd_ice_df) %>% data.frame()
ice_imp <- sd_ice_df %>% melt(id.vars="class")
#head(ice_imp)
colnames(ice_imp) <- c("class", "feature", "score")
#dim(ice_imp) # 250 3

## Combine the data frames
importancescoreY <- bind_rows(permutation_imp, pdp_imp)
importancescoreY <- bind_rows(importancescoreY, ice_imp)
importancescoreY$VI <- rep(c("permutation", "PDP", "ICE"), each=250)

## rank permutation, sd_pdp, and sd_ice scores for each class
importancescoreY$class <- factor(importancescoreY$class, levels=c("rwd", "rw", "ETS.trend", "ETS.dampedtrend", "ETS.notrendnoseasonal", "ARIMA", "ARMA.AR.MA", "wn", "theta","nn"),
                                 labels = c("rwd", "rw", "ETS.trend", "ETS.dampedtrend", "ETS.notrendnoseasonal", "ARIMA", "ARMA.AR.MA", "wn", "theta","nn"))
rank_vi_yearly_classes <- importancescoreY %>% 
  group_by(VI, class) %>% 
  mutate(rank=min_rank(score))
## compute mean rank
meanrank_viy_classes <- rank_vi_yearly_classes %>% group_by(feature,class) %>% summarise_at(vars(c(rank)), funs(mean))

## overal importance of features to the forest
train_impforest <- data.frame(train_importance)
train_impforest <- add_rownames(train_impforest, "Feature")
train_impforest <- train_impforest[, c("Feature","MeanDecreaseAccuracy", "MeanDecreaseGini")]
#head(train_impforest)
train_impforest <- train_impforest%>% 
  mutate(rank_permu=min_rank(MeanDecreaseAccuracy)) %>%
  mutate(rank_gini=min_rank(MeanDecreaseGini))
train_impforest$mean_rank <- (train_impforest$rank_permu+train_impforest$rank_gini)/2
meanrank_viy_forest <- data.frame(feature=train_impforest$Feature,
                                       class=rep("overall", 25),
                                       rank=train_impforest$mean_rank)
## combine mean ranks for overall forest and separate classes
meanrank_yearly <- dplyr::bind_rows(meanrank_viy_forest, meanrank_viy_classes)
## create horizontal bar chart for ranks
orderOverall <- filter(meanrank_yearly, class=="overall")
meanrank_yearly$feature <- factor(meanrank_yearly$feature, levels = orderOverall$feature[order(orderOverall$rank)])
meanrank_yearly$class <- factor(meanrank_yearly$class, 
                               levels=c("overall","rwd",
                                 "rw",
                                 "ETS.trend", 
                                 "ETS.dampedtrend",
                                 "ETS.notrendnoseasonal",
                                 "ARIMA",
                                 "ARMA.AR.MA",
                                 "wn",
                                 "theta",
                                 "nn"))
feaImp_yearly <- ggplot(meanrank_yearly, aes(y=rank, x=feature)) + 
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~class,ncol = 6, nrow = 2) + 
  coord_flip()+ylab("Average rank")
feaImp_yearly 

## ---- pdp_yearly

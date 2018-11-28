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

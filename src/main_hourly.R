
################################################################
#              HOurly data                                     #
################################################################

## ---- oob_hourly
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
votes_oobH$classlabel <- factor(votes_oobH$classlabel, 
                                levels=rev(c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                             "theta","nn","wn"))
)
horizontalbar_hourly <- ggplot(votes_oobH, aes(x = variable, y = value, fill = classlabel)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.4) +
  ylab("Proportion") +
  xlab("") + 
  theme(legend.position = "right", legend.title = element_blank(), legend.text.align = 0, text = element_text(size=20)) + 
  guides(fill=guide_legend(reverse=TRUE)) +
  scale_x_discrete(limits=rev(c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                "theta","nn","wn"))) +
  coord_flip() 
horizontalbar_hourly

## ----pdp_hourly



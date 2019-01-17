#################################################################
#                  Daily data and Hourly series                              #
#################################################################
## ---- oobdailyhourly
load("data/daily/trainD_votes.rda") # oob votes from the random forest
load("data/daily/trainD_predictions_oob.rda") # based on oob prediction
load("data/daily/daily_training.rda") # random forest training set
votes_oobD <- data.frame(trainD_votes)
names(votes_oobD) <- names(table(trainD_predictions_oob))
votes_oobD$predicted <- trainD_predictions_oob
votes_oobD$classlabel <- daily_training$classlabels
votes_oobD <- votes_oobD %>%
  mutate(id = seq_len(n())) %>%
  melt(id.var = c("classlabel", "id", "predicted"), na.rm = T) %>%
  select(-id)
# new addition to arrange labels
votes_oobD$classlabel <- factor(votes_oobD$classlabel, levels = rev(c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                                                      "theta","nn","wn")))
oob_boxplot_daily <- ggplot(votes_oobD, aes(x = variable, y = value, fill = classlabel)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.4) +
  ylab("Proportion") +
  xlab("") +
  theme(legend.position = "none", legend.title = element_blank(), legend.text.align = 0, text = element_text(size = 15)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = rev(c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                  "theta","nn","wn"))) +
  coord_flip()+labs(subtitle = "A: Daily")

# hourly data
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
  guides(fill=guide_legend(reverse=TRUE)) +
  scale_x_discrete(limits=rev(c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                "theta","nn","wn"))) +
  coord_flip()+theme(legend.position = "right", legend.title = element_blank(), legend.text.align = 0,text = element_text(size=15),axis.text.y = element_blank())+labs(subtitle = "B: Hourly")
oob_boxplot_daily|horizontalbar_hourly

## ---- friedmandailyhourly
load("data/friedmanHstat_daily.rda")
col.order <- c("seasonal_strength1", "stability", "trend", "lumpiness",
               "linearity", "nonlinearity", "y_pacf5", "curvature", "e_acf1",
               "spikiness", "sediff_seacf1","seas_pacf", "N", "sediff_acf5",
               "entropy", "y_acf5", "y_acf1", "diff1y_acf5", "sediff_acf1",
               "diff1y_pacf5", "diff1y_acf1", "hurst", "seasonal_strength2",
               "diff2y_acf1", "diff2y_acf5", "diff2y_pacf5")
#friedmanHstat_daily$interaction <- ifelse(friedmanHstat_daily$interaction < 0.5, 0, friedmanHstat_daily$interaction)
## snaive
snaive_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="snaive",]
snaive_DFH_cormat <- friedmanHstat_matrix(snaive_DFH, 26, rev(col.order))
fd1 <- ggcorrplot(snaive_DFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("snaive-daily")

## rw
rw_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="rw",]
rw_DFH_cormat <- friedmanHstat_matrix(rw_DFH, 26, rev(col.order))
fd2 <- ggcorrplot(rw_DFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("rw-daily")
## rwd
rwd_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="rwd",]
rwd_DFH_cormat <- friedmanHstat_matrix(rwd_DFH, 26, rev(col.order))
fd3 <- ggcorrplot(rwd_DFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("rwd-daily")

#mstlarima
mstlarima_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="mstlarima",]
mstlarima_DFH_cormat <- friedmanHstat_matrix(mstlarima_DFH, 26, rev(col.order))
fd4 <- ggcorrplot(mstlarima_DFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("mstlarima-daily")

#mstlets
mstlets_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="mstlets",]
mstlets_DFH_cormat <- friedmanHstat_matrix(mstlets_DFH, 26, rev(col.order))
fd5 <- ggcorrplot(mstlets_DFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("mstlets-daily")
# tbats
tbats_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="tbats",]
tbats_DFH_cormat <- friedmanHstat_matrix(tbats_DFH, 26, rev(col.order))
fd6 <- ggcorrplot(tbats_DFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("tbats-daily")

#stlar
stlar_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="stlar",]
stlar_DFH_cormat <- friedmanHstat_matrix(stlar_DFH, 26, rev(col.order))
fd7 <- ggcorrplot(stlar_DFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("stlar-daily")


#theta
theta_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="theta",]
theta_DFH_cormat <- friedmanHstat_matrix(theta_DFH, 26, rev(col.order))
fd8 <- ggcorrplot(theta_DFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("theta-daily")


#nn
nn_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="nn",]
nn_DFH_cormat <- friedmanHstat_matrix(nn_DFH, 26, rev(col.order))
fd9 <- ggcorrplot(nn_DFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("nn-daily")

#wn
wn_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="wn",]
wn_DFH_cormat <- friedmanHstat_matrix(wn_DFH, 26, rev(col.order))
fd10 <- ggcorrplot(wn_DFH_cormat, hc.order = FALSE, type = "upper",
                  outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  ggtitle("wn-daily")

# hourly data
load("data/friedmanHstat_hourly.rda")
col.order <- c("seasonal_strength1", "stability", "trend", "lumpiness",
               "linearity", "nonlinearity", "y_pacf5", "curvature", "e_acf1",
               "spikiness", "sediff_seacf1","seas_pacf", "N", "sediff_acf5",
               "entropy", "y_acf5", "y_acf1", "diff1y_acf5", "sediff_acf1",
               "diff1y_pacf5", "diff1y_acf1", "hurst", "seasonal_strength2",
               "diff2y_acf1", "diff2y_acf5", "diff2y_pacf5")
## snaive
snaive_HFH <- friedmanHstat_hourly[friedmanHstat_hourly$class=="snaive",]
snaive_HFH_cormat <- friedmanHstat_matrix(snaive_HFH, 26, rev(col.order))
fh1 <- ggcorrplot(snaive_HFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("snaive-hourly")

## rw
rw_HFH <- friedmanHstat_hourly[friedmanHstat_hourly$class=="rw",]
rw_HFH_cormat <- friedmanHstat_matrix(rw_HFH, 26, rev(col.order))
fh2 <- ggcorrplot(rw_HFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("rw-hourly")

## rwd
rwd_HFH <- friedmanHstat_hourly[friedmanHstat_hourly$class=="rwd",]
rwd_HFH_cormat <- friedmanHstat_matrix(rwd_HFH, 26, rev(col.order))
fh3 <- ggcorrplot(rwd_HFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("rwd-hourly")

#mstlarima
mstlarima_HFH <- friedmanHstat_hourly[friedmanHstat_hourly$class=="mstlarima",]
mstlarima_HFH_cormat <- friedmanHstat_matrix(mstlarima_HFH, 26, rev(col.order))
fh4 <- ggcorrplot(mstlarima_HFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("mstlarima-hourly")

#mstlets
mstlets_HFH <- friedmanHstat_hourly[friedmanHstat_hourly$class=="mstlets",]
mstlets_HFH_cormat <- friedmanHstat_matrix(mstlets_HFH, 26, rev(col.order))
fh5 <- ggcorrplot(mstlets_HFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("mstlets-hourly")

# tbats
tbats_HFH <- friedmanHstat_hourly[friedmanHstat_hourly$class=="tbats",]
tbats_HFH_cormat <- friedmanHstat_matrix(tbats_HFH, 26, rev(col.order))
fh6 <- ggcorrplot(tbats_HFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("tbats-hourly")

#stlar
stlar_HFH <- friedmanHstat_hourly[friedmanHstat_hourly$class=="stlar",]
stlar_HFH_cormat <- friedmanHstat_matrix(stlar_HFH, 26, rev(col.order))
fh7 <- ggcorrplot(stlar_HFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("stlar-hourly")


#theta
theta_HFH <- friedmanHstat_hourly[friedmanHstat_hourly$class=="theta",]
theta_HFH_cormat <- friedmanHstat_matrix(theta_HFH, 26, rev(col.order))
fh8 <- ggcorrplot(theta_HFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("theta-hourly")

#nn
nn_HFH <- friedmanHstat_hourly[friedmanHstat_hourly$class=="nn",]
nn_HFH_cormat <- friedmanHstat_matrix(nn_HFH, 26, rev(col.order))
fh9 <- ggcorrplot(theta_HFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("nn-hourly")

#wn
wn_HFH <- friedmanHstat_hourly[friedmanHstat_hourly$class=="wn",]
wn_HFH_cormat <- friedmanHstat_matrix(wn_HFH, 26, rev(col.order))
fh10 <- ggcorrplot(wn_HFH_cormat, hc.order = FALSE, type = "upper",
                  outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  ggtitle("wn-hourly")

fd1+fd2+fh1+fh2+fd3+fd4+fh3+fh4+fd5+fd6+fh5+fh6+fd7+fd8+fh7+fh8+fd9+fd10+fh9+fh10+plot_layout(ncol = 4, nrow = 5)


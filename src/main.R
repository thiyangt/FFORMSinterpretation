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
library(forestviews)
# install_github(repo = 'thiyangt/forestviews')
library(networkD3)
library(RColorBrewer)
library(iml) #machine learning interpretability package
library(ggcorrplot) # to draw  ggcorrplot
source("src/reordercormat.R")

#################################################################
#                  Yearly data                               #
#################################################################

## ---- yearly_oob
load("data/yearly/train_votes.rda") # oob votes from the random forest
load("data/yearly/train_predictions_oob.rda") # based on oob prediction
load("data/yearly/yearly_training.rda") # random forest training set
votes_oob <- data.frame(train_votes)
names(votes_oob) <- names(table(train_predictions_oob))
votes_oob$predicted <- train_predictions_oob
votes_oob$classlabel <- yearly_training$classlabels
votes_oob <- votes_oob %>%
  mutate(id = seq_len(n())) %>%
  melt(id.var = c("classlabel", "id", "predicted"), na.rm = T) %>%
  select(-id)
# arrange labels
votes_oob$classlabel <- factor(votes_oob$classlabel,
  levels = c(
    "nn",
    "theta",
    "wn",
    "ARMA/AR/MA",
    "ARIMA",
    "ETS-notrendnoseasonal",
    "ETS-dampedtrend",
    "ETS-trend",
    "rw",
    "rwd"
  )
)

oob_boxplot_yearly <- ggplot(votes_oob, aes(
  x = variable,
  y = value, fill = classlabel
)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.4) +
  scale_fill_manual(values = c(
    "#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090",
    "#e0f3f8", "#abd9e9", "#74add1", "#4575b4",
    "#313695"
  )) +
  ylab("Proportion") +
  xlab("") +
  theme(legend.position = "right", legend.title = element_blank(), legend.text.align = 0) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = c(
    "nn",
    "theta",
    "wn",
    "ARMA/AR/MA",
    "ARIMA",
    "ETS-notrendnoseasonal",
    "ETS-dampedtrend",
    "ETS-trend",
    "rw",
    "rwd"
  )) +
  coord_flip()
oob_boxplot_yearly

## ---- vi_yearly
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
  levels = c("rwd", "rw", "ETS.trend", "ETS.dampedtrend", "ETS.notrendnoseasonal", "ARIMA", "ARMA.AR.MA", "wn", "theta", "nn"),
  labels = c("rwd", "rw", "ETS.trend", "ETS.dampedtrend", "ETS.notrendnoseasonal", "ARIMA", "ARMA.AR.MA", "wn", "theta", "nn")
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
    "overall", "rwd",
    "rw",
    "ETS.trend",
    "ETS.dampedtrend",
    "ETS.notrendnoseasonal",
    "ARIMA",
    "ARMA.AR.MA",
    "wn",
    "theta",
    "nn"
  )
)
feaImp_yearly <- ggplot(meanrank_yearly, aes(y = rank, x = feature)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~class, ncol = 6, nrow = 2) +
  coord_flip() + ylab("Average rank")
feaImp_yearly

## ---- pdp_yearly
load("data/yearly/pdp_yearly/trendgrid.rda")
trendgrid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/y_acf1grid.rda")
y_acf1grid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/y_acf5grid.rda")
y_acf5grid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/entropygrid.rda")
entropygrid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/hurstgrid.rda")
hurstgrid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/ur_ppgrid.rda")
ur_ppgrid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/alphagrid.rda")
alphagrid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/betagrid.rda")
betagrid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/spikinessgrid.rda")
spikinessgrid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/linearitygrid.rda")
linearitygrid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/curvaturegrid.rda")
curvaturegrid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/e_acf1grid.rda")
e_acf1grid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/diff1y_acf1grid.rda")
diff1y_acf1grid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/diff1y_acf5grid.rda")
diff1y_acf5grid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/diff1y_pacf5grid.rda")
diff1y_pacf5grid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/stabilitygrid.rda")
stabilitygrid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/nonlinearitygrid.rda")
nonlinearitygrid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/Ngrid.rda")
Ngrid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/lmres_acf1grid.rda")
lmres_acf1grid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/y_pacf5grid.rda")
y_pacf5grid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/lumpinessgrid.rda")
lumpinessgrid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/diff2y_pacf5grid.rda")
diff2y_pacf5grid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/diff2y_pacf5grid.rda")
diff2y_pacf5grid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/diff2y_acf5grid.rda")
diff2y_acf5grid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/diff2y_acf1grid.rda")
diff2y_acf1grid$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/ur_kpssgrid.rda")
ur_kpssgrid$variable <- rep(1:1000, 20)
# removing outliers
load("data/yearly/pdp_yearly/ur_ppgrid_rmout.rda")
ur_ppgrid_rmout$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/linearitygrid_rmout.rda")
linearitygrid_rmout$variable <- rep(1:1000, 20)
load("data/yearly/pdp_yearly/curvaturegrid_rmout.rda")
curvaturegrid_rmout$variable <- rep(1:1000, 20)

## pdp plots with confidence intervals
## rwd
p1 <- ggplot(data = diff1y_acf1grid, aes_string(x = diff1y_acf1grid$diff1y_acf1, y = "rwd")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("diff1y_acf1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("random walk-drift")
p2 <- ggplot(data = linearitygrid_rmout, aes_string(x = linearitygrid_rmout$linearity, y = "rwd")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
p3 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "rwd")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")

# rw
p4 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "rw")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + xlab("ur_pp") + ylab("random walk")
p5 <- ggplot(data = linearitygrid_rmout, aes_string(x = linearitygrid_rmout$linearity, y = "rw")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
p6 <- ggplot(data = lmres_acf1grid, aes_string(x = lmres_acf1grid$lmres_acf1, y = "rw")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("lmres_acf1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")

# ETS-trend
p7 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "ETS.trend")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + xlab("ur_pp") + ylab("ETS-trend")
p8 <- ggplot(data = trendgrid, aes_string(x = trendgrid$trend, y = "ETS.trend")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
p9 <- ggplot(data = curvaturegrid_rmout, aes_string(x = curvaturegrid_rmout$curvature, y = "ETS.trend")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("curvature") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")

# ETS-dampedtrend
p10 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "ETS.dampedtrend")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + xlab("ur_pp") + ylab("ETS-dampedtrend")
p11 <- ggplot(data = diff1y_acf1grid, aes_string(x = diff1y_acf1grid$diff1y_acf1, y = "ETS.dampedtrend")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("diff1y_acf1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
p12 <- ggplot(data = betagrid, aes_string(x = betagrid$beta, y = "ETS.dampedtrend")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("beta") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")

# ETS-notrendnoseasonal
p13 <- ggplot(data = trendgrid, aes_string(x = trendgrid$trend, y = "ETS.notrendnoseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("ETS-notrendnoseasonal")
p14 <- ggplot(data = linearitygrid_rmout, aes_string(x = linearitygrid_rmout$linearity, y = "ETS.notrendnoseasonal")) +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") + theme(legend.position = "none") + ylab("")
p15 <- ggplot(data = lmres_acf1grid, aes_string(x = lmres_acf1grid$lmres_acf1, y = "ETS.notrendnoseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("lmres_acf1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")

# ARIMA
p16 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "ARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("ARIMA")
p17 <- ggplot(data = diff1y_acf1grid, aes_string(x = diff1y_acf1grid$diff1y_acf1, y = "ARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("diff1y_acf1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
p18 <- ggplot(data = trendgrid, aes_string(x = trendgrid$trend, y = "ARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")

# ARMA.AR.MA
p19 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("ARMA.AR.MA")
p20 <- ggplot(data = linearitygrid_rmout, aes_string(x = linearitygrid_rmout$linearity, y = "ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
p21 <- ggplot(data = trendgrid, aes_string(x = trendgrid$trend, y = "ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")

# wn
p22 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "wn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("wn")
p23 <- ggplot(data = trendgrid, aes_string(x = trendgrid$trend, y = "wn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
p24 <- ggplot(data = linearitygrid, aes_string(x = linearitygrid$linearity, y = "wn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")

# theta
p25 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "theta")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("theta")
p26 <- ggplot(data = lmres_acf1grid, aes_string(x = lmres_acf1grid$lmres_acf1, y = "theta")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
p27 <- ggplot(data = trendgrid, aes_string(x = trendgrid$trend, y = "theta")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")

## nn
p28 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "nn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("nn")
p29 <- ggplot(data = trendgrid, aes_string(x = trendgrid$trend, y = "nn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
p30 <- ggplot(data = linearitygrid_rmout, aes_string(x = linearitygrid_rmout$linearity, y = "nn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")

(p3 | p2 | p1) / (p4 | p5 | p6) / (p7 | p8 | p9) / (p10 | p11 | p12) / (p13 | p14 | p15) / (p16 | p17 | p18) / (p19 | p20 | p21) / (p22 | p23 | p24) / (p25 | p26 | p27) / (p28 | p29 | p30)

## ---- sankey_yearly
load("data/yearly/rf.yearly.all.paths.rda")
nd3y <- rf_sankey(all.paths.out = rf.yearly.all.paths, all.nodes = FALSE, plot.node.lim = 6)
sankeyNetwork(
  Links = nd3y$links, Nodes = nd3y$nodes,
  Source = "source", Target = "target", Value = "value",
  NodeID = "name", units = "Count", fontSize = 12,
  nodeWidth = 30, NodeGroup = NULL
)

## ---- friedmany
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
load("data/friedmanHstat_yearly.rda")
rwd_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="rwd",]
#dim(rwd_YFH)
#head(rwd_YFH)
rwd_YFH_cor <- rwd_YFH %>% select(c("feature1", "feature2", "interaction"))
names(rwd_YFH_cor) <- c("Var1", "Var2", "value")
df1 <- data.frame(Var1=names(table(rwd_YFH_cor$Var1)),
                  Var2=names(table(rwd_YFH_cor$Var1)),
                  value=rep(1.00, 25))

cormat <- dplyr::bind_rows(rwd_YFH_cor, df1)
cormat <- dcast(cormat, Var1 ~ Var2, value.var="value")
colnames(cormat)[1] <- ""
#class(cormat)
cormat <- data.matrix(cormat)
#dim(cormat)
#colnames(cormat) 
cormat <- cormat[,-1]
rownames(cormat) <- colnames(cormat)
cormat <- round(cormat,2)
cormat1 <- reorder_cormat(cormat)
p1 <- ggcorrplot(cormat1, hc.order = TRUE, type = "upper",
           outline.col = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.5, limit = c(0,1), space = "Lab", 
                       name="")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+guides(fill=FALSE, color=FALSE)+ggtitle("rwd")
## random walk
rw_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="rw",]
#dim(rwd_YFH)
#head(rwd_YFH)
rw_YFH_cor <- rw_YFH %>% select(c("feature1", "feature2", "interaction"))
names(rw_YFH_cor) <- c("Var1", "Var2", "value")
df1 <- data.frame(Var1=names(table(rw_YFH_cor$Var1)),
                  Var2=names(table(rw_YFH_cor$Var1)),
                  value=rep(1.00, 25))

cormatrw <- dplyr::bind_rows(rw_YFH_cor, df1)
cormatrw <- dcast(cormatrw, Var1 ~ Var2, value.var="value")
colnames(cormatrw)[1] <- ""
#class(cormat)
cormatrw <- data.matrix(cormatrw)
#dim(cormat)
#colnames(cormat) 
cormatrw <- cormatrw[,-1]
rownames(cormatrw) <- colnames(cormatrw)
cormatrw <- round(cormatrw,2)
cormatrw1 <- reorder_cormat(cormatrw)
p2 <- ggcorrplot(cormatrw1, hc.order = TRUE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.5, limit = c(0,1), space = "Lab", 
                       name="")+theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("rw")

## ETS-trend
etst_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="ETS.trend",]
etst_YFH_cor <- etst_YFH %>% select(c("feature1", "feature2", "interaction"))
names(etst_YFH_cor) <- c("Var1", "Var2", "value")
df1 <- data.frame(Var1=names(table(etst_YFH_cor$Var1)),
                  Var2=names(table(etst_YFH_cor$Var1)),
                  value=rep(1.00, 25))

cormatetst <- dplyr::bind_rows(etst_YFH_cor, df1)
cormatetst <- dcast(cormatetst, Var1 ~ Var2, value.var="value")
colnames(cormatetst)[1] <- ""
cormatetst <- data.matrix(cormatetst)
cormatetst <- cormatetst[,-1]
rownames(cormatetst) <- colnames(cormatetst)
cormatetst <- round(cormatetst,2)
cormatetst1 <- reorder_cormat(cormatetst)
p3 <- ggcorrplot(cormatetst1, hc.order = TRUE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.5, limit = c(0,1), space = "Lab", 
                       name="")+theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                                                 size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ETS-trend")

## ETS-damped trend
etsdt_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="ETS.dampedtrend",]
etsdt_YFH_cor <- etsdt_YFH %>% select(c("feature1", "feature2", "interaction"))
names(etsdt_YFH_cor) <- c("Var1", "Var2", "value")
df1 <- data.frame(Var1=names(table(etsdt_YFH_cor$Var1)),
                  Var2=names(table(etsdt_YFH_cor$Var1)),
                  value=rep(1.00, 25))

cormatetsdt <- dplyr::bind_rows(etsdt_YFH_cor, df1)
cormatetsdt <- dcast(cormatetsdt, Var1 ~ Var2, value.var="value")
colnames(cormatetsdt)[1] <- ""
cormatetsdt <- data.matrix(cormatetsdt)
cormatetsdt <- cormatetsdt[,-1]
rownames(cormatetsdt) <- colnames(cormatetsdt)
cormatetsdt <- round(cormatetsdt,2)
cormatetsdt1 <- reorder_cormat(cormatetsdt)
p4 <- ggcorrplot(cormatetsdt1, hc.order = TRUE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.5, limit = c(0,1), space = "Lab", 
                       name="")+theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                                                 size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ETS-dampedtrend")


## ETS-notrendnoseasonal
etsntns_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="ETS.notrendnoseasonal",]
etsntns_YFH_cor <- etsntns_YFH %>% select(c("feature1", "feature2", "interaction"))
names(etsntns_YFH_cor) <- c("Var1", "Var2", "value")
df1 <- data.frame(Var1=names(table(etsntns_YFH_cor$Var1)),
                  Var2=names(table(etsntns_YFH_cor$Var1)),
                  value=rep(1.00, 25))

cormatetsntns <- dplyr::bind_rows(etsntns_YFH_cor, df1)
cormatetsntns <- dcast(cormatetsntns, Var1 ~ Var2, value.var="value")
colnames(cormatetsntns)[1] <- ""
cormatetsntns <- data.matrix(cormatetsntns)
cormatetsntns <- cormatetsntns[,-1]
rownames(cormatetsntns) <- colnames(cormatetsntns)
cormatetsntns <- round(cormatetsntns,2)
cormatetsntns1 <- reorder_cormat(cormatetsntns)
p5 <- ggcorrplot(cormatetsntns1, hc.order = TRUE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.5, limit = c(0,1), space = "Lab", 
                       name="")+theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                                                 size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ETS-notrendnoseasonal")

## ARIMA
arima_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="ARIMA",]
arima_YFH_cor <- arima_YFH %>% select(c("feature1", "feature2", "interaction"))
names(arima_YFH_cor) <- c("Var1", "Var2", "value")
df1 <- data.frame(Var1=names(table(arima_YFH_cor$Var1)),
                  Var2=names(table(arima_YFH_cor$Var1)),
                  value=rep(1.00, 25))

cormatarima <- dplyr::bind_rows(arima_YFH_cor, df1)
cormatarima <- dcast(cormatarima, Var1 ~ Var2, value.var="value")
colnames(cormatarima)[1] <- ""
cormatarima <- data.matrix(cormatarima)
cormatarima <- cormatarima[,-1]
rownames(cormatarima) <- colnames(cormatarima)
cormatarima <- round(cormatarima,2)
cormatarima1 <- reorder_cormat(cormatarima)
p6 <- ggcorrplot(cormatarima1, hc.order = TRUE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.5, limit = c(0,1), space = "Lab", 
                       name="")+theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                                                 size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ARIMA")

##  ARMA.AR.MA 
arma_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="ARMA.AR.MA",]
arma_YFH_cor <- arma_YFH %>% select(c("feature1", "feature2", "interaction"))
names(arma_YFH_cor) <- c("Var1", "Var2", "value")
df1 <- data.frame(Var1=names(table(arma_YFH_cor$Var1)),
                  Var2=names(table(arma_YFH_cor$Var1)),
                  value=rep(1.00, 25))

cormatarma <- dplyr::bind_rows(arma_YFH_cor, df1)
cormatarma <- dcast(cormatarma, Var1 ~ Var2, value.var="value")
colnames(cormatarma)[1] <- ""
cormatarma <- data.matrix(cormatarma)
cormatarma <- cormatarma[,-1]
rownames(cormatarma) <- colnames(cormatarma)
cormatarma <- round(cormatarma,2)
cormatarma1 <- reorder_cormat(cormatarma)
p7 <- ggcorrplot(cormatarma1, hc.order = TRUE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.5, limit = c(0,1), space = "Lab", 
                       name="")+theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                                                 size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ARMA/AR/MA")


##  wn 
wn_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="wn",]
wn_YFH_cor <- arma_YFH %>% select(c("feature1", "feature2", "interaction"))
names(wn_YFH_cor) <- c("Var1", "Var2", "value")
df1 <- data.frame(Var1=names(table(wn_YFH_cor$Var1)),
                  Var2=names(table(wn_YFH_cor$Var1)),
                  value=rep(1.00, 25))

cormatwn <- dplyr::bind_rows(wn_YFH_cor, df1)
cormatwn <- dcast(cormatwn, Var1 ~ Var2, value.var="value")
colnames(cormatwn)[1] <- ""
cormatwn <- data.matrix(cormatwn)
cormatwn <- cormatwn[,-1]
rownames(cormatwn) <- colnames(cormatwn)
cormatwn <- round(cormatwn,2)
cormatwn1 <- reorder_cormat(cormatwn)
p8 <- ggcorrplot(cormatwn1, hc.order = TRUE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.5, limit = c(0,1), space = "Lab", 
                       name="")+theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                                                 size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("wn")

## theta
theta_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="theta",]
theta_YFH_cor <- theta_YFH %>% select(c("feature1", "feature2", "interaction"))
names(theta_YFH_cor) <- c("Var1", "Var2", "value")
df1 <- data.frame(Var1=names(table(theta_YFH_cor$Var1)),
                  Var2=names(table(theta_YFH_cor$Var1)),
                  value=rep(1.00, 25))

cormattheta <- dplyr::bind_rows(theta_YFH_cor, df1)
cormattheta <- dcast(cormattheta, Var1 ~ Var2, value.var="value")
colnames(cormattheta)[1] <- ""
cormattheta <- data.matrix(cormattheta)
cormattheta <- cormattheta[,-1]
rownames(cormattheta) <- colnames(cormattheta)
cormattheta <- round(cormattheta,2)
cormattheta1 <- reorder_cormat(cormattheta)
p9 <- ggcorrplot(cormattheta, hc.order = TRUE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.5, limit = c(0,1), space = "Lab", 
                       name="")+theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                                                 size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("theta")

## nn
nn_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="nn",]
nn_YFH_cor <- nn_YFH %>% select(c("feature1", "feature2", "interaction"))
names(nn_YFH_cor) <- c("Var1", "Var2", "value")
df1 <- data.frame(Var1=names(table(nn_YFH_cor$Var1)),
                  Var2=names(table(nn_YFH_cor$Var1)),
                  value=rep(1.00, 25))

cormatnn <- dplyr::bind_rows(nn_YFH_cor, df1)
cormatnn <- dcast(cormatnn, Var1 ~ Var2, value.var="value")
colnames(cormatnn)[1] <- ""
cormatnn <- data.matrix(cormatnn)
cormatnn <- cormatnn[,-1]
rownames(cormatnn) <- colnames(cormatnn)
cormatnn <- round(cormatnn,2)
cormatnn1 <- reorder_cormat(cormatnn)
p10 <- ggcorrplot(cormatnn1, hc.order = TRUE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.5, limit = c(0,1), space = "Lab", 
                       name="Friedman's H-statistic")+theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                                                 size = 12, hjust = 1))+
  ggtitle("nn")


p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+plot_layout(ncol = 3, nrow = 4)

## ---- two_way_interaction_yearly1
load("data/yearly/trend.urpprmout.y.rda")
load("data/yearly/spikiness.diff1y_acf5.y.rda")
load("data/yearly/hurst.trend.y.rda")
load("data/yearly/trend.linearityrmout.y.rda")
colNamestrur <- colnames(trend.urpprmout.y)[27:36]
colNamessd <- colnames(spikiness.diff1y_acf5.y)[27:36]
colNamesht <- colnames(spikiness.diff1y_acf5.y)[27:36]
colNamestl <- colnames(trend.linearityrmout.y)[27:36]


# rwd
int1 <- ggplot(
  data = trend.urpprmout.y, aes_string(x = trend.urpprmout.y$trend,y = trend.urpprmout.y$ur_pp, z = colNamestrur[8], fill = colNamestrur[8]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.4), breaks = seq(0, 0.4, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("ur_pp") + theme(legend.position="none")+ggtitle("rwd (H=0.32)")
  
int2 <- ggplot(
  data = spikiness.diff1y_acf5.y,
  aes_string(
    x = spikiness.diff1y_acf5.y$spikiness,
    y = spikiness.diff1y_acf5.y$diff1y_acf5, z = colNamessd[8], fill = colNamessd[8]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("spikiness") + ylab("diff1y_acf5")+ theme(legend.position="none")+ggtitle("rwd (H=0.88)")
int3 <- ggplot(
  data = hurst.trend.y,
  aes_string(
    x = hurst.trend.y$hurst,
    y = hurst.trend.y$trend, z = colNamessd[8], fill = colNamessd[8]
  )) + geom_tile() +
  scale_fill_continuous(limits = c(0, 0.4), breaks = seq(0, 0.4, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("hurst") + ylab("trend")+ theme(legend.position="none")+ggtitle("rwd (H=0.14)")
int4 <- ggplot(
  data = trend.linearityrmout.y,
  aes_string(
    x = trend.linearityrmout.y$trend,
    y = trend.linearityrmout.y$linearity, z = colNamestl[8], fill = colNamestl[8]
  )) + geom_tile() +
  scale_fill_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("linearity")+ theme(legend.position="none")+ggtitle("rwd (H=1.00)")
## rw
int5 <- ggplot(
  data = trend.urpprmout.y,
  aes_string(
    x = trend.urpprmout.y$trend,
    y = trend.urpprmout.y$ur_pp, z = colNamestrur[7], fill = colNamestrur[7]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.15), breaks = seq(0, 0.15, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("ur_pp")+ theme(legend.position="none")+ggtitle("rw (H=0.40)")
int6 <- ggplot(
  data = spikiness.diff1y_acf5.y,
  aes_string(
    x = spikiness.diff1y_acf5.y$spikiness,
    y = spikiness.diff1y_acf5.y$diff1y_acf5, z = colNamessd[7], fill = colNamessd[7]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("spikiness") + ylab("diff1y_acf5")+ theme(legend.position="none")+ggtitle("rw (H=0.80)")
int7 <- ggplot(
  data = hurst.trend.y,
  aes_string(
    x = hurst.trend.y$hurst,
    y = hurst.trend.y$trend, z = colNamessd[7], fill = colNamessd[7]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.15), breaks = seq(0, 0.15, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("hurst") + ylab("trend")+ theme(legend.position="none")+ggtitle("rw (H=0.63)")
int8 <- ggplot(
  data = trend.linearityrmout.y,
  aes_string(
    x = trend.linearityrmout.y$trend,
    y = trend.linearityrmout.y$linearity, z = colNamestl[7], fill = colNamestl[7]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("linearity")+ theme(legend.position="none")+ggtitle("rw (H=0.63)")
# ETS-trend
int9 <- ggplot(
  data = trend.urpprmout.y,
  aes_string(
    x = trend.urpprmout.y$trend,
    y = trend.urpprmout.y$ur_pp, z = colNamestrur[5], fill = colNamestrur[5]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("ur_pp")+ theme(legend.position="none")+ggtitle("ETS-T (H=0.24)")
int10 <- ggplot(
  data = spikiness.diff1y_acf5.y,
  aes_string(
    x = spikiness.diff1y_acf5.y$spikiness,
    y = spikiness.diff1y_acf5.y$diff1y_acf5, z = colNamessd[5], fill = colNamessd[5]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("spikiness") + ylab("diff1y_acf5")+ theme(legend.position="none")+ggtitle("ETS-T (H=1.00)")
int11 <- ggplot(
  data = hurst.trend.y,
  aes_string(
    x = hurst.trend.y$hurst,
    y = hurst.trend.y$trend, z = colNamessd[5], fill = colNamessd[5]
  )) +
  geom_tile() +
  scale_fill_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("hurst") + ylab("trend")+ theme(legend.position="none")+ggtitle("ETS-T (H=0.13)")
int12 <- ggplot(
  data = trend.linearityrmout.y,
  aes_string(
    x = trend.linearityrmout.y$trend,
    y = trend.linearityrmout.y$linearity, z = colNamestl[5], fill = colNamestl[5]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("linearity")+ theme(legend.position="none")+ggtitle("ETS-T (H=0.25)")
## ETS_dampedtrend
int13 <- ggplot(
  data = trend.urpprmout.y,
  aes_string(
    x = trend.urpprmout.y$trend,
    y = trend.urpprmout.y$ur_pp, z = colNamestrur[3], fill = colNamestrur[3]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.05), breaks = seq(0, 0.05, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("ur_pp")+ theme(legend.position="none")+ggtitle("ETS-DT (H=0.56)")
int14 <- ggplot(
  data = spikiness.diff1y_acf5.y,
  aes_string(
    x = spikiness.diff1y_acf5.y$spikiness,
    y = spikiness.diff1y_acf5.y$diff1y_acf5, z = colNamessd[3], fill = colNamessd[3]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.05), breaks = seq(0, 0.05, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("spikiness") + ylab("diff1y_acf5")+ theme(legend.position="none")+ggtitle("ETS-DT (H=0.87)")
int15 <- ggplot(
  data = hurst.trend.y,
  aes_string(
    x = hurst.trend.y$hurst,
    y = hurst.trend.y$trend, z = colNamessd[3], fill = colNamessd[3]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.04), breaks = seq(0, 0.04, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("hurst") + ylab("trend")+ theme(legend.position="none")+ggtitle("ETS-DT (H=0.29)")

int16 <- ggplot(
  data = trend.linearityrmout.y,
  aes_string(
    x = trend.linearityrmout.y$trend,
    y = trend.linearityrmout.y$linearity, z = colNamestl[3], fill = colNamestl[3]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("linearity")+ theme(legend.position="none")+ggtitle("ETS-DT (H=0.31)")
# ETS-notrendseasonal
int17 <- ggplot(
  data = trend.urpprmout.y,
  aes_string(
    x = trend.urpprmout.y$trend,
    y = trend.urpprmout.y$ur_pp, z = colNamestrur[4], fill = colNamestrur[4]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("ur_pp")+ theme(legend.position="none")+ggtitle("ETS-NTNS (H=0.64)")
int18 <- ggplot(
  data = spikiness.diff1y_acf5.y,
  aes_string(
    x = spikiness.diff1y_acf5.y$spikiness,
    y = spikiness.diff1y_acf5.y$diff1y_acf5, z = colNamessd[4], fill = colNamessd[4]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("spikiness") + ylab("diff1y_acf5")+ theme(legend.position="none")+ggtitle("ETS-DT (H=0.71)")
int19 <- ggplot(
  data = hurst.trend.y,
  aes_string(
    x = hurst.trend.y$hurst,
    y = hurst.trend.y$trend, z = colNamessd[4], fill = colNamessd[4]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("hurst") + ylab("trend")+ theme(legend.position="none")+ggtitle("ETS-DT (H=0.29)")
int20 <- ggplot(
  data = trend.linearityrmout.y,
  aes_string(
    x = trend.linearityrmout.y$trend,
    y = trend.linearityrmout.y$linearity, z = colNamestl[4], fill = colNamestl[4]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("linearity")+ theme(legend.position="none")+ggtitle("ETS-DT (H=1.00)")

(int1 | int3 | int4) / (int5 | int7 | int8) / (int9 | int11 | int12) / (int13 | int14 | int15)/(int17|int19|int20)


## ---- two_way_interaction_yearly2
# ARIMA
int1 <- ggplot(
  data = trend.urpprmout.y, aes_string(x = trend.urpprmout.y$trend,y = trend.urpprmout.y$ur_pp, z = colNamestrur[1], fill = colNamestrur[1]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.4), breaks = seq(0, 0.4, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("ur_pp") + theme(legend.position="none")+ggtitle("ARIMA (H=0.50)")

int2 <- ggplot(
  data = spikiness.diff1y_acf5.y,
  aes_string(
    x = spikiness.diff1y_acf5.y$spikiness,
    y = spikiness.diff1y_acf5.y$diff1y_acf5, z = colNamessd[1], fill = colNamessd[1]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("spikiness") + ylab("diff1y_acf5")+ theme(legend.position="none")+ggtitle("ARIMA (H=0.94)")
int3 <- ggplot(
  data = hurst.trend.y,
  aes_string(
    x = hurst.trend.y$hurst,
    y = hurst.trend.y$trend, z = colNamessd[1], fill = colNamessd[1]
  )) + geom_tile() +
  scale_fill_continuous(limits = c(0, 0.4), breaks = seq(0, 0.4, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("hurst") + ylab("trend")+ theme(legend.position="none")+ggtitle("ARIMA (H=0.87)")
int4 <- ggplot(
  data = trend.linearityrmout.y,
  aes_string(
    x = trend.linearityrmout.y$trend,
    y = trend.linearityrmout.y$linearity, z = colNamestl[1], fill = colNamestl[1]
  )) + geom_tile() +
  scale_fill_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("linearity")+ theme(legend.position="none")+ggtitle("ARIMA (H=0.71)")
## ARMA
int5 <- ggplot(
  data = trend.urpprmout.y,
  aes_string(
    x = trend.urpprmout.y$trend,
    y = trend.urpprmout.y$ur_pp, z = colNamestrur[2], fill = colNamestrur[2]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.06), breaks = seq(0, 0.06, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("ur_pp")+ theme(legend.position="none")+ggtitle("ARMA (H=0.77)")
int6 <- ggplot(
  data = spikiness.diff1y_acf5.y,
  aes_string(
    x = spikiness.diff1y_acf5.y$spikiness,
    y = spikiness.diff1y_acf5.y$diff1y_acf5, z = colNamessd[2], fill = colNamessd[2]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.06), breaks = seq(0, 0.06, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("spikiness") + ylab("diff1y_acf5")+ theme(legend.position="none")+ggtitle("ARMA (H=0.77)")
int7 <- ggplot(
  data = hurst.trend.y,
  aes_string(
    x = hurst.trend.y$hurst,
    y = hurst.trend.y$trend, z = colNamessd[2], fill = colNamessd[2]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.05), breaks = seq(0, 0.05, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("hurst") + ylab("trend")+ theme(legend.position="none")+ggtitle("ARMA (H=0.87)")
int8 <- ggplot(
  data = trend.linearityrmout.y,
  aes_string(
    x = trend.linearityrmout.y$trend,
    y = trend.linearityrmout.y$linearity, z = colNamestl[2], fill = colNamestl[2]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("linearity")+ theme(legend.position="none")+ggtitle("ARMA (H=1.00)")
# wn
int9 <- ggplot(
  data = trend.urpprmout.y,
  aes_string(
    x = trend.urpprmout.y$trend,
    y = trend.urpprmout.y$ur_pp, z = colNamestrur[10], fill = colNamestrur[10]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("ur_pp")+ theme(legend.position="none")+ggtitle("wn (H=0.53)")
int10 <- ggplot(
  data = spikiness.diff1y_acf5.y,
  aes_string(
    x = spikiness.diff1y_acf5.y$spikiness,
    y = spikiness.diff1y_acf5.y$diff1y_acf5, z = colNamessd[10], fill = colNamessd[10]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 1), breaks = seq(0, 1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("spikiness") + ylab("diff1y_acf5")+ theme(legend.position="none")+ggtitle("wn (H=0.5)")
int11 <- ggplot(
  data = hurst.trend.y,
  aes_string(
    x = hurst.trend.y$hurst,
    y = hurst.trend.y$trend, z = colNamessd[10], fill = colNamessd[10]
  )) +
  geom_tile() +
  scale_fill_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("hurst") + ylab("trend")+ theme(legend.position="none")+ggtitle("wn (H=0.18)")
int12 <- ggplot(
  data = trend.linearityrmout.y,
  aes_string(
    x = trend.linearityrmout.y$trend,
    y = trend.linearityrmout.y$linearity, z = colNamestl[10], fill = colNamestl[10]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("linearity")+ theme(legend.position="none")+ggtitle("wn(H=0.49)")
## theta
int13 <- ggplot(
  data = trend.urpprmout.y,
  aes_string(
    x = trend.urpprmout.y$trend,
    y = trend.urpprmout.y$ur_pp, z = colNamestrur[9], fill = colNamestrur[9]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("ur_pp")+ theme(legend.position="none")+ggtitle("theta (H=0.53)")
int14 <- ggplot(
  data = spikiness.diff1y_acf5.y,
  aes_string(
    x = spikiness.diff1y_acf5.y$spikiness,
    y = spikiness.diff1y_acf5.y$diff1y_acf5, z = colNamessd[9], fill = colNamessd[9]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("spikiness") + ylab("diff1y_acf5")+ theme(legend.position="none")+ggtitle("theta (H=1.00)")
int15 <- ggplot(
  data = hurst.trend.y,
  aes_string(
    x = hurst.trend.y$hurst,
    y = hurst.trend.y$trend, z = colNamessd[9], fill = colNamessd[9]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("hurst") + ylab("trend")+ theme(legend.position="none")+ggtitle("theta (H=0.38)")

int16 <- ggplot(
  data = trend.linearityrmout.y,
  aes_string(
    x = trend.linearityrmout.y$trend,
    y = trend.linearityrmout.y$linearity, z = colNamestl[9], fill = colNamestl[9]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("linearity")+ theme(legend.position="none")+ggtitle("theta (H=0.58)")
# nn
int17 <- ggplot(
  data = trend.urpprmout.y,
  aes_string(
    x = trend.urpprmout.y$trend,
    y = trend.urpprmout.y$ur_pp, z = colNamestrur[6], fill = colNamestrur[6]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("ur_pp")+ theme(legend.position="none")+ggtitle("nn (H=0.68)")
int18 <- ggplot(
  data = spikiness.diff1y_acf5.y,
  aes_string(
    x = spikiness.diff1y_acf5.y$spikiness,
    y = spikiness.diff1y_acf5.y$diff1y_acf5, z = colNamessd[6], fill = colNamessd[6]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("spikiness") + ylab("diff1y_acf5")+ theme(legend.position="none")+ggtitle("nn (H=1.00)")
int19 <- ggplot(
  data = hurst.trend.y,
  aes_string(
    x = hurst.trend.y$hurst,
    y = hurst.trend.y$trend, z = colNamessd[6], fill = colNamessd[6]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("hurst") + ylab("trend")+ theme(legend.position="none")+ggtitle("nn (H=0.58)")
int20 <- ggplot(
  data = trend.linearityrmout.y,
  aes_string(
    x = trend.linearityrmout.y$trend,
    y = trend.linearityrmout.y$linearity, z = colNamestl[6], fill = colNamestl[6]
  )) +geom_tile() +
  scale_fill_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("linearity")+ theme(legend.position="none")+ggtitle("nn (H=0.58)")

(int1 | int3 | int4) / (int5 | int7 | int8) / (int9 | int10 | int12) / (int13 | int14 | int15)/(int17|int19|int20)



## ---- yearly_pca
load("data/yearly/train_votes.rda")
pcaYvariables <- yearly_training[, 1:25]
pcaM4Y <- prcomp(pcaYvariables, center = TRUE, scale = TRUE)
# summary(pcaM1Y)
PC1m4y <- pcaM4Y$x[, 1]
PC2m4y <- pcaM4Y$x[, 2]
PC3m4y <- pcaM4Y$x[, 3]
m4yPCAresults1 <- data.frame(PC1 = PC1m4y, PC2 = PC2m4y, PC3 = PC3m4y, pcaYvariables)
m4yPCAresults1$predicted <- train_predictions_oob
train_votes1 <- data.frame(train_votes)
m4yPCAresults <- dplyr::bind_cols(m4yPCAresults1, train_votes1)

pca1M4Y_rwd <- ggplot(m4yPCAresults, aes(x = PC1, y = PC2, color = rwd)) +
geom_point() +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
#  geom_point(data = m4yPCAresults[m4yPCAresults$predicted == "rwd", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  scale_colour_gradientn(colours=c("forestgreen","firebrick1"))
  labs(subtitle = "rwd") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Y_rw <- ggplot(m4yPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4yPCAresults[m4yPCAresults$predicted == "rw", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "rw") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Y_etstrend <- ggplot(m4yPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4yPCAresults[m4yPCAresults$predicted == "ETS-trend", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-trend") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Y_etsdamtrend <- ggplot(m4yPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4yPCAresults[m4yPCAresults$predicted == "ETS-dampedtrend", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-dampedtrend") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))


pca1M4Y_notrend <- ggplot(m4yPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4yPCAresults[m4yPCAresults$predicted == "ETS-notrendnoseasonal", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-notrendnoseasonal") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Y_ARIMA <- ggplot(m4yPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4yPCAresults[m4yPCAresults$predicted == "ARIMA", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ARIMA") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Y_ARMA <- ggplot(m4yPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4yPCAresults[m4yPCAresults$predicted == "ARMA/AR/MA", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ARMA/AR/MA") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Y_wn <- ggplot(m4yPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4yPCAresults[m4yPCAresults$predicted == "wn", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "wn") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Y_theta <- ggplot(m4yPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4yPCAresults[m4yPCAresults$predicted == "theta", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "theta") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Y_nn <- ggplot(m4yPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4yPCAresults[m4yPCAresults$predicted == "nn", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "nn") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

(pca1M4Y_rwd | pca1M4Y_rw | pca1M4Y_etstrend | pca1M4Y_etsdamtrend | pca1M4Y_notrend) /
  (pca1M4Y_ARIMA | pca1M4Y_ARMA | pca1M4Y_wn | pca1M4Y_theta | pca1M4Y_nn)


#################################################################
#                  Quarterly data                               #
#################################################################

## ---- oob_quarterly
load("data/quarterly/trainQ_votes.rda") # oob votes from the random forest
load("data/quarterly/trainQ_predictions_oob.rda") # based on oob prediction
load("data/quarterly/quarterly_training.rda") # random forest training set
votes_oobQ <- data.frame(trainQ_votes)
names(votes_oobQ) <- names(table(trainQ_predictions_oob))
votes_oobQ$predicted <- trainQ_predictions_oob
votes_oobQ$classlabel <- quarterly_training$classlabels
votes_oobQ <- votes_oobQ %>%
  mutate(id = seq_len(n())) %>%
  melt(id.var = c("classlabel", "id", "predicted"), na.rm = T) %>%
  select(-id)
# new addition to arrange labels
votes_oobQ$classlabel <- factor(votes_oobQ$classlabel, levels = rev(c(
  "snaive", "rwd", "rw", "ETS-notrendnoseasonal", "ETS-dampedtrend", "ETS-trend", "ETS-dampedtrendseasonal", "ETS-trendseasonal", "ETS-seasonal", "SARIMA",
  "ARIMA", "ARMA/AR/MA", "stlar", "tbats", "wn", "theta", "nn"
)))
oob_boxplot_quarterly <- ggplot(votes_oobQ, aes(x = variable, y = value, fill = classlabel)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.4) +
  ylab("Classification error based on OOB error") +
  xlab("") +
  theme(legend.position = "right", legend.title = element_blank(), legend.text.align = 0, text = element_text(size = 20)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = rev(c(
    "snaive", "rwd", "rw", "ETS-notrendnoseasonal", "ETS-dampedtrend", "ETS-trend", "ETS-dampedtrendseasonal", "ETS-trendseasonal", "ETS-seasonal", "SARIMA",
    "ARIMA", "ARMA/AR/MA", "stlar", "tbats", "wn", "theta", "nn"
  ))) +
  coord_flip()
oob_boxplot_quarterly

## ---- vi_quarterly
# All variable scores into one dataframe
load("data/quarterly/trainQ_importance.rda")
load(file = "data/quarterly/sd_pdf_dfQ.rda")
load(file = "data/quarterly/sd_ice_dfQ.rda")
## Permutation based
train_imp_dfQ <- data.frame(trainQ_importance)
train_imp_dfQ <- add_rownames(train_imp_dfQ, "Feature")
train_imp_dfQ <- within(train_imp_dfQ, rm("MeanDecreaseAccuracy", "MeanDecreaseGini"))
permutation_impQ <- train_imp_dfQ %>% melt(id.vars = "Feature")
# dim(permutation_impQ) # 510 3
colnames(permutation_impQ) <- c("feature", "class", "score")

## PDP-based
sd_pdf_dfQ <- add_rownames(sd_pdf_dfQ, "class")
pdp_imp <- sd_pdf_dfQ %>% melt(id.vars = "class")
colnames(pdp_imp) <- c("class", "feature", "score")

## ICE-based
sd_ice_dfQ <- add_rownames(sd_ice_dfQ, "class")
ice_imp <- sd_ice_dfQ %>% melt(id.vars = "class")
colnames(ice_imp) <- c("class", "feature", "score")

## Combine the data frames
importancescoreQ <- bind_rows(permutation_impQ, pdp_imp)
importancescoreQ <- bind_rows(importancescoreQ, ice_imp)
importancescoreQ$VI <- rep(c("permutation", "PDP", "ICE"), each = 510)

## rank permutation, sd_pdp, and sd_ice scores for each class
importancescoreQ$class <- factor(importancescoreQ$class,
  levels = c(
    "snaive", "rwd", "rw", "ETS.notrendnoseasonal", "ETS.dampedtrend", "ETS.trend", "ETS.dampedtrendseasonal", "ETS.trendseasonal", "ETS.seasonal", "SARIMA",
    "ARIMA", "ARMA.AR.MA", "stlar", "tbats", "wn", "theta", "nn"
  ),
  labels = c(
    "snaive", "rwd", "rw", "ETS.NTNS", "ETS.DT", "ETS.T", "ETS.DTS", "ETS.TS", "ETS.S", "SARIMA",
    "ARIMA", "ARMA.AR.MA", "stlar", "tbats", "wn", "theta", "nn"
  )
)

rank_vi_quarterly_classes <- importancescoreQ %>%
  group_by(VI, class) %>%
  mutate(rank = min_rank(score))

## compute mean rank
meanrank_viq_classes <- rank_vi_quarterly_classes %>% group_by(feature, class) %>% summarise_at(vars(c(rank)), funs(mean))

## overall importance of features to the forest
train_impforestQ <- data.frame(trainQ_importance)
train_impforestQ <- add_rownames(train_impforestQ, "Feature")
train_impforestQ <- train_impforestQ[, c("Feature", "MeanDecreaseAccuracy", "MeanDecreaseGini")]
train_impforestQ <- train_impforestQ %>%
  mutate(rank_permu = min_rank(MeanDecreaseAccuracy)) %>%
  mutate(rank_gini = min_rank(MeanDecreaseGini))
train_impforestQ$mean_rank <- (train_impforestQ$rank_permu + train_impforestQ$rank_gini) / 2
meanrank_viq_forest <- data.frame(
  feature = train_impforestQ$Feature,
  class = rep("overall", 30),
  rank = train_impforestQ$mean_rank
)
## combine mean ranks for overall forest and separate classes
meanrank_quarterly <- dplyr::bind_rows(meanrank_viq_forest, meanrank_viq_classes)
## create horizontal bar chart for ranks
orderOverall <- filter(meanrank_quarterly, class == "overall")
meanrank_quarterly$feature <- factor(meanrank_quarterly$feature, levels = orderOverall$feature[order(orderOverall$rank)])
meanrank_quarterly$class <- factor(meanrank_quarterly$class,
  levels = c(
    "overall", "snaive", "rwd", "rw", "ETS.NTNS", "ETS.DT", "ETS.T", "ETS.DTS", "ETS.TS", "ETS.S", "SARIMA",
    "ARIMA", "ARMA.AR.MA", "stlar", "tbats", "wn", "theta", "nn"
  ),
  labels = c(
    "overall", "snaive", "rwd", "rw", "ETS.NTNS", "ETS.DT", "ETS.T", "ETS.DTS", "ETS.TS", "ETS.S", "SARIMA",
    "ARIMA", "ARMA", "stlar", "tbats", "wn", "theta", "nn"
  )
)
feaImp_quarterly <- ggplot(meanrank_quarterly, aes(y = rank, x = feature)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~class, ncol = 9, nrow = 2) +
  coord_flip() + ylab("Average rank")
feaImp_quarterly

#### ---- pdp_quarterly1
load("data/quarterly/pdp_quarterly/alphagridQ.rda")
alphagridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/betagridQ.rda")
betagridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/curvaturegridQ.rda")
curvaturegridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/diff1y_acf1gridQ.rda")
diff1y_acf1gridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/diff1y_acf5gridQ.rda")
diff1y_acf5gridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/diff1y_pacf5gridQ.rda")
diff1y_pacf5gridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/diff2y_acf1gridQ.rda")
diff2y_acf1gridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/diff2y_acf5gridQ.rda")
diff2y_acf5gridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/diff2y_pacf5gridQ.rda")
diff2y_pacf5gridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/e_acf1gridQ.rda")
e_acf1gridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/entropygridQ.rda")
entropygridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/hurstgridQ.rda")
hurstgridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/hwalphagridQ.rda")
hwalphagridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/hwbetagridQ.rda")
hwbetagridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/hwgammagridQ.rda")
hwgammagridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/linearitygridQ.rda")
linearitygridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/lumpinessgridQ.rda")
lumpinessgridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/NgridQ.rda")
NgridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/nonlinearitygridQ.rda")
nonlinearitygridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/seas_pacfgridQ.rda")
seas_pacfgridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/seasonalitygridQ.rda")
seasonalitygridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/sediff_acf1gridQ.rda")
sediff_acf1gridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/sediff_acf5gridQ.rda")
sediff_acf5gridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/sediff_seacf1gridQ.rda")
sediff_seacf1gridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/spikinessgridQ.rda")
spikinessgridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/stabilitygridQ.rda")
stabilitygridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/trendgridQ.rda")
trendgridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/y_acf1gridQ.rda")
y_acf1gridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/y_acf5gridQ.rda")
y_acf5gridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/y_pacf5gridQ.rda")
y_pacf5gridQ$variable <- rep(1:1700, 20)

#snaive
p1 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="snaive")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+ylab("snaive")
p2 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="snaive")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none",text = element_text(size=20))+ylab("")
p3 <- ggplot(data=diff1y_pacf5gridQ, aes_string(x=diff1y_pacf5gridQ$diff1y_pacf5, y="snaive")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("diff1y_pacf5")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")

## rwd
p4 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="rwd")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("rwd")
p5 <- ggplot(data= diff1y_pacf5gridQ, aes_string(x=diff1y_pacf5gridQ$diff1y_pacf5, y="rwd")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("diff1y_pacf5")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("")
p6 <- ggplot(data=y_pacf5gridQ, aes_string(x=y_pacf5gridQ$y_pacf5, y="rwd")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("y_pacf5")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("")

## rw
p7 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="rw")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("rw")
p8 <- ggplot(data= diff1y_pacf5gridQ, aes_string(x=diff1y_pacf5gridQ$diff1y_pacf5, y="rw")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("diff1y_pacf5")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("")
p9 <- ggplot(data=y_pacf5gridQ, aes_string(x=y_pacf5gridQ$y_pacf5, y="rw")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("y_pacf5")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("")


## ETS.NTNS
p10 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="ETS.notrendnoseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("ETS.NTNS")
p11 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="ETS.notrendnoseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("")
p12 <- ggplot(data=diff1y_pacf5gridQ, aes_string(x=diff1y_pacf5gridQ$diff1y_pacf5, y="ETS.notrendnoseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("diff1y_pacf5")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("")

## ETS.DT
p13 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="ETS.dampedtrend")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("ETS.DT")
p14 <- ggplot(data= diff1y_acf1gridQ, aes_string(x=diff1y_acf1gridQ$diff1y_acf1, y="ETS.dampedtrend")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("diff1y_acf1")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("")
p15 <- ggplot(data=betagridQ, aes_string(x=betagridQ$beta, y="ETS.dampedtrend")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("beta")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("")

## ETS.T
p16 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="ETS.trend")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("ETS.T")
p17 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="ETS.trend")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("")
p18 <- ggplot(data=y_pacf5gridQ, aes_string(x=y_pacf5gridQ$y_pacf5, y="ETS.trend")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("y_pacf5")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("")


#ETS-DTS
p19 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="ETS.dampedtrendseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+
  theme(legend.position="none", text = element_text(size=20))+ylab("ETS.DTS")
p20 <- ggplot(data= diff1y_pacf5gridQ, aes_string(x=diff1y_pacf5gridQ$diff1y_pacf5, y="ETS.dampedtrendseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("diff1y_pacf5")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none",text = element_text(size=20))+ylab("")
p21 <- ggplot(data=curvaturegridQ, aes_string(x=curvaturegridQ$curvature, y="ETS.dampedtrendseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("curvature")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")

## ETS.TS
p22 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="ETS.trendseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("ETS.TS")
p23 <- ggplot(data= diff1y_pacf5gridQ, aes_string(x=diff1y_pacf5gridQ$diff1y_pacf5, y="ETS.trendseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("diff1y_pacf5")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none",text = element_text(size=20))+ylab("")
p24 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="ETS.trendseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")

## ETS.S
p25 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="ETS.seasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("ETS.seasonal")
p26 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="ETS.seasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
p27 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="ETS.seasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")

## SARIMA
p28 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="SARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("SARIMA")
p29 <- ggplot(data= diff1y_pacf5gridQ, aes_string(x=diff1y_pacf5gridQ$diff1y_pacf5, y="SARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("diff1y_pacf5")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
p30 <- ggplot(data=diff1y_acf5gridQ, aes_string(x=diff1y_acf5gridQ$diff1y_acf5, y="SARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("diff1y_acf5")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")

(p1|p2|p3)/(p4|p5|p6)/(p7|p8|p9)/(p10|p11|p12)/(p13|p14|p15)/(p16|p17|p18)/
  (p19|p20|p21)/(p22|p23|p24)/(p25|p26|p27)/(p28|p29|p30)


## ---- pdp_quarterly2

## ARIMA
p1 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="ARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("ARIMA")
p2 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="ARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
p3 <- ggplot(data=diff1y_acf1gridQ, aes_string(x=diff1y_acf1gridQ$diff1y_acf1, y="ARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("diff1y_acf1")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")

## ARMA
p4 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("ARMA.AR.MA")
p5 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
p6 <- ggplot(data=y_acf5gridQ, aes_string(x=y_acf5gridQ$y_acf5, y="ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("y_acf5")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")


#stlar
p7 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="stlar")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("stlar")
p8 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="stlar")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none",text = element_text(size=20))+ylab("")
p9 <- ggplot(data=y_pacf5gridQ, aes_string(x=y_pacf5gridQ$y_pacf5, y="stlar")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("y_pacf5")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")

##tbats
p10 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="tbats")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("tbats")
p11 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="tbats")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
p12 <- ggplot(data=diff1y_pacf5gridQ, aes_string(x=diff1y_pacf5gridQ$diff1y_pacf5, y="tbats")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("diff1y_pacf5")+stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(legend.position="none", text = element_text(size=20))+ylab("")

## WN
p13 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="wn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("wn")
p14 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="wn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
p15 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="wn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")

## theta
p16 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="theta")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("theta")
p17 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="theta")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
p18 <- ggplot(data=diff1y_pacf5gridQ, aes_string(x=diff1y_pacf5gridQ$diff1y_pacf5, y="theta")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("sediff_acf5")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")


## nn
p19 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="nn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("nn")
p20 <- ggplot(data= y_pacf5gridQ, aes_string(x=y_pacf5gridQ$y_pacf5, y="nn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("y_pacf5")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
p21 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="nn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity") + 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+
theme(legend.position="none", text = element_text(size=20)) +ylab("")

(p1|p2|p3)/(p4|p5|p6)/(p7|p8|p9)/(p10|p11|p12)/(p13|p14|p15)/(p16|p17|p18)/
  (p19|p20|p21)

## ---- two_way_quarterly1
load("data/quarterly/trend.seasonality.q.rda")
load("data/quarterly/seasonality.spikiness.q.rda")
load("data/quarterly/seasonality.lumpiness.q.rda")
colNames <- colnames(trend.seasonality.q)[32:48]

# snaive
int1 <- ggplot(
  data = trend.seasonality.q, aes_string(x = trend.seasonality.q$trend,y = trend.seasonality.q$seasonality, z = colNames[13], fill = colNames[13]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("snaive (H=0.48)")

int2 <- ggplot(
  data = seasonality.spikiness.q, aes_string(x = seasonality.spikiness.q$seasonality,y = seasonality.spikiness.q$spikiness, z = colNames[13], fill = colNames[13]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("spikiness") + theme(legend.position="none")+ggtitle("snaive (H=0.33)")

int3 <- ggplot(
  data = seasonality.lumpiness.q, aes_string(x = seasonality.lumpiness.q$seasonality,y = seasonality.lumpiness.q$lumpiness, z = colNames[13], fill = colNames[13]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("lumpiness") + theme(legend.position="none")+ggtitle("snaive (H=0.57)")

# rwd
int4 <- ggplot(
  data = trend.seasonality.q, aes_string(x = trend.seasonality.q$trend,y = trend.seasonality.q$seasonality, z = colNames[11], fill = colNames[11]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("rwd (H=0.49)")
int5 <- ggplot(
  data = seasonality.spikiness.q, aes_string(x = seasonality.spikiness.q$seasonality,y = seasonality.spikiness.q$spikiness, z = colNames[11], fill = colNames[11]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("spikiness") + theme(legend.position="none")+ggtitle("rwd (H=0.67)")
int6 <- ggplot(
  data = seasonality.lumpiness.q, aes_string(x = seasonality.lumpiness.q$seasonality,y = seasonality.lumpiness.q$lumpiness, z = colNames[11], fill = colNames[11]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("lumpiness") + theme(legend.position="none")+ggtitle("rwd (H=0.90)")
#rw
int7 <- ggplot(
  data = trend.seasonality.q, aes_string(x = trend.seasonality.q$trend,y = trend.seasonality.q$seasonality, z = colNames[10], fill = colNames[10]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("rw (H=0.22)")
int8 <- ggplot(
  data = seasonality.spikiness.q, aes_string(x = seasonality.spikiness.q$seasonality,y = seasonality.spikiness.q$spikiness, z = colNames[10], fill = colNames[10]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("spikiness") + theme(legend.position="none")+ggtitle("rw (H=0.22)")
int9 <- ggplot(
  data = seasonality.lumpiness.q, aes_string(x = seasonality.lumpiness.q$seasonality,y = seasonality.lumpiness.q$lumpiness, z = colNames[10], fill = colNames[10]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("lumpiness") + theme(legend.position="none")+ggtitle("rw (H=0.26)")

## ETS-notrendnoseasonal
int10 <- ggplot(
  data = trend.seasonality.q, aes_string(x = trend.seasonality.q$trend,y = trend.seasonality.q$seasonality, z = colNames[5], fill = colNames[5]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("ETS-NTNS (H=0.24)")
int11 <- ggplot(
  data = seasonality.spikiness.q, aes_string(x = seasonality.spikiness.q$seasonality,y = seasonality.spikiness.q$spikiness, z = colNames[5], fill = colNames[5]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("spikiness") + theme(legend.position="none")+ggtitle("ETS-NTNS (H=0.29)")
int12 <- ggplot(
  data = seasonality.lumpiness.q, aes_string(x = seasonality.lumpiness.q$seasonality,y = seasonality.lumpiness.q$lumpiness, z = colNames[5], fill = colNames[5]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("lumpiness") + theme(legend.position="none")+ggtitle("ETS-NTNS (H=0.33)")

## ETS-dt
int13 <- ggplot(
  data = trend.seasonality.q, aes_string(x = trend.seasonality.q$trend,y = trend.seasonality.q$seasonality, z = colNames[3], fill = colNames[3]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.05), breaks = seq(0, 0.05, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("ETS-DT (H=0.19)")
int14 <- ggplot(
  data = seasonality.spikiness.q, aes_string(x = seasonality.spikiness.q$seasonality,y = seasonality.spikiness.q$spikiness, z = colNames[3], fill = colNames[3]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.05), breaks = seq(0, 0.05, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("spikiness") + theme(legend.position="none")+ggtitle("ETS-DT (H=0.14)")
int15 <- ggplot(
  data = seasonality.lumpiness.q, aes_string(x = seasonality.lumpiness.q$seasonality,y = seasonality.lumpiness.q$lumpiness, z = colNames[3], fill = colNames[3]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.05), breaks = seq(0, 0.05, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("lumpiness") + theme(legend.position="none")+ggtitle("ETS-DT (H=0.12)")

(int1|int2|int3)/(int4|int5|int6)/(int7|int8|int9)/(int10|int11|int12)/(int13|int14|int15)

## ---- two_way_quarterly2
## ETS-t
int16 <- ggplot(
  data = trend.seasonality.q, aes_string(x = trend.seasonality.q$trend,y = trend.seasonality.q$seasonality, z = colNames[7], fill = colNames[7]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.05), breaks = seq(0, 0.05, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("ETS-Trend (H=0.34)")
int17 <- ggplot(
  data = seasonality.spikiness.q, aes_string(x = seasonality.spikiness.q$seasonality,y = seasonality.spikiness.q$spikiness, z = colNames[7], fill = colNames[7]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.05), breaks = seq(0, 0.05, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("spikiness") + theme(legend.position="none")+ggtitle("ETS-Trend (H=0.09)")
int18 <- ggplot(
  data = seasonality.lumpiness.q, aes_string(x = seasonality.lumpiness.q$seasonality,y = seasonality.lumpiness.q$lumpiness, z = colNames[7], fill = colNames[7]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.05), breaks = seq(0, 0.05, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("lumpiness") + theme(legend.position="none")+ggtitle("ETS-Trend (H=0.13)")

## ETS-dampedtrendseasonal
int19 <- ggplot(
  data = trend.seasonality.q, aes_string(x = trend.seasonality.q$trend,y = trend.seasonality.q$seasonality, z = colNames[4], fill = colNames[4]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.05), breaks = seq(0, 0.05, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("ETS-DTS (H=0.49)")
int20 <- ggplot(
  data = seasonality.spikiness.q, aes_string(x = seasonality.spikiness.q$seasonality,y = seasonality.spikiness.q$spikiness, z = colNames[4], fill = colNames[4]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.05), breaks = seq(0, 0.05, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("spikiness") + theme(legend.position="none")+ggtitle("ETS-DTS (H=0.36)")
int21 <- ggplot(
  data = seasonality.lumpiness.q, aes_string(x = seasonality.lumpiness.q$seasonality,y = seasonality.lumpiness.q$lumpiness, z = colNames[4], fill = colNames[4]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.05), breaks = seq(0, 0.05, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("lumpiness") + theme(legend.position="none")+ggtitle("ETS-DTS (H=0.25)")

## ETS-trendseasonal
int22 <- ggplot(
  data = trend.seasonality.q, aes_string(x = trend.seasonality.q$trend,y = trend.seasonality.q$seasonality, z = colNames[8], fill = colNames[8]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("ETS-TS (H=0.49)")
int23 <- ggplot(
  data = seasonality.spikiness.q, aes_string(x = seasonality.spikiness.q$seasonality,y = seasonality.spikiness.q$spikiness, z = colNames[8], fill = colNames[8]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("spikiness") + theme(legend.position="none")+ggtitle("ETS-TS (H=0.37)")
int24 <- ggplot(
  data = seasonality.lumpiness.q, aes_string(x = seasonality.lumpiness.q$seasonality,y = seasonality.lumpiness.q$lumpiness, z = colNames[8], fill = colNames[8]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("lumpiness") + theme(legend.position="none")+ggtitle("ETS-TS (H=0.31)")

## ETS-seasonal
int25 <- ggplot(
  data = trend.seasonality.q, aes_string(x = trend.seasonality.q$trend,y = trend.seasonality.q$seasonality, z = colNames[6], fill = colNames[6]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("ETS-S (H=0.36)")
int26 <- ggplot(
  data = seasonality.spikiness.q, aes_string(x = seasonality.spikiness.q$seasonality,y = seasonality.spikiness.q$spikiness, z = colNames[6], fill = colNames[6]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("spikiness") + theme(legend.position="none")+ggtitle("ETS-S (H=0.40)")
int27 <- ggplot(
  data = seasonality.lumpiness.q, aes_string(x = seasonality.lumpiness.q$seasonality,y = seasonality.lumpiness.q$lumpiness, z = colNames[6], fill = colNames[6]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("lumpiness") + theme(legend.position="none")+ggtitle("ETS-S (H=0.56)")
## SARIMA
int28 <- ggplot(
  data = trend.seasonality.q, aes_string(x = trend.seasonality.q$trend,y = trend.seasonality.q$seasonality, z = colNames[12], fill = colNames[12]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("SARIMA (H=0.18)")
int29 <- ggplot(
  data = seasonality.spikiness.q, aes_string(x = seasonality.spikiness.q$seasonality,y = seasonality.spikiness.q$spikiness, z = colNames[12], fill = colNames[12]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("spikiness") + theme(legend.position="none")+ggtitle("SARIMA (H=0.21)")
int30 <- ggplot(
  data = seasonality.lumpiness.q, aes_string(x = seasonality.lumpiness.q$seasonality,y = seasonality.lumpiness.q$lumpiness, z = colNames[12], fill = colNames[12]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("lumpiness") + theme(legend.position="none")+ggtitle("SARIMA (H=0.15)")

(int16|int17|int18)/(int19|int20|int21)/(int22|int23|int24)/(int25|int26|int27)/(int28|int29|int30)

## ---- two_way_quarterly3
## ARIMA
int31 <- ggplot(
  data = trend.seasonality.q, aes_string(x = trend.seasonality.q$trend,y = trend.seasonality.q$seasonality, z = colNames[1], fill = colNames[1]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("ARIMA (H=0.20)")
int32 <- ggplot(
  data = seasonality.spikiness.q, aes_string(x = seasonality.spikiness.q$seasonality,y = seasonality.spikiness.q$spikiness, z = colNames[1], fill = colNames[1]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("spikiness") + theme(legend.position="none")+ggtitle("ARIMA (H=0.13)")
int33 <- ggplot(
  data = seasonality.lumpiness.q, aes_string(x = seasonality.lumpiness.q$seasonality,y = seasonality.lumpiness.q$lumpiness, z = colNames[1], fill = colNames[1]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("lumpiness") + theme(legend.position="none")+ggtitle("ARIMA (H=0.16)")
## ARMA
int34 <- ggplot(
  data = trend.seasonality.q, aes_string(x = trend.seasonality.q$trend,y = trend.seasonality.q$seasonality, z = colNames[2], fill = colNames[2]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("ARMA (H=0.29)")
int35 <- ggplot(
  data = seasonality.spikiness.q, aes_string(x = seasonality.spikiness.q$seasonality,y = seasonality.spikiness.q$spikiness, z = colNames[2], fill = colNames[2]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("spikiness") + theme(legend.position="none")+ggtitle("ARMA (H=0.34)")
int36 <- ggplot(
  data = seasonality.lumpiness.q, aes_string(x = seasonality.lumpiness.q$seasonality,y = seasonality.lumpiness.q$lumpiness, z = colNames[2], fill = colNames[2]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("lumpiness") + theme(legend.position="none")+ggtitle("ARMA (H=0.39)")

##stlar
int37 <- ggplot(
  data = trend.seasonality.q, aes_string(x = trend.seasonality.q$trend,y = trend.seasonality.q$seasonality, z = colNames[14], fill = colNames[14]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("stlar (H=0.87)")
int38 <- ggplot(
  data = seasonality.spikiness.q, aes_string(x = seasonality.spikiness.q$seasonality,y = seasonality.spikiness.q$spikiness, z = colNames[14], fill = colNames[14]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("spikiness") + theme(legend.position="none")+ggtitle("stlar (H=1.00)")
int39 <- ggplot(
  data = seasonality.lumpiness.q, aes_string(x = seasonality.lumpiness.q$seasonality,y = seasonality.lumpiness.q$lumpiness, z = colNames[14], fill = colNames[14]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("lumpiness") + theme(legend.position="none")+ggtitle("stlar (H=0.79)")
##tbats
int40 <- ggplot(
  data = trend.seasonality.q, aes_string(x = trend.seasonality.q$trend,y = trend.seasonality.q$seasonality, z = colNames[15], fill = colNames[15]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("tbats (H=0.85)")
int41 <- ggplot(
  data = seasonality.spikiness.q, aes_string(x = seasonality.spikiness.q$seasonality,y = seasonality.spikiness.q$spikiness, z = colNames[15], fill = colNames[15]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("spikiness") + theme(legend.position="none")+ggtitle("tbats (H=0.56)")
int42 <- ggplot(
  data = seasonality.lumpiness.q, aes_string(x = seasonality.lumpiness.q$seasonality,y = seasonality.lumpiness.q$lumpiness, z = colNames[15], fill = colNames[15]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("lumpiness") + theme(legend.position="none")+ggtitle("tbats (H=0.59)")

##wn
int43 <- ggplot(
  data = trend.seasonality.q, aes_string(x = trend.seasonality.q$trend,y = trend.seasonality.q$seasonality, z = colNames[17], fill = colNames[17]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("wn (H=0.34)")
int44 <- ggplot(
  data = seasonality.spikiness.q, aes_string(x = seasonality.spikiness.q$seasonality,y = seasonality.spikiness.q$spikiness, z = colNames[17], fill = colNames[17]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("spikiness") + theme(legend.position="none")+ggtitle("wn (H=0.31)")
int45 <- ggplot(
  data = seasonality.lumpiness.q, aes_string(x = seasonality.lumpiness.q$seasonality,y = seasonality.lumpiness.q$lumpiness, z = colNames[17], fill = colNames[17]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("lumpiness") + theme(legend.position="none")+ggtitle("wn (H=0.40)")

(int31|int32|int33)/(int34|int35|int36)/(int37|int38|int39)/(int40|int41|int42)/(int43|int44|int45)

## ---- two_way_quarterly4
## theta
int46 <- ggplot(
  data = trend.seasonality.q, aes_string(x = trend.seasonality.q$trend,y = trend.seasonality.q$seasonality, z = colNames[16], fill = colNames[16]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("theta (H=1.00)")
int47 <- ggplot(
  data = seasonality.spikiness.q, aes_string(x = seasonality.spikiness.q$seasonality,y = seasonality.spikiness.q$spikiness, z = colNames[16], fill = colNames[16]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("spikiness") + theme(legend.position="none")+ggtitle("theta (H=0.89)")
int48 <- ggplot(
  data = seasonality.lumpiness.q, aes_string(x = seasonality.lumpiness.q$seasonality,y = seasonality.lumpiness.q$lumpiness, z = colNames[16], fill = colNames[16]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("lumpiness") + theme(legend.position="none")+ggtitle("theta (H=0.74)")

## nn
int49 <- ggplot(
  data = trend.seasonality.q, aes_string(x = trend.seasonality.q$trend,y = trend.seasonality.q$seasonality, z = colNames[9], fill = colNames[9]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("nn (H=0.77)")
int50 <- ggplot(
  data = seasonality.spikiness.q, aes_string(x = seasonality.spikiness.q$seasonality,y = seasonality.spikiness.q$spikiness, z = colNames[9], fill = colNames[9]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("spikiness") + theme(legend.position="none")+ggtitle("nn (H=0.33)")
int51 <- ggplot(
  data = seasonality.lumpiness.q, aes_string(x = seasonality.lumpiness.q$seasonality,y = seasonality.lumpiness.q$lumpiness, z = colNames[9], fill = colNames[9]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("seasonality") + ylab("lumpiness") + theme(legend.position="none")+ggtitle("nn (H=0.66)")

(int46|int47|int48)/(int49|int50|int51)

## ---- quarterly_pca
pcaQvariables <- quarterly_training[, 1:30]
pcaM4Q <- prcomp(pcaQvariables, center = TRUE, scale = TRUE)
# summary(pcaM1Y)
PC1m4q <- pcaM4Q$x[, 1]
PC2m4q <- pcaM4Q$x[, 2]
PC3m4q <- pcaM4Q$x[, 3]
m4qPCAresults <- data.frame(PC1 = PC1m4q, PC2 = PC2m4q, PC3 = PC3m4q, pcaQvariables)
m4qPCAresults$predicted <- trainQ_predictions_oob
pca1M4Q_snaive <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "snaive", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "rwd") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_rwd <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "rwd", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "rwd") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_rw <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "rw", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "rw") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))


pca1M4Q_notrend <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "ETS-notrendnoseasonal", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-notrendnoseasonal") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_etsdamtrend <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "ETS-dampedtrend", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-dampedtrend") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_etstrend <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "ETS-trend", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-trend") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_etsdtrends <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "ETS-dampedtrendseasonal", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-DTS") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_trends <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "ETS-trendseasonal", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-trendseasonal") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_s <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "ETS-seasonal", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-seasonal") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_sarima <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "SARIMA", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "SARIMA") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_ARIMA <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "ARIMA", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ARIMA") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_ARMA <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "ARMA/AR/MA", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ARMA/AR/MA") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_stlar <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "stlar", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "stlar") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_tbats <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "tbats", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "stlar") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_wn <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "wn", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "wn") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_theta <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "theta", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "theta") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Q_nn <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4qPCAresults[m4qPCAresults$predicted == "nn", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "nn") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))


(pca1M4Q_snaive|pca1M4Q_rwd|pca1M4Q_rw|pca1M4Q_notrend|pca1M4Q_etsdamtrend)/
  (pca1M4Q_etstrend|pca1M4Q_etsdtrends|pca1M4Q_trends|pca1M4Q_s|pca1M4Q_sarima)/
  (pca1M4Q_ARIMA|pca1M4Q_ARMA|pca1M4Q_stlar|pca1M4Q_tbats|pca1M4Q_wn)/
  (pca1M4Q_theta|pca1M4Q_nn )

################################################################################
#                      Monthly  data                                           #
################################################################################

## ---- oob_monthly
load("data/monthly/trainM_votes.rda") #oob votes from the random forest
load("data/monthly/trainM_predictions_oob.rda") # based on oob prediction
load("data/monthly/monthly_training.rda") # random forest training set
votes_oobM <- data.frame(trainM_votes)
names(votes_oobM) <- names(table(trainM_predictions_oob))
votes_oobM$predicted <- trainM_predictions_oob
votes_oobM$classlabel <- monthly_training$classlabels
votes_oobM <- votes_oobM %>% mutate(id=seq_len(n())) %>%
  melt(id.var=c('classlabel','id','predicted'), na.rm=T) %>%
  select(-id)
votes_oobM$classlabel <- factor(votes_oobM$classlabel, levels=rev(c("snaive","rwd", "rw", "ETS-notrendnoseasonal","ETS-dampedtrend", "ETS-trend", "ETS-dampedtrendseasonal", "ETS-trendseasonal","ETS-seasonal","SARIMA",
                                                                    "ARIMA", "ARMA/AR/MA","stlar" ,"tbats","wn", "theta","nn"))
)
oob_monthly <- ggplot(votes_oobM, aes(x = variable, y = value, fill = classlabel)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.4) +
  ylab("Classification error based on OOB error") +
  xlab("") + 
  theme(legend.position = "right", legend.title = element_blank(), legend.text.align = 0, text = element_text(size=20)) + 
  guides(fill=guide_legend(reverse=TRUE)) +
  scale_x_discrete(limits=rev(c("snaive","rwd", "rw", "ETS-notrendnoseasonal","ETS-dampedtrend", "ETS-trend", "ETS-dampedtrendseasonal", "ETS-trendseasonal","ETS-seasonal","SARIMA",
                                "ARIMA", "ARMA/AR/MA","stlar" ,"tbats","wn", "theta","nn"))) +
  coord_flip() 
oob_monthly

## ---- vi_monthly
load("data/monthly/trainM_importance.rda")
load(file="data/monthly/sd_pdf_dfM.rda")
load(file="data/monthly/sd_ice_dfM.rda")
## Permutation based
train_imp_dfM <- data.frame(trainM_importance)
train_imp_dfM <- add_rownames(train_imp_dfM, "Feature")
train_imp_dfM <- within(train_imp_dfM, rm("MeanDecreaseAccuracy", "MeanDecreaseGini"))
permutation_impM <- train_imp_dfM %>% melt(id.vars = "Feature")
colnames(permutation_impM) <- c("feature", "class", "score")

## PDP-based
sd_pdf_dfM <- add_rownames(sd_pdf_dfM, "class")
pdp_impM <- sd_pdf_dfM %>% melt(id.vars = "class")
colnames(pdp_impM) <- c("class", "feature", "score")

## ICE-based
sd_ice_dfM <- add_rownames(sd_ice_dfM, "class")
ice_impM <- sd_ice_dfM %>% melt(id.vars = "class")
colnames(ice_impM) <- c("class", "feature", "score")

## Combine the data frames
importancescoreM <- bind_rows(permutation_impM, pdp_impM)
importancescoreM <- bind_rows(importancescoreM, ice_impM)
importancescoreM$VI <- rep(c("permutation", "PDP", "ICE"), each = 510)

## rank permutation, sd_pdp, and sd_ice scores for each class
importancescoreM$class <- factor(importancescoreM$class,
                                 levels = c(
                                   "snaive", "rwd", "rw", "ETS.notrendnoseasonal", "ETS.dampedtrend", "ETS.trend", "ETS.dampedtrendseasonal", "ETS.trendseasonal", "ETS.seasonal", "SARIMA",
                                   "ARIMA", "ARMA.AR.MA", "stlar", "tbats", "wn", "theta", "nn"
                                 ),
                                 labels = c(
                                   "snaive", "rwd", "rw", "ETS.NTNS", "ETS.DT", "ETS.T", "ETS.DTS", "ETS.TS", "ETS.S", "SARIMA",
                                   "ARIMA", "ARMA.AR.MA", "stlar", "tbats", "wn", "theta", "nn"
                                 )
)

rank_vi_monthly_classes <- importancescoreM %>%
  group_by(VI, class) %>%
  mutate(rank = min_rank(score))

## compute mean rank
meanrank_vim_classes <- rank_vi_monthly_classes %>% group_by(feature, class) %>% summarise_at(vars(c(rank)), funs(mean))

## overall importance of features to the forest
train_impforestM <- data.frame(trainM_importance)
train_impforestM <- add_rownames(train_impforestM, "Feature")
train_impforestM <- train_impforestM[, c("Feature", "MeanDecreaseAccuracy", "MeanDecreaseGini")]
train_impforestM <- train_impforestM %>%
  mutate(rank_permu = min_rank(MeanDecreaseAccuracy)) %>%
  mutate(rank_gini = min_rank(MeanDecreaseGini))
train_impforestM$mean_rank <- (train_impforestM$rank_permu + train_impforestM$rank_gini) / 2
meanrank_vim_forest <- data.frame(
  feature = train_impforestM$Feature,
  class = rep("overall", 30),
  rank = train_impforestM$mean_rank
)
## combine mean ranks for overall forest and separate classes
meanrank_monthly <- dplyr::bind_rows(meanrank_vim_forest, meanrank_vim_classes)
## create horizontal bar chart for ranks
orderOverall <- filter(meanrank_monthly, class == "overall")
meanrank_monthly$feature <- factor(meanrank_monthly$feature, levels = orderOverall$feature[order(orderOverall$rank)])
meanrank_monthly$class <- factor(meanrank_monthly$class,
                                   levels = c(
                                     "overall", "snaive", "rwd", "rw", "ETS.NTNS", "ETS.DT", "ETS.T", "ETS.DTS", "ETS.TS", "ETS.S", "SARIMA",
                                     "ARIMA", "ARMA.AR.MA", "stlar", "tbats", "wn", "theta", "nn"
                                   ),
                                   labels = c(
                                     "overall", "snaive", "rwd", "rw", "ETS.NTNS", "ETS.DT", "ETS.T", "ETS.DTS", "ETS.TS", "ETS.S", "SARIMA",
                                     "ARIMA", "ARMA", "stlar", "tbats", "wn", "theta", "nn"
                                   )
)
feaImp_monthly <- ggplot(meanrank_monthly, aes(y = rank, x = feature)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~class, ncol = 9, nrow = 2) +
  coord_flip() + ylab("Average rank")
feaImp_monthly


## ---- pdp_monthly1
load("data/monthly/pdp_monthly/alphagridM.rda")
alphagridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/betagridM.rda")
betagridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/curvaturegridM.rda")
curvaturegridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/diff1y_acf1gridM.rda")
diff1y_acf1gridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/diff1y_acf5gridM.rda")
diff1y_acf5gridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/diff1y_pacf5gridM.rda")
diff1y_pacf5gridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/diff2y_acf1gridM.rda")
diff2y_acf1gridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/diff2y_acf5gridM.rda")
diff2y_acf5gridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/diff2y_pacf5gridM.rda")
diff2y_pacf5gridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/e_acf1gridM.rda")
e_acf1gridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/entropygridM.rda")
entropygridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/hurstgridM.rda")
hurstgridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/hwalphagridM.rda")
hwalphagridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/hwbetagridM.rda")
hwbetagridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/hwgammagridM.rda")
hwgammagridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/linearitygridM.rda")
linearitygridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/lumpinessgridM.rda")
lumpinessgridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/NgridM.rda")
NgridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/nonlinearitygridM.rda")
nonlinearitygridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/seas_pacfgridM.rda")
seas_pacfgridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/seasonalitygridM.rda")
seasonalitygridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/sediff_acf1gridM.rda")
sediff_acf1gridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/sediff_acf5gridM.rda")
sediff_acf5gridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/sediff_seacf1gridM.rda")
sediff_seacf1gridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/spikinessgridM.rda")
spikinessgridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/stabilitygridM.rda")
stabilitygridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/trendgridM.rda")
trendgridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/y_acf1gridM.rda")
y_acf1gridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/y_acf5gridM.rda")
y_acf5gridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/y_pacf5gridM.rda")
y_pacf5gridM$variable <- rep(1:1700, 20)


#snaive
p1 <- ggplot(data=seasonalitygridM, aes_string(x=seasonalitygridM$seasonality, y="snaive")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("seasonality")+ theme(legend.position="none", text = element_text(size=20))+ylab("snaive")
p2 <- ggplot(data= trendgridM, aes_string(x=trendgridM$trend, y="snaive")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none",text = element_text(size=20))+ylab("")
p3 <- ggplot(data=hwgammagridM, aes_string(x=hwgammagridM$hwgamma, y="snaive")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("hwgamma")+ theme(legend.position="none", text = element_text(size=20))+ylab("")

## rwd
p4 <- ggplot(data=seasonalitygridM, aes_string(x=seasonalitygridM$seasonality, y="rwd")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("seasonality")+ theme(legend.position="none", text = element_text(size=20))+ylab("rwd")
p5 <- ggplot(data= trendgridM, aes_string(x=trendgridM$trend, y="rwd")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("trend")+ theme(legend.position="none", text = element_text(size=20))+ylab("")
p6 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="rwd")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("N")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")

## rw
p7 <- ggplot(data=seasonalitygridM, aes_string(x=seasonalitygridM$seasonality, y="rw")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("rw")
p8 <- ggplot(data= trendgridM, aes_string(x=trendgridM$trend, y="rw")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
p9 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="rw")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("N")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")

## ETS.NTNS
p10 <- ggplot(data=seasonalitygridM, aes_string(x=seasonalitygridM$seasonality, y="ETS.notrendnoseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("ETS.NTNS")
p11 <- ggplot(data= trendgridM, aes_string(x=trendgridM$trend, y="ETS.notrendnoseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
p12 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="ETS.notrendnoseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("N")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")

## ETS.DT
p13 <- ggplot(data=seasonalitygridM, aes_string(x=seasonalitygridM$seasonality, y="ETS.dampedtrend")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("ETS.DT")
p14 <- ggplot(data= diff1y_acf1gridM, aes_string(x=diff1y_acf1gridM$diff1y_acf1, y="ETS.dampedtrend")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("diff1y_acf1")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
p15 <- ggplot(data=betagridM, aes_string(x=betagridM$beta, y="ETS.dampedtrend")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("beta")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")


## ETS.T
p16 <- ggplot(data=seasonalitygridM, aes_string(x=seasonalitygridM$seasonality, y="ETS.trend")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("seasonality")+ theme(legend.position="none", text = element_text(size=20))+ylab("ETS.T")
p17 <- ggplot(data= trendgridM, aes_string(x=trendgridM$trend, y="ETS.trend")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
p18 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="ETS.trend")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("N")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")

#ETS-DTS
p19 <- ggplot(data=seasonalitygridM, aes_string(x=seasonalitygridM$seasonality, y="ETS.dampedtrendseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("seasonality")+ theme(legend.position="none", text = element_text(size=20))+ylab("ETS.DTS")
p20 <- ggplot(data= sediff_acf5gridM, aes_string(x=sediff_acf5gridM$sediff_acf5, y="ETS.dampedtrendseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("sediff_acf5")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none",text = element_text(size=20))+ylab("")
p21 <- ggplot(data=hwgammagridM, aes_string(x=hwgammagridM$hwgamma, y="ETS.dampedtrendseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("hwgamma")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")

## ETS.TS
p22 <- ggplot(data=seasonalitygridM, aes_string(x=seasonalitygridM$seasonality, y="ETS.trendseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("ETS.TS")
p23 <- ggplot(data= trendgridM, aes_string(x=trendgridM$trend, y="ETS.trendseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
p24 <- ggplot(data=hwgammagridM, aes_string(x=hwgammagridM$hwgamma, y="ETS.trendseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("hwgamma")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")

## ETS.S
p25 <- ggplot(data=seasonalitygridM, aes_string(x=seasonalitygridM$seasonality, y="ETS.seasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("ETS.seasonal")
p26 <- ggplot(data= trendgridM, aes_string(x=trendgridM$trend, y="ETS.seasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
p27 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="ETS.seasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("N")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")

## SARIMA
p28 <- ggplot(data=hwgammagridM, aes_string(x=hwgammagridM$hwgamma, y="SARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("hwgamma")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("SARIMA")
p29 <- ggplot(data= trendgridM, aes_string(x=trendgridM$trend, y="SARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
p30 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="SARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("N")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")

(p1|p2|p3)/(p4|p5|p6)/(p7|p8|p9)/(p10|p11|p12)/(p13|p14|p15)/(p16|p17|p18)/
  (p19|p20|p21)/(p22|p23|p24)/(p25|p26|p27)/(p28|p29|p30)

## ---- pdp_monthly2
## ARIMA
p1 <- ggplot(data=seasonalitygridM, aes_string(x=seasonalitygridM$seasonality, y="ARIMA"))+ 
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("seasonality")+ theme(legend.position="none", text = element_text(size=20))+ylab("ARIMA")
p2 <- ggplot(data= trendgridM, aes_string(x=trendgridM$trend, y="ARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("trend")+ theme(legend.position="none", text = element_text(size=20))+ylab("")
p3 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="ARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("N")+ theme(legend.position="none", text = element_text(size=20))+ylab("")

## ARMA
p4 <- ggplot(data=seasonalitygridM, aes_string(x=seasonalitygridM$seasonality, y="ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("seasonality")+ theme(legend.position="none", text = element_text(size=20))+ylab("ARMA.AR.MA")
p5 <- ggplot(data= trendgridM, aes_string(x=trendgridM$trend, y="ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("trend")+ theme(legend.position="none", text = element_text(size=20))+ylab("")
p6 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("N")+ theme(legend.position="none", text = element_text(size=20))+ylab("")

#stlar
p7 <- ggplot(data=seasonalitygridM, aes_string(x=seasonalitygridM$seasonality, y="stlar")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("seasonality")+ theme(legend.position="none", text = element_text(size=20))+ylab("stlar")
p8 <- ggplot(data= trendgridM, aes_string(x=trendgridM$trend, y="stlar")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("trend")+ theme(legend.position="none",text = element_text(size=20))+ylab("")
p9 <- ggplot(data=y_pacf5gridM, aes_string(x=y_pacf5gridM$y_pacf5, y="stlar")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("y_pacf5")+ theme(legend.position="none", text = element_text(size=20))+ylab("")

##tbats
p10 <- ggplot(data=seasonalitygridM, aes_string(x=seasonalitygridM$seasonality, y="tbats")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("seasonality")+ theme(legend.position="none", text = element_text(size=20))+ylab("tbats")
p11 <- ggplot(data= trendgridM, aes_string(x=trendgridM$trend, y="tbats")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("trend")+ theme(legend.position="none", text = element_text(size=20))+ylab("")
p12 <- ggplot(data=sediff_acf5gridM, aes_string(x=sediff_acf5gridM$sediff_acf5, y="tbats")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("sediff_acf5")+ theme(legend.position="none", text = element_text(size=20))+ylab("")

## WN
p13 <- ggplot(data=seasonalitygridM, aes_string(x=seasonalitygridM$seasonality, y="wn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("seasonality")+ theme(legend.position="none", text = element_text(size=20))+ylab("wn")
p14 <- ggplot(data= trendgridM, aes_string(x=trendgridM$trend, y="wn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("trend")+ theme(legend.position="none", text = element_text(size=20))+ylab("")
p15 <- ggplot(data=stabilitygridM, aes_string(x=stabilitygridM$stability, y="wn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("stability")+ theme(legend.position="none", text = element_text(size=20))+ylab("")

## theta
p16 <- ggplot(data=seasonalitygridM, aes_string(x=seasonalitygridM$seasonality, y="theta")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("seasonality")+ theme(legend.position="none", text = element_text(size=20))+ylab("theta")
p17 <- ggplot(data= trendgridM, aes_string(x=trendgridM$trend, y="theta")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("trend")+ theme(legend.position="none", text = element_text(size=20))+ylab("")
p18 <- ggplot(data=sediff_acf5gridM, aes_string(x=sediff_acf5gridM$sediff_acf5, y="theta")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("sediff_acf5")+ theme(legend.position="none", text = element_text(size=20))+ylab("")

## nn
p19 <- ggplot(data=seasonalitygridM, aes_string(x=seasonalitygridM$seasonality, y="nn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("seasonality")+ theme(legend.position="none", text = element_text(size=20))+ylab("nn")
p20 <- ggplot(data= trendgridM, aes_string(x=trendgridM$trend, y="nn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("trend")+ theme(legend.position="none", text = element_text(size=20))+ylab("")
p21 <- ggplot(data=stabilitygridM, aes_string(x=stabilitygridM$stability, y="nn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+xlab("stability") + theme(legend.position="none", text = element_text(size=20)) +ylab("")

(p1|p2|p3)/(p4|p5|p6)/(p7|p8|p9)/(p10|p11|p12)/(p13|p14|p15)/(p16|p17|p18)/
  (p19|p20|p21)


## ---- two_way_monthly1
load("data/monthly/seasinality.trend.m.rda")
load("data/monthly/N.seasonality.m.rda")
load("data/monthly/N.hwgamma.m.rda")
colNames <- colnames(seasinality.trend.m)[32:48]

# snaive
int1 <- ggplot(
  data = seasinality.trend.m, aes_string(x = seasinality.trend.m$trend,y = seasinality.trend.m$seasonality, z = colNames[13], fill = colNames[13]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("snaive (H=0.72)")
int2 <- ggplot(
  data = N.seasonality.m, aes_string(x = N.seasonality.m$N,y = N.seasonality.m$seasonality, z = colNames[13], fill = colNames[13]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("N") + ylab("seasonality") + theme(legend.position="none")+ggtitle("snaive (H=0.79)")
int3 <- ggplot(
  data = N.hwgamma.m, aes_string(x = N.hwgamma.m$N,y = N.hwgamma.m$hwgamma, z = colNames[13], fill = colNames[13]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.15), breaks = seq(0, 0.15, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("N") + ylab("hwgamma") + theme(legend.position="none")+ggtitle("snaive (H=0.58)")
## rwd
int4 <- ggplot(
  data = seasinality.trend.m, aes_string(x = seasinality.trend.m$trend,y = seasinality.trend.m$seasonality, z = colNames[11], fill = colNames[11]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("rwd (H=0.32)")
int5 <- ggplot(
  data = N.seasonality.m, aes_string(x = N.seasonality.m$N,y = N.seasonality.m$seasonality, z = colNames[11], fill = colNames[11]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("N") + ylab("seasonality") + theme(legend.position="none")+ggtitle("rwd (H=0.53)")
int6 <- ggplot(
  data = N.hwgamma.m, aes_string(x = N.hwgamma.m$N,y = N.hwgamma.m$hwgamma, z = colNames[11], fill = colNames[11]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.15), breaks = seq(0, 0.15, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("N") + ylab("hwgamma") + theme(legend.position="none")+ggtitle("rwd (H=0.24)")
## rw
int7 <- ggplot(
  data = seasinality.trend.m, aes_string(x = seasinality.trend.m$trend,y = seasinality.trend.m$seasonality, z = colNames[10], fill = colNames[10]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("rw (H=0.15)")
int8 <- ggplot(
  data = N.seasonality.m, aes_string(x = N.seasonality.m$N,y = N.seasonality.m$seasonality, z = colNames[10], fill = colNames[10]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("N") + ylab("seasonality") + theme(legend.position="none")+ggtitle("rw (H=0.28)")
int9 <- ggplot(
  data = N.hwgamma.m, aes_string(x = N.hwgamma.m$N,y = N.hwgamma.m$hwgamma, z = colNames[10], fill = colNames[10]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("N") + ylab("hwgamma") + theme(legend.position="none")+ggtitle("rw (H=0.46)")
## ETS-notnos
int7 <- ggplot(
  data = seasinality.trend.m, aes_string(x = seasinality.trend.m$trend,y = seasinality.trend.m$seasonality, z = colNames[5], fill = colNames[5]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("ETS-NTNS (H=0.34)")
int8 <- ggplot(
  data = N.seasonality.m, aes_string(x = N.seasonality.m$N,y = N.seasonality.m$seasonality, z = colNames[5], fill = colNames[5]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("N") + ylab("seasonality") + theme(legend.position="none")+ggtitle("ETS-NTNS (H=0.73)")
int9 <- ggplot(
  data = N.hwgamma.m, aes_string(x = N.hwgamma.m$N,y = N.hwgamma.m$hwgamma, z = colNames[5], fill = colNames[5]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("N") + ylab("hwgamma") + theme(legend.position="none")+ggtitle("ETS-NTNS ")
## ETS-dt
int10 <- ggplot(
  data = seasinality.trend.m, aes_string(x = seasinality.trend.m$trend,y = seasinality.trend.m$seasonality, z = colNames[3], fill = colNames[3]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("ETS-DT (H=0.13)")
int11 <- ggplot(
  data = N.seasonality.m, aes_string(x = N.seasonality.m$N,y = N.seasonality.m$seasonality, z = colNames[3], fill = colNames[3]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("N") + ylab("seasonality") + theme(legend.position="none")+ggtitle("ETS-DT (H=0.37)")
int12 <- ggplot(
  data = N.hwgamma.m, aes_string(x = N.hwgamma.m$N,y = N.hwgamma.m$hwgamma, z = colNames[3], fill = colNames[3]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("N") + ylab("hwgamma") + theme(legend.position="none")+ggtitle("ETS-DT (H=0.57)")
## rw
int13 <- ggplot(
  data = seasinality.trend.m, aes_string(x = seasinality.trend.m$trend,y = seasinality.trend.m$seasonality, z = colNames[7], fill = colNames[7]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("trend") + ylab("seasonality") + theme(legend.position="none")+ggtitle("ETS-T (H=0.37)")
int14 <- ggplot(
  data = N.seasonality.m, aes_string(x = N.seasonality.m$N,y = N.seasonality.m$seasonality, z = colNames[7], fill = colNames[7]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("N") + ylab("seasonality") + theme(legend.position="none")+ggtitle("ETS-T (H=0.17)")
int15 <- ggplot(
  data = N.hwgamma.m, aes_string(x = N.hwgamma.m$N,y = N.hwgamma.m$hwgamma, z = colNames[7], fill = colNames[7]
  ))+geom_tile() + 
  scale_fill_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), low = "#edf8b1", high = "#2c7fb8") +
  xlab("N") + ylab("hwgamma") + theme(legend.position="none")+ggtitle("ETS-T (H=0.41)")

(int1|int2|int3)/(int4|int5|int6)/(int7|int8|int9)/(int10|int11|int12)/(int13|int14|int15)

## ---- monthly_pca
pcaMvariables <- monthly_training[, 1:30]
pcaM4M <- prcomp(pcaMvariables, center = TRUE, scale = TRUE)
PC1m4m <- pcaM4M$x[, 1]
PC2m4m <- pcaM4M$x[, 2]
PC3m4m <- pcaM4M$x[, 3]
#which.max(PC2m4m)249937
m4mPCAresults <- data.frame(PC1 = PC1m4m, PC2 = PC2m4m, PC3 = PC3m4m, pcaMvariables)
m4mPCAresults$predicted <- trainM_predictions_oob
m4mPCAresults <- m4mPCAresults[-249937, ]
pca1M4M_snaive <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "snaive", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "rwd") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4M_rwd <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "rwd", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "rwd") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4M_rw <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "rw", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "rw") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))


pca1M4M_notrend <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ETS-notrendnoseasonal", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-notrendnoseasonal") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4M_etsdamtrend <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ETS-dampedtrend", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-dampedtrend") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4M_etstrend <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ETS-trend", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-trend") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4M_etsdtrends <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ETS-dampedtrendseasonal", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-DTS") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4M_trends <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ETS-trendseasonal", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-trendseasonal") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4M_s <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ETS-seasonal", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-seasonal") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4M_sarima <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "SARIMA", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "SARIMA") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4M_ARIMA <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ARIMA", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ARIMA") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4M_ARMA <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ARMA/AR/MA", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ARMA/AR/MA") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4M_stlar <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "stlar", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "stlar") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4M_tbats <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "tbats", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "stlar") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4M_wn <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "wn", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "wn") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4M_theta <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "theta", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "theta") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4M_nn <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "nn", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "nn") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))


(pca1M4M_snaive|pca1M4M_rwd|pca1M4M_rw|pca1M4M_notrend|pca1M4M_etsdamtrend)/
  (pca1M4M_etstrend|pca1M4M_etsdtrends|pca1M4M_trends|pca1M4M_s|pca1M4M_sarima)/
  (pca1M4M_ARIMA|pca1M4M_ARMA|pca1M4M_stlar|pca1M4M_tbats|pca1M4M_wn)/
  (pca1M4M_theta|pca1M4M_nn )





##################################################################################
#                     Weekly data                                                #
##################################################################################

## ---- oob_weekly


################################################################
#              Daily data                                      #
################################################################



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
votes_oobH$classlabel <- factor(votes_oobH$classlabel, levels=rev(c("tbats","stlar", "mstlarima", "mstlets","snaive", "rw", "rwd", "theta","nn","wn"))
)
horizontalbar_hourly <- ggplot(votes_oobH, aes(x = variable, y = value, fill = classlabel)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.4) +
  ylab("Classification error based on OOB error") +
  xlab("") + 
  theme(legend.position = "right", legend.title = element_blank(), legend.text.align = 0, text = element_text(size=20)) + 
  guides(fill=guide_legend(reverse=TRUE)) +
  scale_x_discrete(limits=rev(c("tbats","stlar", "mstlarima", "mstlets","snaive", "rw", "rwd", "theta","nn","wn"))) +
  coord_flip() 
horizontalbar_hourly

## ----pdp_hourly



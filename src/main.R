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
  ylab("Classification error based on OOB error") +
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
p24 <- ggplot(data = y_pacf5grid, aes_string(x = y_pacf5grid$y_pacf5, y = "wn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("y_pacf5") +
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

(p1 | p2 | p3) / (p4 | p5 | p6) / (p7 | p8 | p9) / (p10 | p11 | p12) / (p13 | p14 | p15) / (p16 | p17 | p18) / (p19 | p20 | p21) / (p22 | p23 | p24) / (p25 | p26 | p27) / (p28 | p29 | p30)

## ---- sankey_yearly

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

## ---- functionfriedman
source("src/friedmanHstatmatrix.R")

#################################################################
#                  Yearly data                               #
#################################################################

## ---- yearlyoob
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

## ---- viyearly
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

meanrank_yearly$rn <- 1:275

top <- meanrank_yearly %>%
  group_by(class) %>%
  top_n(n = 5, wt = rank)

meanrank_yearly$istop <- ifelse(meanrank_yearly$rn%in%top$rn, TRUE, FALSE)

feaImp_yearly <- ggplot(meanrank_yearly, aes(y = rank, x = feature, fill=as.factor(istop))) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~class, ncol = 6, nrow = 2) +
  coord_flip() + ylab("Average rank")+ 
  scale_fill_manual(breaks=c("0","1"), values=c("black","red"), guide="none")+theme(text=element_text(size = 20))
feaImp_yearly

## ---- pdpyearly
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
p1 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "rwd")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("rwd")
p2 <- ggplot(data = trendgrid, aes_string(x = trendgrid$trend, y = "rwd")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
p3 <- ggplot(data = linearitygrid, aes_string(x = linearitygrid$linearity, y = "rwd")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
pda1 <- ggplot(data = diff1y_acf1grid, aes_string(x = diff1y_acf1grid$diff1y_acf1, y = "rwd")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("diff1y_acf1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")

# rw
p4 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "rw")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + xlab("ur_pp") + ylab("random walk")
p5 <- ggplot(data = trendgrid, aes_string(x = trendgrid$trend, y = "rw")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
p6 <- ggplot(data = linearitygrid, aes_string(x = linearitygrid$linearity, y = "rw")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
pda2 <- ggplot(data = diff1y_acf1grid, aes_string(x = diff1y_acf1grid$diff1y_acf1, y = "rw")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("diff1y_acf1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")

# ETS-trend
p7 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "ETS.trend")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + xlab("ur_pp") + ylab("ETS-trend")
p8 <- ggplot(data = trendgrid, aes_string(x = trendgrid$trend, y = "ETS.trend")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
p9 <- ggplot(data =linearitygrid, aes_string(x = linearitygrid$linearity, y = "ETS.trend")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
pda3 <- ggplot(data = diff1y_acf1grid, aes_string(x = diff1y_acf1grid$diff1y_acf1, y = "ETS.trend")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("diff1y_acf1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")

# ETS-dampedtrend
p10 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "ETS.dampedtrend")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + xlab("ur_pp") + ylab("ETS-dampedtrend")
p11 <- ggplot(data = trendgrid, aes_string(x = trendgrid$trend, y = "ETS.dampedtrend")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
p12 <- ggplot(data = linearitygrid, aes_string(x = linearitygrid$linearity, y = "ETS.dampedtrend")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
pda4 <- ggplot(data = diff1y_acf1grid, aes_string(x = diff1y_acf1grid$diff1y_acf1, y = "ETS.dampedtrend")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("diff1y_acf1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
# ETS-notrendnoseasonal
p13 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "ETS.notrendnoseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("ETS-notrendnoseasonal")
p14 <- ggplot(data = trendgrid, aes_string(x = trendgrid$trend, y = "ETS.notrendnoseasonal")) +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") + theme(legend.position = "none") + ylab("")
p15 <- ggplot(data = linearitygrid, aes_string(x = linearitygrid$linearity, y = "ETS.notrendnoseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
pda5 <- ggplot(data = diff1y_acf1grid, aes_string(x = diff1y_acf1grid$diff1y_acf1, y = "ETS.notrendnoseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("diff1y_acf1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
# ARIMA
p16 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "ARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("ARIMA")
p17 <- ggplot(data = trendgrid, aes_string(x = trendgrid$trend, y = "ARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
p18 <- ggplot(data = linearitygrid, aes_string(x = linearitygrid$linearity, y = "ARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
pda6 <- ggplot(data = diff1y_acf1grid, aes_string(x = diff1y_acf1grid$diff1y_acf1, y = "ARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("diff1y_acf1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
# ARMA.AR.MA
p19 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("ARMA.AR.MA")
p20 <- ggplot(data = trendgrid, aes_string(x = trendgrid$trend, y = "ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
p21 <- ggplot(data = linearitygrid, aes_string(x = linearitygrid$linearity, y = "ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
pda7 <- ggplot(data = diff1y_acf1grid, aes_string(x = diff1y_acf1grid$diff1y_acf1, y = "ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("diff1y_acf1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
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
pda8 <- ggplot(data = diff1y_acf1grid, aes_string(x = diff1y_acf1grid$diff1y_acf1, y = "wn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("diff1y_acf1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
# theta
p25 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "theta")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("theta")
p26 <- ggplot(data = trendgrid, aes_string(x = trendgrid$trend, y = "theta")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
p27 <- ggplot(data = linearitygrid, aes_string(x = linearitygrid$linearity, y = "theta")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
pda9 <- ggplot(data = diff1y_acf1grid, aes_string(x = diff1y_acf1grid$diff1y_acf1, y = "theta")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("diff1y_acf1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
## nn
p28 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "nn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("nn")
p29 <- ggplot(data = trendgrid, aes_string(x = trendgrid$trend, y = "nn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
p30 <- ggplot(data = linearitygrid, aes_string(x = linearitygrid$linearity, y = "nn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position = "none") + ylab("")
pda10 <- ggplot(data = diff1y_acf1grid, aes_string(x = diff1y_acf1grid$diff1y_acf1, y = "nn")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("diff1y_acf1") +
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + ylab("")
(p1 | p2 | p3|pda1) / (p4 | p5 | p6|pda2) / (p7 | p8 | p9|pda3) / (p10 | p11 | p12|pda4) / (p13 | p14 | p15|pda5) / (p16 | p17 | p18|pda6) / (p19 | p20 | p21|pda7) / (p22 | p23 | p24|pda8) / (p25 | p26 | p27|pda9) / (p28 | p29 | p30|pda10)

## ---- sankeyyearly
load("data/yearly/rf.yearly.all.paths.rda")
nd3y <- rf_sankey(all.paths.out = rf.yearly.all.paths, all.nodes = FALSE, plot.node.lim = 6)
sankeyNetwork(
  Links = nd3y$links, Nodes = nd3y$nodes,
  Source = "source", Target = "target", Value = "value",
  NodeID = "name", units = "Count", fontSize = 12,
  nodeWidth = 30, NodeGroup = NULL
)

## ---- friedmany
load("data/friedmanHstat_yearly.rda")
col.order <- c("trend", "ur_pp","spikiness", "beta",
               "diff1y_acf1", "linearity", "diff1y_acf5", "curvature",
               "lmres_acf1","y_pacf5", "ur_kpss", "y_acf1", "nonlinearity",
               "alpha", "diff1y_pacf5", "hurst", "entropy", "e_acf1", "y_acf5",
               "diff2y_pacf5",
               "diff2y_acf1", "N", "diff2y_acf5", "lumpiness", "stability")

## random walk with drift
rwd_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="rwd",]
rwd_YFH_cormat <- friedmanHstat_matrix(rwd_YFH, 25, rev(col.order))
p1 <- ggcorrplot(rwd_YFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                        high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("rwd")
## random walk
rw_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="rw",]
rw_YFH_cormat <- friedmanHstat_matrix(rw_YFH, 25, rev(col.order))
p2 <- ggcorrplot(rw_YFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("rw")
## ETS-trend
etst_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="ETS.trend",]
etst_YFH_cormat <- friedmanHstat_matrix(etst_YFH, 25, rev(col.order))
p3 <- ggcorrplot(etst_YFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                      , high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ETS-trend")
## ETS-dampedtrend
etsdt_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="ETS.dampedtrend",]
etsdt_YFH_cormat <- friedmanHstat_matrix(etsdt_YFH, 25, rev(col.order))
p4 <- ggcorrplot(etsdt_YFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ETS-dampedtrend")

## ETS-notrendnoseasonal
etsntns_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="ETS.notrendnoseasonal",]
etsntns_YFH_cormat <- friedmanHstat_matrix(etsntns_YFH, 25, rev(col.order))
p5 <- ggcorrplot(etsntns_YFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                      high = "#ef8a62", low = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ETS-notrendnoseasonal")

## ARIMA
arima_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="ARIMA",]
arima_YFH_cormat <- friedmanHstat_matrix(arima_YFH, 25, rev(col.order))
p6 <- ggcorrplot(arima_YFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                        high = "#ef8a62", low = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ARIMA")

##  ARMA.AR.MA 
arma_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="ARMA.AR.MA",]
arma_YFH_cormat <- friedmanHstat_matrix(arma_YFH, 25, rev(col.order))
p7 <- ggcorrplot(arma_YFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                        high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ARMA/AR/MA")

##  wn 
wn_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="wn",]
wn_YFH_cormat <- friedmanHstat_matrix(wn_YFH, 25, rev(col.order))
p8 <- ggcorrplot(wn_YFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                        high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("wn")

## theta
theta_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="theta",]
theta_YFH_cormat <- friedmanHstat_matrix(theta_YFH, 25, rev(col.order))
p9 <- ggcorrplot(theta_YFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                        high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("theta")

## nn
nn_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="nn",]
nn_YFH_cormat <- friedmanHstat_matrix(nn_YFH, 25, rev(col.order))
p10 <- ggcorrplot(nn_YFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                        high = "#ef8a62", low = "#f7f7f7",
                       name = "Friedman's H-statistic")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  ggtitle("nn")

p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+plot_layout(ncol = 3, nrow = 4)

## ---- yearlypca
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

pca1M4Y_rwd <- ggplot(m4yPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4yPCAresults[m4yPCAresults$predicted == "rwd", ], aes(x = PC1, y = PC2), color = "forestgreen") +
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
#                  Quarterly and Monthly data                               #
#################################################################

## ---- oobquarterlymonthly
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
  ylab("Proportion") +
  xlab("") +
  theme(legend.position = "none", legend.title = element_blank(), legend.text.align = 0, text = element_text(size = 20)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = rev(c(
    "snaive", "rwd", "rw", "ETS-notrendnoseasonal", "ETS-dampedtrend", "ETS-trend", "ETS-dampedtrendseasonal", "ETS-trendseasonal", "ETS-seasonal", "SARIMA",
    "ARIMA", "ARMA/AR/MA", "stlar", "tbats", "wn", "theta", "nn"
  ))) +
  coord_flip()+labs(subtitle = "A: Quarterly")

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
  ylab("Proportion") +
  xlab("") + 
  guides(fill=guide_legend(reverse=TRUE)) +
  scale_x_discrete(limits=rev(c("snaive","rwd", "rw", "ETS-notrendnoseasonal","ETS-dampedtrend", "ETS-trend", "ETS-dampedtrendseasonal", "ETS-trendseasonal","ETS-seasonal","SARIMA",
                                "ARIMA", "ARMA/AR/MA","stlar" ,"tbats","wn", "theta","nn"))) +
  theme(legend.position = "right", legend.title = element_blank(), legend.text.align = 0, text = element_text(size=20),  axis.text.y = element_blank()) + 
  coord_flip()+labs(subtitle = "B: Monthly")

oob_boxplot_quarterly|oob_monthly


## ---- viquarterly
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
meanrank_quarterly$rn <- 1:540
topq <- meanrank_quarterly %>%
  group_by(class) %>%
  top_n(n = 5, wt = rank)
meanrank_quarterly$istop <- ifelse(meanrank_quarterly$rn %in% topq$rn, TRUE, FALSE)
feaImp_quarterly <- ggplot(meanrank_quarterly, aes(y = rank, x = feature,fill=as.factor(istop)), width=0.1) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~class, ncol = 9, nrow = 2) +
  coord_flip() + ylab("Average rank")+ 
  scale_fill_manual(breaks=c("0","1"), values=c("black","red"), guide="none")+
  theme(text=element_text(size = 20))
feaImp_quarterly

#### ---- pdpquarterly1
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
## Monthly linearity
load("data/monthly/pdp_monthly/linearitygridM.rda")
linearitygridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/seasonalitygridM.rda")
seasonalitygridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/trendgridM.rda")
trendgridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/NgridM.rda")
NgridM$variable <- rep(1:1700, 20)

#snaive
pq1 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="snaive")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+ylab("snaive")
pq2 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="snaive")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none",text = element_text(size=20))+ylab("")
pq3 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="snaive")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("linearity")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")
pq4 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="snaive")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")
pmn1 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="snaive")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("N")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")

## rwd
pq5 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="rwd")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("rwd")
pq6 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="rwd")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("")
pq7 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="rwd")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("linearity")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("")
pq8 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="rwd")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")
pmn2 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="rwd")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("N")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")

## rw
pq9 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="rw")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("rw")
pq10 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="rw")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("")
pq11 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="rw")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("linearity")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("")
pq12 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="rw")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")
pmn3 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="rw")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("N")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")

## rw_monthly
pm7 <- ggplot(data=seasonalitygridM, aes_string(x=seasonalitygridM$seasonality, y="rw")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("seasonality")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("rw")
pm8 <- ggplot(data= trendgridM, aes_string(x=trendgridM$trend, y="rw")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("trend")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
pm9 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="rw")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("linearity")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
pmn4 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="rw")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("N")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")

## ETS.NTNS
pq13 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="ETS.notrendnoseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("ETS.NTNS")
pq14 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="ETS.notrendnoseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("")
pq15 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="ETS.notrendnoseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("linearity")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("")
pq16 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="ETS.notrendnoseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")
pmn5 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="ETS.notrendnoseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("N")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")

## ETS.DT
pq17 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="ETS.dampedtrend")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("ETS.DT")
pq18 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="ETS.dampedtrend")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("")
pq19 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="ETS.dampedtrend")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("linearity")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("")
pq20 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="ETS.dampedtrend")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")
pmn6 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="ETS.dampedtrend")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("N")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")

## ETS.T
pq21 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="ETS.trend")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("ETS.T")
pq22 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="ETS.trend")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("")
pq23 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="ETS.trend")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("linearity")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=20))+ylab("")
pq24 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="ETS.trend")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")
pmn7 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="ETS.trend")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("N")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")

#ETS-DTS
pq25 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="ETS.dampedtrendseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+
  theme(legend.position="none", text = element_text(size=20))+ylab("ETS.DTS")
pq26 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="ETS.dampedtrendseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none",text = element_text(size=20))+ylab("")
pq27 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="ETS.dampedtrendseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("linearity")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
pq28 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="ETS.dampedtrendseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")
pmn8 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="ETS.dampedtrendseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("N")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")

## ETS.TS
pq29 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="ETS.trendseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("ETS.TS")
pq30 <- ggplot(data=trendgridQ, aes_string(x=trendgridQ$trend, y="ETS.trendseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none",text = element_text(size=20))+ylab("")
pq31 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="ETS.trendseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("linearity")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
pq32 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="snaive")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("ETS.trendseasonal")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")
pmn9 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="ETS.trendseasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("N")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")

## ETS.S
pq33 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="ETS.seasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("ETS.seasonal")
pq34 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="ETS.seasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
pq35 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="ETS.seasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("linearity")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
pq36 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="ETS.seasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")
pmn10 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="ETS.seasonal")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("N")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")

## SARIMA
pq37 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="SARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("SARIMA")
pq38 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="SARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
pq39 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="SARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("linearity")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
pq40 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="SARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")
pmn11 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="SARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("N")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")
(pq1|pq2|pq4|pq3|pmn1)/(pq5|pq6|pq8|pq7|pmn2)/(pq9|pq10|pq12|pq11|pmn3)/(pm7|pm8|pq12|pm9|pmn4)/(pq13|pq14|pq16|pq15|pmn5)/(pq17|pq18|pq20|pq19|pmn6)/(pq21|pq22|pq24|pq23|pmn7)/
  (pq25|pq26|pq28|pq27|pmn8)/(pq29|pq30|pq32|pq31|pmn9)


## ---- pdpquarterly2

## ARIMA
pq41 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="ARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("ARIMA")
pq42 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="ARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
pq43 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="ARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("linearity")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
pq44 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="ARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")
pmn12 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="ARIMA")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("N")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")

## ARMA
pq45 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("ARMA.AR.MA")
pq46 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
pq47 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("linearity")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
pq48 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")
pmn13 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="ARMA.AR.MA")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("N")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")

#stlar
pq49 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="stlar")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("stlar")
pq50 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="stlar")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none",text = element_text(size=20))+ylab("")
pq51 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="stlar")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("linearity")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
pq52 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="stlar")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")
pmn14 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="stlar")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("N")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")

##tbats
pq53 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="tbats")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("tbats")
pq54 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="tbats")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
pq55 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="tbats")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("linearity")+stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(legend.position="none", text = element_text(size=20))+ylab("")
pq56 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="tbats")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")
pmn15 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="tbats")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("N")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")

## WN
pq57 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="wn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("wn")
pq58 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="wn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
pq59 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="wn")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("linearity")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
pq60 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="wn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")
pmn16 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="wn")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("N")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")

## theta
pq61 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="theta")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("theta")
pq62 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="theta")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
pq63 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="theta")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("linearity")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
pq64 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="theta")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")
pmn17 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="theta")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("N")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")

## nn
pq65 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="nn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("nn")
pq66 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="nn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+theme(legend.position="none", text = element_text(size=20))+ylab("")
pq67 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="nn")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("linearity") + 
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+
theme(legend.position="none", text = element_text(size=20)) +ylab("")
pq68 <- ggplot(data=linearitygridQ, aes_string(x=linearitygridQ$linearity, y="nn")) +
  stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")
pmn18 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="nn")) +
  stat_summary(fun.y = mean, geom = "line", col="blue", size=1)+xlab("N")+ 
  theme(legend.position="none", text = element_text(size=20))+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
  theme(legend.position = "none")+
  ylab("")

(pq33|pq34|pq36|pq35|pmn10)/(pq37|pq38|pq40|pq39|pmn11)/(pq41|pq42|pq44|pq43|pmn12)/(pq45|pq46|pq48|pq47|pmn13)/(pq49|pq50|pq52|pq51|pmn14)/(pq53|pq54|pq56|pq55|pmn15)/(pq57|pq58|pq60|pq59|pmn16)/
  (pq61|pq62|pq64|pq63|pmn17)/(pq65|pq66|pq68|pq67|pmn18)


## ---- friedmanQ
load("data/friedmanHstat_quarterly.rda")
col.order <- c("seasonality", "lumpiness","diff1y_pacf5", "trend",
               "linearity", "spikiness", "alpha", "diff1y_acf5",
               "y_pacf5","curvature", "seas_pacf", "diff2y_pacf5", "diff2y_acf5",
               "stability", "beta", "hwgamma", "nonlinearity", "hwbeta", "sediff_seacf1",
               "diff1y_acf1",
               "entropy", "hwalpha", "N", "sediff_acf1", "hurst", "y_acf1",
               "y_acf5", "sediff_acf5", "e_acf1", "diff2y_acf1")
#col.order <- rev(col.order)


## snaive
snaive_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="snaive",]
snaive_QFH_cormat <- friedmanHstat_matrix(snaive_QFH, 30, rev(col.order))
#snaive_QFH_cormat  <- snaive_QFH_cormat[,col.order]
p1 <- ggcorrplot(snaive_QFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("snaive")

## random walk with drift
rwd_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="rwd",]
rwd_QFH_cormat <- friedmanHstat_matrix(rwd_QFH, 30, rev(col.order))
#snaive_QFH_cormat  <- snaive_QFH_cormat[,col.order]
p2 <- ggcorrplot(rwd_QFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("rwd")
## random walk
rw_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="rw",]
rw_QFH_cormat <- friedmanHstat_matrix(rw_QFH, 30, rev(col.order))
p3 <- ggcorrplot(rw_QFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("rw")

## ETSNTNS
etsntns_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="ETS.notrendnoseasonal",]
etsntns_QFH_cormat <- friedmanHstat_matrix(etsntns_QFH, 30, rev(col.order))
p4 <- ggcorrplot(etsntns_QFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ETS-notrendnoseasonal")


## ETS-damped trend
etsdt_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="ETS.dampedtrend",]
etsdt_QFH_cormat <- friedmanHstat_matrix(etsdt_QFH, 30, rev(col.order))
p5 <- ggcorrplot(etsdt_QFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ETS-dampedtrend")


## ETS-trend
etst_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="ETS.trend",]
etst_QFH_cormat <- friedmanHstat_matrix(etst_QFH, 30, rev(col.order))
p6 <- ggcorrplot(etsdt_QFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ETS-trend")

## ETS-dampedtrendseasonal
etsdts_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="ETS.dampedtrendseasonal",]
etsdts_QFH_cormat <- friedmanHstat_matrix(etsdts_QFH , 30, rev(col.order))
p7 <- ggcorrplot(etsdts_QFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ETS-dampedtrendseasonal")

## ETS-trendseasonal
etsts_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="ETS.trendseasonal",]
etsts_QFH_cormat <- friedmanHstat_matrix(etsts_QFH , 30, rev(col.order))
p8 <- ggcorrplot(etsts_QFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ETS-trendseasonal")

## ETS-seasonal
etss_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="ETS.seasonal",]
etss_QFH_cormat <- friedmanHstat_matrix(etss_QFH , 30, rev(col.order))
p9 <- ggcorrplot(etss_QFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ETS-seasonal")

## SARIMA
sarima_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="SARIMA",]
sarima_QFH_cormat <- friedmanHstat_matrix(sarima_QFH , 30, rev(col.order))
p10 <- ggcorrplot(sarima_QFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("SARIMA")


## ARIMA
arima_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="ARIMA",]
arima_QFH_cormat <- friedmanHstat_matrix(arima_QFH , 30, rev(col.order))
p11 <- ggcorrplot(arima_QFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ARIMA")

## ARMA
arma_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="ARMA.AR.MA",]
arma_QFH_cormat <- friedmanHstat_matrix(arma_QFH , 30, rev(col.order))
p12 <- ggcorrplot(arma_QFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ARMA/AR/MA")

## stlar
stlar_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="stlar",]
stlar_QFH_cormat <- friedmanHstat_matrix(stlar_QFH , 30, rev(col.order))
p13 <- ggcorrplot(stlar_QFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("stlar")

## tbats
tbats_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="tbats",]
tbats_QFH_cormat <- friedmanHstat_matrix(tbats_QFH , 30, rev(col.order))
p14 <- ggcorrplot(tbats_QFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("tbats")

## wn
wn_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="wn",]
wn_QFH_cormat <- friedmanHstat_matrix(wn_QFH , 30, rev(col.order))
p15 <- ggcorrplot(wn_QFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("wn")

## theta
theta_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="theta",]
theta_QFH_cormat <- friedmanHstat_matrix(theta_QFH , 30, rev(col.order))
p16 <- ggcorrplot(theta_QFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("theta")


## nn
nn_QFH <- friedmanHstat_quarterly[friedmanHstat_quarterly$class=="nn",]
nn_QFH_cormat <- friedmanHstat_matrix(nn_QFH , 30, rev(col.order))
p17 <- ggcorrplot(nn_QFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       high = "#ef8a62", low = "#f7f7f7")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("nn")


p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+p11+p12+p13+p14+p15+p16+p17+plot_layout(ncol = 4, nrow = 5)

## ---- quarterlypca
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


pca1M4Q_snaive+pca1M4Q_rwd+pca1M4Q_rw+pca1M4Q_notrend+pca1M4Q_etsdamtrend+
  pca1M4Q_etstrend+pca1M4Q_etsdtrends+pca1M4Q_trends+pca1M4Q_s+pca1M4Q_sarima+
  pca1M4Q_ARIMA+pca1M4Q_ARMA+pca1M4Q_stlar+pca1M4Q_tbats+pca1M4Q_wn+
  pca1M4Q_theta+pca1M4Q_nn+plot_layout(ncol = 5, nrow = 4)

################################################################################
#                      Monthly  data                                           #
################################################################################

## ---- vimonthly
# monthly feature importance
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
meanrank_monthly$rn <- 1:540
topq <- meanrank_monthly %>%
  group_by(class) %>%
  top_n(n = 5, wt = rank)
meanrank_monthly$istop <- ifelse(meanrank_monthly$rn %in% topq$rn, TRUE, FALSE)
feaImp_monthly <- ggplot(meanrank_monthly, aes(y = rank, x = feature,fill=as.factor(istop))) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~class, ncol = 9, nrow = 2) +
  coord_flip() + ylab("Average rank")+ 
  scale_fill_manual(breaks=c("0","1"), values=c("black","red"), guide="none")+
  theme(text=element_text(size = 20))
feaImp_monthly

## ---- friedmanM
load("data/friedmanHstat_monthly.rda")
col.order <- c("seasonality", "linearity","lumpiness", "trend",
               "spikiness", "stability", "y_pacf5", "hwgamma", "hwalpha",
               "sediff_acf5", "sediff_seacf1", "seas_pacf",
               "curvature", "beta", "N", "nonlinearity", "e_acf1", "hurst", "entropy",
               "hwbeta", "sediff_acf1", "y_acf5", "diff1y_pacf5", "alpha",
               "y_acf1", "diff1y_acf5", "diff1y_acf1", "diff2y_acf5",
               "diff2y_acf1", "diff2y_pacf5"
               )

## snaive
snaive_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="snaive",]
snaive_MFH_cormat <- friedmanHstat_matrix(snaive_MFH, 30, rev(col.order))
p1 <- ggcorrplot(snaive_MFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("snaive")

## random walk with drift
rwd_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="rwd",]
rwd_MFH_cormat <- friedmanHstat_matrix(rwd_MFH, 30, rev(col.order))
p2 <- ggcorrplot(rwd_MFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("rwd")
## random walk
rw_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="rw",]
rw_MFH_cormat <- friedmanHstat_matrix(rw_MFH, 30, rev(col.order))
p3 <- ggcorrplot(rw_MFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("rw")

## ETSNTNS
etsntns_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="ETS.notrendnoseasonal",]
etsntns_MFH_cormat <- friedmanHstat_matrix(etsntns_QFH, 30, rev(col.order))
p4 <- ggcorrplot(etsntns_MFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ETS-notrendnoseasonal")


## ETS-damped trend
etsdt_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="ETS.dampedtrend",]
etsdt_MFH_cormat <- friedmanHstat_matrix(etsdt_MFH, 30, rev(col.order))
p5 <- ggcorrplot(etsdt_MFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ETS-dampedtrend")


## ETS-trend
etst_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="ETS.trend",]
etst_MFH_cormat <- friedmanHstat_matrix(etst_MFH, 30, rev(col.order))
p6 <- ggcorrplot(etsdt_MFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ETS-trend")

## ETS-dampedtrendseasonal
etsdts_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="ETS.dampedtrendseasonal",]
etsdts_MFH_cormat <- friedmanHstat_matrix(etsdts_MFH , 30, rev(col.order))
p7 <- ggcorrplot(etsdts_MFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ETS-dampedtrendseasonal")

## ETS-trendseasonal
etsts_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="ETS.trendseasonal",]
etsts_MFH_cormat <- friedmanHstat_matrix(etsts_MFH , 30, rev(col.order))
p8 <- ggcorrplot(etsts_MFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ETS-trendseasonal")

## ETS-seasonal
etss_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="ETS.seasonal",]
etss_MFH_cormat <- friedmanHstat_matrix(etss_MFH , 30, rev(col.order))
p9 <- ggcorrplot(etss_MFH_cormat, hc.order = FALSE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ETS-seasonal")

## SARIMA
sarima_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="SARIMA",]
sarima_MFH_cormat <- friedmanHstat_matrix(sarima_MFH , 30, rev(col.order))
p10 <- ggcorrplot(sarima_MFH_cormat, hc.order = FALSE, type = "upper",
                  outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("SARIMA")


## ARIMA
arima_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="ARIMA",]
arima_MFH_cormat <- friedmanHstat_matrix(arima_MFH , 30, rev(col.order))
p11 <- ggcorrplot(arima_MFH_cormat, hc.order = FALSE, type = "upper",
                  outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ARIMA")

## ARMA
arma_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="ARMA.AR.MA",]
arma_MFH_cormat <- friedmanHstat_matrix(arma_MFH , 30, rev(col.order))
p12 <- ggcorrplot(arma_MFH_cormat, hc.order = FALSE, type = "upper",
                  outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("ARMA/AR/MA")

## stlar
stlar_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="stlar",]
stlar_MFH_cormat <- friedmanHstat_matrix(stlar_MFH , 30, rev(col.order))
p13 <- ggcorrplot(stlar_MFH_cormat, hc.order = FALSE, type = "upper",
                  outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("stlar")

## tbats
tbats_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="tbats",]
tbats_MFH_cormat <- friedmanHstat_matrix(tbats_MFH , 30, rev(col.order))
p14 <- ggcorrplot(tbats_MFH_cormat, hc.order = FALSE, type = "upper",
                  outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("tbats")

## wn
wn_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="wn",]
wn_MFH_cormat <- friedmanHstat_matrix(wn_MFH , 30, rev(col.order))
p15 <- ggcorrplot(wn_MFH_cormat, hc.order = FALSE, type = "upper",
                  outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("wn")

## theta
theta_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="theta",]
theta_MFH_cormat <- friedmanHstat_matrix(theta_MFH , 30, rev(col.order))
p16 <- ggcorrplot(theta_MFH_cormat, hc.order = FALSE, type = "upper",
                  outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("theta")


## nn
nn_MFH <- friedmanHstat_monthly[friedmanHstat_monthly$class=="nn",]
nn_MFH_cormat <- friedmanHstat_matrix(nn_MFH , 30, rev(col.order))
p17 <- ggcorrplot(nn_MFH_cormat, hc.order = FALSE, type = "upper",
                  outline.col = "white")+
  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  guides(fill=FALSE, color=FALSE)+ggtitle("nn")


p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+p11+p12+p13+p14+p15+p16+p17+plot_layout(ncol = 4, nrow = 5)


## ---- monthlypca
pcaMvariables <- monthly_training[, 1:30]
pcaM4M <- prcomp(pcaMvariables, center = TRUE, scale = TRUE)
PC1m4m <- pcaM4M$x[, 1]
PC2m4m <- pcaM4M$x[, 2]
PC3m4m <- pcaM4M$x[, 3]
#which.max(PC2m4m)249937
m4mPCAresults <- data.frame(PC1 = PC1m4m, PC2 = PC2m4m, PC3 = PC3m4m, pcaMvariables)
m4mPCAresults$predicted <- trainM_predictions_oob
m4mPCAresults <- m4mPCAresults[-249937, ]
pca1M4Msnaive <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "snaive", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "rwd") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Mrwd <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "rwd", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "rwd") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Mrw <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "rw", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "rw") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))


pca1M4Mnotrend <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ETS-notrendnoseasonal", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-notrendnoseasonal") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Metsdamtrend <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ETS-dampedtrend", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-dampedtrend") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Metstrend <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ETS-trend", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-trend") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Metsdtrends <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ETS-dampedtrendseasonal", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-DTS") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Mtrends <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ETS-trendseasonal", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-trendseasonal") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Ms <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ETS-seasonal", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ETS-seasonal") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Msarima <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "SARIMA", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "SARIMA") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4MARIMA <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ARIMA", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ARIMA") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4MARMA <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "ARMA/AR/MA", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "ARMA/AR/MA") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Mstlar <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "stlar", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "stlar") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Mtbats <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "tbats", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "stlar") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Mwn <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "wn", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "wn") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Mtheta <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "theta", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "theta") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

pca1M4Mnn <- ggplot(m4mPCAresults, aes(x = PC1, y = PC2, color = predicted)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data = m4mPCAresults[m4mPCAresults$predicted == "nn", ], aes(x = PC1, y = PC2), color = "forestgreen") +
  labs(subtitle = "nn") + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))


pca1M4Msnaive+pca1M4Mrwd+pca1M4Mrw+pca1M4Mnotrend+pca1M4Metsdamtrend+
  pca1M4Metstrend+pca1M4Metsdtrends+pca1M4Mtrends+pca1M4Ms+pca1M4Msarima+
  pca1M4MARIMA+pca1M4MARMA+pca1M4Mstlar+pca1M4Mtbats+pca1M4Mwn+
  pca1M4Mtheta+pca1M4Mnn+plot_layout(ncol = 5, nrow = 4)


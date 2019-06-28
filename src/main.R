## ---- loadPackages
library(Mcomp)
library(patchwork)
library(tidyverse)
library(reshape2)
library(grid)
library(gridExtra)
library(ggrepel)
library(png)
library(tsfeatures)
library(ggpubr)
library(RColorBrewer)
library(iml) # machine learning interpretability package
library(ggcorrplot) # to draw  ggcorrplot
library(lime)
library(seer)
library(Hmisc) # to draw confidence intervals


## ---- wntwopdcplots
## yearly
load("data/yearly/lmres_acf1.diff1y_acf1.y.rda")
colNames <- colnames(lmres_acf1.diff1y_acf1.y)[27:36]
wny <- ggplot(
  data = lmres_acf1.diff1y_acf1.y,aes_string(x = lmres_acf1.diff1y_acf1.y$lmres_acf1,y = lmres_acf1.diff1y_acf1.y$diff1y_acf1, fill = colNames[10]
   ))+geom_raster() + 
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_viridis_c(option = "A", direction = -1, breaks=c(0,0.8,100),
                       limits=c(0,0.8))+
  theme(strip.text.x = element_text(size = 10))+
   xlab("lmres_acf1") + ylab("diff1y_acf1") +ggtitle("yearly: wn")

## quarterly
load("data/quarterly/e_acf1.curvature.q.rda")
colNames <- colnames(e_acf1.curvature.q)[32:48]
wnq <- ggplot(
  data = e_acf1.curvature.q,aes_string(x = e_acf1.curvature.q$e_acf1,y =e_acf1.curvature.q$curvature, fill = colNames[17]
  ))+geom_raster() + 
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_viridis_c(option = "A", direction = -1, breaks=c(0,0.8,100),
                       limits=c(0,0.8))+
  theme(strip.text.x = element_text(size = 10))+
  xlab("e_acf1") + ylab("curvature") +ggtitle("quarterly: wn")

## monthly
load("data/monthly/sediff_seacf1.hwalpha.m.rda")
colNames <-colnames(sediff_seacf1.hwalpha.m)[32:48]
wnm <- ggplot(
  data = sediff_seacf1.hwalpha.m,aes_string(x = sediff_seacf1.hwalpha.m$sediff_seacf1,y =sediff_seacf1.hwalpha.m$hwalpha, fill = colNames[17]
  ))+geom_raster() + 
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_viridis_c(option = "A", direction = -1, breaks=c(0,0.8,100),
                       limits=c(0,0.8))+
  theme(strip.text.x = element_text(size = 10))+
  xlab("sediff_seacf1") + ylab("hwalpha") +ggtitle("monthly: wn")


## weekly
load("data/weekly/curvature.linearity.w.rda")
colNames <- colnames(curvature.linearity.w)[29:40]
wnw <- ggplot(
  data = curvature.linearity.w, aes_string(x = curvature.linearity.w$curvature,y =curvature.linearity.w$linearity, fill = colNames[12]
  ))+geom_raster() + 
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_viridis_c(option = "A", direction = -1, breaks=c(0,0.8,100),
                       limits=c(0,0.8))+
  theme(strip.text.x = element_text(size = 10))+
  xlab("curvature") + ylab("linearity") +ggtitle("weekly: wn")

## daily
load("data/daily/stability.seasonal_strength2.d.rda")
colNames <- colnames(stability.seasonal_strength2.d)[28:37]
wnd <- ggplot(
  data = stability.seasonal_strength2.d, aes_string(x = stability.seasonal_strength2.d$stability,y =stability.seasonal_strength2.d$seasonal_strength2, fill = colNames[10]
  ))+geom_raster() + 
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_viridis_c(option = "A", direction = -1, breaks=c(0,0.8,100),
                       limits=c(0,0.8))+
  theme(strip.text.x = element_text(size = 10))+
  xlab("stability") + ylab("seasonal_w") +ggtitle("daily: wn")

#hourly
load("data/hourly/linearity.sediff_seacf1.h.rda")
colNames <- colnames(linearity.sediff_seacf1.h)[28:37]
wnh <- ggplot(
  data = linearity.sediff_seacf1.h, aes_string(x = linearity.sediff_seacf1.h$linearity, y =linearity.sediff_seacf1.h$sediff_seacf1, fill = colNames[10]
  ))+geom_raster() + 
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_viridis_c(option = "A", direction = -1, breaks=c(0,0.8,100),
                       limits=c(0,0.8))+
  theme(strip.text.x = element_text(size = 10))+
  xlab("linearity") + ylab("sediff_seacf1") +ggtitle("hourly: wn")

(wny|wnq|wnm)/(wnw|wnd|wnh)






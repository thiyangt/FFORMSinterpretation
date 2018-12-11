load("C:/phd_MLinterpretability/data/yearly/yearly_training.rda")
library(tidyverse)
a <- yearly_training[c(393474,480357),] %>% data.frame()
load("C:/phd_MLinterpretability/data/yearly/m4yse_df_training.rda")
combined <- rbind(a,m4yse_df_training)
duplicate_rows <- unique(combined[duplicated(combined), ])
duplicate_rows 
# 140651, 227534
m4yse_df_training[227534, ] %>% data.frame()

limeyts <- list(M4YSimARIMA[[20492]][10], M4YSimETS[[14066]][1], M4YSimETS[[22755]][4])
save(limeyts, file="limeyts.rda")

#yearly
subset_yearly <- yearly_training %>% group_by(classlabels) %>% sample_n(size=100)
save(subset_yearly, file="subset_yearly.rda")

#quarterly

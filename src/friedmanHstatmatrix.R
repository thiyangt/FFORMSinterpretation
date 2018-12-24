# dataframe: rwd_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="rwd",]

friedmanHstat_matrix <- function(dataframe_FH, number_of_features){
  YFH_cor <- dataframe_FH %>% select(c("feature1", "feature2", "interaction"))
  names(YFH_cor) <- c("Var1", "Var2", "value")
  df1 <- data.frame(Var1=names(table(YFH_cor$Var1)),
                    Var2=names(table(YFH_cor$Var1)),
                    value=rep(1.00, number_of_features))
  
  cormat <- dplyr::bind_rows(YFH_cor, df1)
  cormat <- dcast(cormat, Var1 ~ Var2, value.var="value")
  #colnames(cormat)[1] <- ""
  cor <- cormat[, -1]
  
  ## take the mean of upper and lower triangle values
  meancor <- matrix(NA, ncol = ncol(cor), nrow = nrow(cor))
  meancor[upper.tri(meancor)] <- rowMeans(cbind(cor[upper.tri(cor, diag = FALSE)], 
                                      cor[lower.tri(cor, diag = FALSE)]))
  diag(meancor) <- rep(1, number_of_features)
  meancor[lower.tri(meancor)] <- t(meancor)[lower.tri(meancor)]
  rownames(meancor) <- colnames(cormat)[-1]
  colnames(meancor) <- colnames(cormat)[-1]
  return(meancor)
  
}

## usage
rwd_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="rwd",]
rwd_YFH_cormat <- friedmanHstat_matrix(rwd_YFH, 25)
cormat1 <- reorder_cormat(rwd_YFH_cormat)
p1 <- ggcorrplot(cormat1, hc.order = TRUE, type = "upper",
                 outline.col = "white")+
  scale_fill_gradient2(limits=c(0.5, 1), breaks=seq(0.5,1,100), 
                      low="blue", mid= "#fee8c8", high = "#e34a33",  
                       name="", na.value = "transparent")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+guides(fill=FALSE, color=FALSE)+ggtitle("rwd")

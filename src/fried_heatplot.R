fried_heatplot <- function(classlabel, nfeatures, ){
  
  snaive_DFH <- friedmanHstat_daily[friedmanHstat_daily$class=="snaive",]
  snaive_DFH_cor <- snaive_DFH %>% select(c("feature1", "feature2", "interaction"))
  names(snaive_DFH_cor) <- c("Var1", "Var2", "value")
  df1 <- data.frame(Var1=names(table(snaive_DFH_cor$Var1)),
                    Var2=names(table(snaive_DFH_cor$Var1)),
                    value=rep(1.00, 26))
  
  cormat <- dplyr::bind_rows(snaive_DFH_cor, df1)
  cormat <- dcast(cormat, Var1 ~ Var2, value.var="value")
  colnames(cormat)[1] <- ""
  cormat <- data.matrix(cormat)
  cormat <- cormat[,-1]
  rownames(cormat) <- colnames(cormat)
  cormat <- round(cormat,2)
  cormat1 <- reorder_cormat(cormat)
  p1 <- ggcorrplot(cormat1, hc.order = TRUE, type = "upper",
                   outline.col = "white")+
    scale_fill_gradient2(limits=c(0.5, 1), breaks=seq(0.5,1,100), 
                         low = "#fee8c8", high = "#e34a33",  
                         name="", na.value = "white")+
    theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                     size = 12, hjust = 1))+guides(fill=FALSE, color=FALSE)+ggtitle("snaive")
  
  
  
  
  
  
}
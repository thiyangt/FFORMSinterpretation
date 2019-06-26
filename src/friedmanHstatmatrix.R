# Function to arrange Friedman's H statistic values in a matrix

friedmanHstat_matrix <- function(dataframe_FH, number_of_features, order){
  
  FH_cor <- dataframe_FH %>% select(c("feature1", "feature2", "interaction"))
  FH_cor$feature1  <- factor(FH_cor$feature1 ,
                     levels = order)
  FH_cor$feature2  <- factor(FH_cor$feature2 ,
                             levels = order)
  
  names(FH_cor) <- c("Var1", "Var2", "value")
  df1 <- data.frame(Var1=names(table(FH_cor$Var1)),
                    Var2=names(table(FH_cor$Var1)),
                    value=rep(1.00, number_of_features))

  cormat <- dplyr::bind_rows(FH_cor, df1)
  cormat <- reshape2::dcast(cormat, Var1 ~ Var2, value.var="value")
  #colnames(cormat)[1] <- ""
  cor <- cormat[, -1]
  cor <- as.matrix(cor)
  tcor <- t(cor)
  
  ## take the mean of upper and lower triangle values
  meancor1 <- matrix(NA, ncol = ncol(cor), nrow = nrow(cor))
  meancor1[lower.tri(meancor1)] <- rowMeans(cbind(tcor[lower.tri(tcor, diag = FALSE)], 
                                      cor[lower.tri(cor, diag = FALSE)]))
  diag(meancor1) <- rep(1, number_of_features)
  meancor <- t(meancor1)
 

  meancor[lower.tri(meancor)] <- meancor1[lower.tri(meancor1)] 
  rownames(meancor) <- order
  colnames(meancor) <- colnames(cormat)[-1]
  return(meancor)
  
}

## usage
# load("data/friedmanHstat_yearly.rda")
# rwd_YFH <- friedmanHstat_yearly[friedmanHstat_yearly$class=="wn",]
# rwd_YFH[rwd_YFH$feature1=="trend",]
# rwd_YFH[rwd_YFH$feature2=="trend",]
#order <- c("trend", "ur_pp","spikiness", "beta",
#          "diff1y_acf1", "linearity", "diff1y_acf5", "curvature",
#          "lmres_acf1","y_pacf5", "ur_kpss", "y_acf1", "nonlinearity",
#         "alpha", "diff1y_pacf5", "hurst", "entropy", "e_acf1", "y_acf5",
#          "diff2y_pacf5",
#          "diff2y_acf1", "N", "diff2y_acf5", "lumpiness", "stability")
#number_of_features = 25
#rwd_YFH_cormat <- friedmanHstat_matrix(rwd_YFH, 25,rev(order))
#p1 <- ggcorrplot(rwd_YFH_cormat, hc.order = FALSE, type = "upper",
#                 outline.col = "white")+
#  scale_fill_gradient2(limits=c(0, 1), breaks=seq(0,1,100), 
#                       low = "#67a9cf", high = "#ef8a62", mid = "#f7f7f7", midpoint = 0.5)+
#  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
#                                   size = 12, hjust = 1))+guides(fill=FALSE, color=FALSE)+ggtitle("rwd")
#p1

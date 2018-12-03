library(reshape2)
ARIMA_YFH_cor <- ARIMA_YFH %>% select(c("feature1", "feature2", "interaction"))
names(ARIMA_YFH_cor) <- c("Var1", "Var2", "value")

df1 <- data.frame(Var1=names(table(ARIMA_YFH_cor$Var1)),
                  Var2=names(table(ARIMA_YFH_cor$Var1)),
                  value=rep(1.00, 25))

cormat <- dplyr::bind_rows(ARIMA_YFH_cor, df1)
cormat <- dcast(cormat, Var1 ~ Var2, value.var="value")
colnames(cormat)[1] <- ""
class(cormat)
cormat <- data.matrix(cormat)
dim(cormat)
colnames(cormat) 
cormat <- cormat[,-1]
dim(cormat)
rownames(cormat) <- colnames(cormat)
dim(cormat)
cormat <- round(cormat,2)
dim(cormat)


# Extra functions

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat3 <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat2)

# Melt the correlation matrix
library(reshape2)
library(digest)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
head(melted_cormat)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.5, limit = c(0,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
ggheatmap


###############
install.packages("ggcorrplot")
library(reshape2)
ARIMA_YFH_cor <- ARIMA_YFH %>% select(c("feature1", "feature2", "interaction"))
names(ARIMA_YFH_cor) <- c("Var1", "Var2", "value")

df1 <- data.frame(Var1=names(table(ARIMA_YFH_cor$Var1)),
                  Var2=names(table(ARIMA_YFH_cor$Var1)),
                  value=rep(1.00, 25))

cormat <- dplyr::bind_rows(ARIMA_YFH_cor, df1)
cormat <- dcast(cormat, Var1 ~ Var2, value.var="value")
colnames(cormat)[1] <- ""
class(cormat)
cormat <- data.matrix(cormat)
dim(cormat)
colnames(cormat) 
cormat <- cormat[,-1]
dim(cormat)
rownames(cormat) <- colnames(cormat)
dim(cormat)
cormat <- round(cormat,2)
dim(cormat)
cormat3 <- reorder_cormat(cormat)
library(ggcorrplot)
ggcorrplot(cormat3, hc.order = TRUE, type = "upper",
           outline.col = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.5, limit = c(0,1), space = "Lab", 
                       name="Pearson\nCorrelation")

save(cormat2, file="cormat2.rda")

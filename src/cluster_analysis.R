## read data
weekly_train_votes <- load("data/weekly/trainW_votes.rda")
train_votes_df <- as.data.frame(trainW_votes)
head(train_votes_df)


## hclust
d <- dist(train_votes_df, method = "euclidean")
hc1 <- hclust(d, method = "complete" )
clusterCut <- cutree(hc1, 5)
train_votes_df$cluster <- as.vector(clusterCut)
head(train_votes_df)
table(train_votes_df$cluster)


## my radar plot
library(tidyverse)
library(magrittr)
df_radar <- train_votes_df %>%
  group_by(cluster) %>%
  summarise_at(vars(ARIMA:wn), funs(median(., na.rm=TRUE)))

df_radar_gather <- gather(df_radar, key = "model", value = "measurement",
      ARIMA:wn)
head(df_radar_gather)
table(df_radar_gather$cluster)

# plot
library(ggplot2)
ggplot(data=df_radar_gather,  aes(x=model, 
                                  y=measurement, group= cluster, 
                                  colour=cluster, fill=cluster)) + 
  geom_point(size=2) + 
  geom_polygon(size = 1, alpha= 0.2) + 
   ggtitle("Radar")  + 
  scale_x_discrete() +
  theme_light()+
  coord_polar()

## ggplot
ggplot(df_radar_gather,
       aes(x = model,
           y = measurement,
           color = cluster,
           group = cluster)) +
  geom_polygon(fill = NA) + 
  coord_polar() +
  facet_wrap(~cluster)

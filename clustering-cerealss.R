library(tidyverse)
library(ggplot2)

cereal <- read.csv("~/clustering-cereals/cereal.csv")

#this is my first non-tutorial-ed codings so let's start from peeking
str(cereal)
head(cereal)


#giving the factor-type column numbers
cereal_matrix <- data.matrix(cereal[c("mfr", "type")])
#combine them with the original dataframe
cereal <- data.frame(cereal, cereal_matrix)
str(cereal)
#to see which number is assigned to which factor
mfr_cat <- unique(cereal[c("mfr", "mfr.1")])
mfr_cat
type_cat <- unique(cereal[c("type", "type.1")])
type_cat
head(cereal)


#choosing which field to be used for clustering
field_chosen <- c("mfr.1", "type.1", "rating")

#starting kmeans
set.seed(100)
sse <- sapply(1:10,
              function(param_k)
              {
                kmeans(cereal[field_chosen], param_k, nstart=25)$tot.withinss
              }
)
sse


#visualize to find out how many clustering should be used
cluster_max <- 10
ssdata = data.frame(cluster=c(1:cluster_max),sse)
ggplot(ssdata, aes(x=cluster,y=sse)) +
  geom_line(color="red") + geom_point() +
  ylab("Within Cluster Sum of Squares") + xlab("Cluster Count") +
  geom_text(aes(label=format(round(sse, 2), nsmall = 2)),hjust=-0.2, vjust=-0.5) +
  scale_x_discrete(limits=c(1:cluster_max))


#try 4 clustering
segment_four <- kmeans(x=cereal[field_chosen], centers=4, nstart=25)
segment_four
#and 5 clustering
segment_five <- kmeans(x=cereal[field_chosen], centers=5, nstart=25)
segment_five

#since the ratio between_SS/total_SS has a bigger score with centroid  = 5, we will use the 5-k clustering
#look which cereal in 1-5 cluster
cereal$cluster <- segment_five$cluster
cereal[which(cereal$cluster == 1),]
cereal[which(cereal$cluster == 2),]
cereal[which(cereal$cluster == 3),]
cereal[which(cereal$cluster == 4),]
cereal[which(cereal$cluster == 5),]


#from the last cluster we can see that most of the data has higher ratings than the rest of the clusters.
#we see that the cluster is dominated by cold cereals, and manufactured by Nabisco.
#Read CSV File
playersStats <- read.csv("clusteringInputForR.csv")

#Scree Plot
mydata <- playersStats[,c(2:5)]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Poèet zhlukov",
     ylab="SSE")

clustersCount = 4


playersStats.use = playersStats[,c(2:5)]

#distance matrix 
playersStats.dist <- dist(playersStats.use, method = "euclidean")
#cluster tree
playersStats.hclust <- hclust(playersStats.dist, method = "ward.D")
#draw dendogram
plot(playersStats.hclust)
#split players into 4 groups
groups.4 = cutree(playersStats.hclust, clustersCount)
#binding names
groups.names = data.frame(playersStats$NAME, groups.4)
#getting number of members of each cluster
table(groups.4)
# draw dendogram with red borders around the (clustersCount) clusters 
rect.hclust(playersStats.hclust, k=clustersCount, border="red")


groups.names.stats = data.frame(playersStats, groups.4)
#split by groups
groups.names.stats.split = split(groups.names.stats, groups.names$groups.4)

#group1.stats = groups.names.stats.split$`1`
group1.stats.colMeans = colMeans(groups.names.stats.split$`1`[,c(2:5)])
#group1.stats = groups.names.stats.split$`2`
group2.stats.colMeans = colMeans(groups.names.stats.split$`2`[,c(2:5)])
#group1.stats = groups.names.stats.split$`3`
group3.stats.colMeans = colMeans(groups.names.stats.split$`3`[,c(2:5)])
#group1.stats = groups.names.stats.split$`4`
group4.stats.colMeans = colMeans(groups.names.stats.split$`4`[,c(2:5)])


write.csv(groups.names.stats.split$`1`[,c(1)], file = "C:/PokerPredictor/ClusteringGroups/middleTightPassive.csv",row.names=FALSE)
write.csv(groups.names.stats.split$`2`[,c(1)], file = "C:/PokerPredictor/ClusteringGroups/looseAggresive.csv",row.names=FALSE)
write.csv(groups.names.stats.split$`3`[,c(1)], file = "C:/PokerPredictor/ClusteringGroups/tightAggressive.csv",row.names=FALSE)
write.csv(groups.names.stats.split$`4`[,c(1)], file = "C:/PokerPredictor/ClusteringGroups/middleTightAggressive.csv",row.names=FALSE)

#table(groups.names.stats[,c(6)])

#Kmeans
kc <- kmeans(groups.names.stats[,c(2:5)], clustersCount)
plot(groups.names.stats[,c(2:5)], col = kc$cluster, pch = 19)
text(groups.names.stats, labels=kc$cluster)

#Silhuette
library(cluster)

sil <- silhouette(cutree(playersStats.hclust, clustersCount), playersStats.dist)
plot(sil, main ="Silhouette plot")







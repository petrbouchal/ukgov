pols <- read.csv('./data-input/Policies.csv')
pols_m <- pols[c(1:2,20:23)]

pols_m$complete <- complete.cases(pols_m)

pols_c <- subset(pols_m, complete==TRUE)

pols_c$fullarea  <- paste(pols_c$Department, pols_c$Policy.spend.area, sep=': ')

pols_c$complete <- NULL
pols_c$Department <- NULL
pols_c$Policy.spend.area <- NULL

row.names(pols_c) <- pols_c$fullarea
pols_c$fullarea <- NULL
hc=hclust(dist(pols_c))

par(mar=c(0,0,0,20))
plot(as.dendrogram(hc),horiz=T, cex=.1)

dist <- dist(pols_c)

library(cluster)
library(fpc)
clust <- kmeans(pols_c, 4)
clusplot(pols_c, clust$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

library(mclust)
fit <- Mclust(pols_c)
plot(fit, pols_c) # plot results 
print(fit) # display the best model
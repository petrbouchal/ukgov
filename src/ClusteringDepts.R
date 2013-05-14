pols6 <- read.csv('./data-input/Outputs_roughscores_6cols.csv')
pols5 <- read.csv('./data-input/Outputs_roughscores_5cols.csv')
pols3 <- read.csv('./data-input/Outputs_roughscores_3cols.csv')

row.names(pols6) <- pols6$Dept
row.names(pols5) <- pols5$Dept
row.names(pols3) <- pols3$Dept

hc6=hclust(dist(pols6))
hc5=hclust(dist(pols5))
hc3=hclust(dist(pols3))

par(mar=c(2,1,1,3))
plot(as.dendrogram(hc6),horiz=T,type='rectangle')
plot(as.dendrogram(hc5),horiz=T,type='rectangle')
plot(as.dendrogram(hc3),horiz=T,type='rectangle')
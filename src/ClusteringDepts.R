pols5 <- read.csv('./data-input/Outputs_roughscores_6cols.csv')

row.names(pols5) <- pols5$Dept

hc5=hclust(dist(pols5))

par(mar=c(2,1,1,3))
plot(as.dendrogram(hc5),horiz=T,type='rectangle')
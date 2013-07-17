library(ggplot2)
library(ggthemes)

source('./src/acses_lib.R')

basecols <- c('#37424a','#00ccff','#d40072','#83389b',
                 '#7a9393','#457e81','#be8b5e')

xtints <- tintshade(basecols,kind='tints',steps=c(.5,.25),hexin=T)
data <- data.frame('x'=sort(rep(1:7,3)),'y'=rep(1:3,7))
data$col <- ''
data$col[data$y==1] <- xtints[,1]
data$col[data$y==2] <- xtints[,2]
data$col[data$y==3] <- xtints[,3]

data$xx <- paste0(data$x,data$y)

data$r <- col2rgb(data$col)[1,]
data$g <- col2rgb(data$col)[2,]
data$b <- col2rgb(data$col)[3,]
data$rgb <- paste('r:',data$r,'\ng:',data$g,'\nb:',data$b, sep='')

blah <- ggplot(data,aes(x,y))+
  geom_tile(aes(fill=as.factor(xx)),show_guide=F)+
  scale_fill_manual(values=data2$col) +theme_few() +
  geom_text(aes(x,y-.25,label=rgb))+
  geom_text(aes(x,y+.25,label=col))
blah
  
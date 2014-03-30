source('./src/lib/lib_acses.R')

basecols <- c('#37424a','#00ccff','#d40072','#83389b','#7a9393','#457e81','#be8b5e')

tints <- c(.75,.5,.25)
shades=c()
xtints <- TintShade(basecols,tints=tints,hexin=T,)
data <- data.frame('x'=sort(rep(1:dim(xtints)[1],dim(xtints)[2])),
                   'y'=rep(1:dim(xtints)[2],dim(xtints)[1]))
data$col <- ''

for(i in 1:dim(xtints)[2]) {
  data$col[data$y==i] <- xtints[,i]
}

data$xx <- paste0(data$x,data$y)

data$r <- col2rgb(data$col)[1,]
data$g <- col2rgb(data$col)[2,]
data$b <- col2rgb(data$col)[3,]
data$rgb <- paste('r:',data$r,'\ng:',data$g,'\nb:',data$b, sep='')

palettesheet <- ggplot(data,aes(x,y))+
  geom_tile(aes(fill=as.factor(xx)),show_guide=F)+
  scale_fill_manual(values=data$col) +theme_few() +
  geom_text(aes(x,y-.25,label=rgb))+
  scale_y_continuous(labels=c('full',paste0(tints*100,'%')),breaks=(1:4))+
  scale_x_continuous(breaks=NULL)+
  geom_text(aes(x,y+.25,label=col)) +
  theme(panel.border=element_blank(),axis.ticks=element_blank(),axis.title=element_blank())
palettesheet

ggsave('./palettes-output/IfG_palette.pdf',width=30,height=21,units='cm')
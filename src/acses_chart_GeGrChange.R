source('./src/acses_lib.R')

uu <- read.csv('./data-input/GenderGrade.csv')
uu$Variable <- factor(uu$Variable,levels(uu$Variable)[c(2,1)])
uu$Grade <- factor(uu$Grade,levels(uu$Grade)[c(5,4,6,3,1,2)])
uu$grp <- as.factor(paste0(uu$Gender,' ',uu$Variable))

plotname <- 'plot_GeGrChange'
plottitle <- 'Gender balance in Civil Service Grades and change 2010-12'
ylabel <- ''
xlabel <- ''
ph <- 24.5/2.5
pw <- 15.3
plotformat='wmf'
uu$ylabels <- paste0(signif(uu$value*100,2),'%')

plot_GeGrChange <- ggplot(uu,aes(y=value,x=Variable)) +
  geom_bar(position='stack',stat='identity', aes(fill=grp))+
  geom_text(aes(x=Variable,y=rep(c(.2,.8,.2,.8),6),label=ylabels,
                col=rep(c('white','white',IfGcols[1,1],IfGcols[1,1]),6)),size=2,
            family=fontfamily,vjust=.5)+
  coord_flip()+
  facet_wrap(~Grade,ncol=1)+
  scale_y_continuous(labels=percent,expand=c(0,0))+
  scale_colour_manual(values=c(IfGcols[1,1],'white'),guide=FALSE)+
  scale_fill_manual(values=c(IfGcols[2,1],IfGcols[2,2],IfGcols[5,1],IfGcols[5,2]))+
  theme(plot.margin=unit(c(0.5,1,0,0),'cm'),
        panel.margin=unit(c(0,0,0,0),'cm'))+
  labs(y=ylabel,x=xlabel,title=plottitle)
plot_GeGrChange

SavePlot(plotname=plotname,ffamily=fontfamily,plotformat=plotformat,ploth=ph,plotw=pw)
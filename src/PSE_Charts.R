# Setup -------------------------------------------------------------------
source('./src/acses_lib.R')
source('./src/PSE_Reshape.R') # to prepare data if needed
PSE <- change

# Plain chart in ggplot2 --------------------------------------------------

PSE$chart <- TRUE
PSE$chart[PSE$Dept == 'Total']  <- FALSE
PSE$chart[PSE$Dept == 'AGO']  <- FALSE
PSE$chart[PSE$Dept == 'NIO']  <- FALSE
PSE$chart[PSE$Dept == 'GEO']  <- FALSE
PSE$chart[PSE$Dept == 'Welsh Gov']  <- FALSE
PSE$chart[PSE$Dept == 'Scot Gov']  <- FALSE
PSE$chart[PSE$Dept == 'FCO' & PSE$Whitehall!='Total']  <- FALSE

# change order of facets
totals <- PSE[PSE$measure=='Cumulative_Perc_net_change' & PSE$Whitehall=='Total' & PSE$Period=='2013Q1',
                  c(1,3)]
totals$sorter <- totals$value
totals$value <- NULL
PSE <- merge(PSE, totals,all.x=TRUE)
#PSE$sorter[PSE$Dept =='Total excl. WH FCO'] <- max(PSE$sorter)*1.1
PSE$Dept <- reorder(PSE$Dept,PSE$sorter,mean)
PSE$Whitehall <- factor(PSE$Whitehall,levels(PSE$Whitehall)[c(3,1,2)])

labelsx <- c('2010Q3','Q4','2011Q1','Q2','Q3','Q4','2012Q1','Q2','Q3','Q4','2013Q4')
PSE$totalgroup <- ifelse(PSE$Dept=='Total excl. WH FCO',TRUE,FALSE)
HLcol <- IfGcols[3,1]

plot_PSE <- ggplot(data=PSE[PSE$measure=='Cumulative_Perc_net_change' &
                                 PSE$chart,],
                  aes(x=Period,y=value, group=group, colour=Whitehall)) + 
  geom_rect(data = PSE[PSE$totalgroup & PSE$measure=='Cumulative_Perc_net_change' &
                         PSE$chart,],fill=HLcol,xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 1, show_guide=FALSE) +
  geom_hline(yintercept=0,colour=IfGcols[1,1]) +
  geom_line(size=1) +
  geom_rect(data = PSE[PSE$totalgroup & PSE$measure=='Cumulative_Perc_net_change' &
                         PSE$chart,],colour=HLcol,xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 1,fill=NA,size=2, show_guide=FALSE) +
  facet_wrap(~Dept, scales='fixed',nrow=3) +
  scale_color_manual(values=c(IfGcols[3,1], IfGcols[2,1], IfGcols[5,1]),
                     labels=c('Departmental group','Whitehall','Non-Whitehall')) +
  scale_y_continuous(labels=percent) +
  scale_x_discrete(labels=labelsx) +
  labs(y='% change since Spending Review 2010 (2010 Q3)',
       x = 'Departmental groups ordered by reduction made to 2013 Q1') +
  guides(colour = guide_legend(ncol = 3,keywidth=unit(1,'cm'))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),plot.title=element_blank(),
        axis.line.x=element_line(size=unit(1,'mm'),colour=IfGcols[1,2]),
        panel.border=element_rect(fill=NA,color=IfGcols[1,2]),
        panel.margin=unit(c(1,1,1,1),'mm'),
        axis.ticks=element_line(colour=IfGcols[1,2]),axis.ticks.x=element_blank(),
        panel.grid=element_line(colour=IfGcols[1,3]),panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
        axis.text.x=element_text(size=8))
plot_PSE
# Save ggplot -------------------------------------------------------------

SavePlot(plotname='plot_PSE',plotformat=plotformat,ffamily=fontfamily,
         ploth=ph,plotw=pw)
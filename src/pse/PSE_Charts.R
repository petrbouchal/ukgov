# Setup -------------------------------------------------------------------
source('./src/lib/lib_acses.R')
source('./src/pse/PSE_Reshape.R') # to prepare data if needed
uu <- change

# Plain chart in ggplot2 --------------------------------------------------

uu$chart <- TRUE
uu$chart[uu$Dept == 'Total employment']  <- FALSE
uu$chart[uu$Dept == 'Total']  <- FALSE
uu$chart[uu$Dept == 'AGO']  <- FALSE
uu$chart[uu$Dept == 'NIO']  <- FALSE
uu$chart[uu$Dept == 'GEO']  <- FALSE
uu$chart[uu$Dept == 'Welsh Gov']  <- FALSE
uu$chart[uu$Dept == 'Scot Gov']  <- FALSE
uu$chart[uu$Dept == 'FCO' & uu$Whitehall!='Departmental Group']  <- FALSE

# change order of facets
totals <- uu[uu$measure=='Cumulative_Perc_net_change' & uu$Whitehall=='Departmental Group' & uu$Period=='2013Q3',
                  c(1,3)]
totals$sorter <- totals$value
totals$value <- NULL
uu <- merge(uu, totals,all.x=TRUE)
# uu$sorter[uu$Dept =='Total excl. Whitehall FCO'] <- max(uu$sorter)*1.1
uu$Dept <- reorder(uu$Dept,uu$sorter,mean)
uu$Whitehall <- factor(uu$Whitehall,levels(uu$Whitehall)[c(2,1,4,3)])

labelsx <- c('2010Q3','Q4',
             '2011Q1','Q2','Q3','Q4',
             '2012Q1','Q2','Q3','Q4',
             '2013Q1','Q2','Q3')
uu$totalgroup <- ifelse(uu$Dept=='Total excl. Whitehall FCO',TRUE,FALSE)
HLcol <- IfGcols[4,3]
HLmarg <- IfGcols[4,1]

# Build chart -------------------------------------------------------------

plotname <- 'plot_PSE'

uu <- uu[uu$measure=='Cumulative_Perc_net_change' & uu$chart,]
uu2 <- uu[uu$totalgroup & uu$measure=='Cumulative_Perc_net_change' & uu$chart,]

plot_PSE <- ggplot(data=uu,aes(x=Period,y=value, group=group, colour=Whitehall)) + 
#   geom_rect(data=uu2[uu2$Period=='2013Q3' & uu2$Whitehall=='Non-Whitehall Civil Service',],fill=HLcol,
#             xmin = -Inf,xmax = Inf,colour=NA,
#             ymin = -Inf,ymax = Inf,alpha = 1, show_guide=FALSE) +
  geom_hline(yintercept=0,colour=IfGcols[1,1]) +
  geom_line(data=uu,size=1) +
#   geom_rect(data=uu2[uu2$Period=='2013Q3' & uu2$Whitehall=='Non-Whitehall Civil Service',],colour=HLmarg,
#             xmin = -Inf,xmax = Inf,
#             ymin = -Inf,ymax = Inf,alpha = 1,fill=NA,size=1, show_guide=FALSE) +
  facet_wrap(~Dept,nrow=3) +
  scale_color_manual(values=c(IfGcols[3,1], IfGcols[2,1], IfGcols[5,1])) +
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

# SavePlot(plotname=plotname,plotformat=plotformat,ffamily=fontfamily,ploth=ph,plotw=pw)
# source('./src/lib/lib_acses.R')
library(pbtools)

# Load data ---------------------------------------------------------------

origdata <- read.csv('./data-input/wmi_2014-09-19.csv')

# Process data ------------------------------------------------------------

uu <- origdata %>%
  filter(IfG.classification=='Whitehall') %>%
  select(Dept= Short.Form, Month,
         FTE_Payroll= Payroll.staff.Total.Employees.Full.time.Equivalent,
         Paybill_Payroll= Payroll.Staff.Costs.Total.paybill.for.payroll.staff) %>%
  melt(id.vars=c('Dept','Month')) %>%
  mutate(value=as.numeric(value),
         grp = paste0(variable,Dept), 
         Month = as.Date(Month,format='%d/%m/%Y')) %>%
  group_by(variable, Dept, Month, grp) %>%
  summarise(value=sum(value, na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(variable,grp) %>%
  arrange(Month) %>%
  mutate(base=first(value),
         valuepc = (value-base)/base) %>%
  filter(Dept!='Wales Off' & Dept!='Scot Off' & Dept!='AG Depts') %>%
  mutate(yvar = valuepc) %>%
  arrange(variable, grp, Month)

# Setup plot --------------------------------------------------------------

plottitle <- ''
plotname <- 'plot_WMI'
ylabel <- 'Change March 2012-March 2013 (%)'
xlabel <- NULL
ylimits <- c(min(uu$yvar,na.rm=TRUE),max(uu$yvar,na.rm=TRUE))

# Build plot --------------------------------------------------------------

loadcustomthemes(ifgbasecolours,'Calibri')
plot_WMI <- ggplot(uu,aes(x=Month,y=yvar)) +
  geom_hline(y=0, colour=ifgcolours[1,2]) +
  geom_line(aes(colour=variable,group=grp),size=.6) +
  facet_wrap(~Dept,nrow=5,scales='fixed') +
  #scale_x_datetime(labels=date_format('%Y-%m'),breaks=date_breaks(width = "3 months"))+
#   scale_x_datetime(labels=c('Apr\n2012','Jul\n2012','Oct\n2012','Jan\n2013'))+
  scale_colour_manual(values=c(ifgcolours[3,1],ifgcolours[2,1]),
                      labels=c('Payroll staff (FTE)','Pay bill (payroll only)')) +
  scale_y_continuous(labels=percent,limits=ylimits,breaks=c(-.3,-.15,0,.15,.3)) +
  guides(colour=guide_legend(keywidth=unit(1,'cm'))) +
  labs(x=xlabel,y=ylabel) +
  theme(panel.border=element_rect(fill=NA,colour=ifgcolours[1,2]),
        plot.title=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major.y=element_line(colour=ifgcolours[1,3]),
        axis.text=element_text(size=8))
plot_WMI

# SavePlot(plotname=plotname,plotformat=plotformat,ploth=ph,plotw=pw,ffamily=fontfamily)
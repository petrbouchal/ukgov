source('./src/acses_lib.R')

origdata <- read.csv('./data-input/WMI.csv')

uu <- origdata
uu$Paybill_Payroll <- uu$Paybill_Payroll/10E3

uu <- melt(uu)
uu$grp <- paste0(uu$variable,uu$Dept)
uu$Month <- as.POSIXct(uu$Month)

wmiplot <- ggplot(uu,aes(x=Month,y=value)) +
  geom_line(aes(colour=variable,group=grp)) +
  scale_x_datetime(labels=date_format('%Y-%m')) +
  facet_wrap(~Dept,scales='free_y',nrow=7)
wmiplot
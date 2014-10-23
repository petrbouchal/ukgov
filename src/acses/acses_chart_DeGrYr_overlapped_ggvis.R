library(plyr)
library(ggvis)
library(pbtools)
source('./src/lib/lib_acses.R')

# Load data ---------------------------------------------------------------

filename <- 'ACSES_Gender_Dept_Grade_Pay_data.tsv'
origdata <- LoadAcsesData2014(file_name=filename,location=location)
managed <- TRUE

# Process data ------------------------------------------------------------
uu <- origdata %>%
  # Filter out unneeded totals
  filter(Wage.band=='Total' & Gender=='Total') %>%
  # Add organisation data and exclude what isn't needed
  AddOrgData(managedonly = managed) %>%
  filter(Group!='NIO' & Group!='AGO') %>%
  # Drop unneeded vars
  select(Group, Civil.Service.grad, Date, count, Organisation) %>%
  # Summarise by departmental group
  group_by(Group, Date, Civil.Service.grad) %>%
  summarise(count=sum(count, na.rm=T)) %>%
  # Create total variable - dept group total on each row
  group_by(Group, Date) %>%
  mutate(total=sum(count[Civil.Service.grad=='Total'])) %>%
  # Exclude unneeded grades
  filter(Civil.Service.grad!='Total' & Civil.Service.grad!='Not reported') %>%
  # create share variable
  mutate(share=count/total) %>%
  RelabelGrades()

# Create 'managed' total if needed
if(managed) {
  managedtotal <- uu %>%
    filter(Group!='Whole Civil Service') %>%
    group_by(Date, Civil.Service.grad) %>%
    summarise(count=sum(count),total=sum(total), share=count/total) %>%
    mutate(Group = 'All\nmanaged')
  uu <- rbind(uu[uu$Group!='Whole Civil Service',],managedtotal)
}

# Sort departments --------------------------------------------------------
gradevalues <- data.frame('gradeval'=c(1:length(levels(uu$Civil.Service.grad))),
                          'Civil.Service.grad'=levels(uu$Civil.Service.grad))
uu <- merge(uu,gradevalues) %>%
  group_by(Group, Date, Civil.Service.grad) %>%
  mutate(sharebothgenders=sum(share, na.rm=TRUE)) %>%
  merge(gradevalues) %>%
  mutate(gradescore = gradeval*sharebothgenders) %>%
  group_by(Group,Date) %>%
  mutate(meangradescore=mean(gradescore), sorter=meangradescore) %>%
  ungroup() %>%
  filter(Date!='2008' & Date!='2009') %>%
  mutate(Group=reorder(Group,-sorter,mean)) %>%
  mutate(totalgroup = ifelse(Group=='Whole Civil Service' | Group=='All\nmanaged',
                             TRUE, FALSE)) %>%
  select(-meangradescore, -sharebothgenders)


# Build plot --------------------------------------------------------------

# create 'left' and 'right' data
uu$share2 <- uu$share/2
uu$left <- TRUE
uu2 <- uu
uu2$share2 <- -uu2$share/2
uu2$left <- FALSE

uu <- rbind(uu,uu2)
uu$grp <- paste0(uu$left, uu$Date)
uu <- arrange(uu, Group, grp)


HLcol <- ifelse(managed,ifgcolours[4,1],ifgcolours[3,1])

plotname <- 'plot_DeGrYr_overlapped'

plottitle <- 'Civil Servants by gender and grade'
ylabel = ' most senior workforce top left'
if(managed){
  plottitle=paste0(plottitle,' - managed departments')
  ylabel = paste0('% of Civil Servants in grade. Managed departments, ',ylabel)
  plotname = paste0(plotname,'_WH')
} else {
  plottitle=paste0(plottitle,' - departmental groups.')
  ylabel = paste0('% of Civil Servants in grade. Departmental groups, ',ylabel)
  plotname = paste0(plotname,'_Group')
}

uu$yvar <- uu$share2

maxY <- max(abs(uu$yvar),na.rm=TRUE)
ylimits <- c(-maxY*1.04, maxY*1.04)
ybreaks <- c(-.3,-.15,0,.15,.3)
ylabels <- paste0(abs(ybreaks*100*2),'%')
ylabels[1:2] <- ''
ylabels[3] <- '0'

loadcustomthemes(ifgcolours, fontfamily)

visdata <- uu[uu$Group=='CO',]
# visdata <- uu[uu$Group=='CO' & uu$left==TRUE,]
visdata$updown <- ifelse(visdata$left, 'TOP','BOTTOM')

# visdata <- visdata %>% group_by(updown)

year_lr <- left_right(2010, 2014, value=2010, step=1)
year_wrapper <- reactive({
  as.numeric(year_lr() == visdata$Date)
})

class(year_wrapper) <- c("broker", class(year_wrapper))
attr(year_wrapper, "broker") <- attr(year_lr, "broker")

visdata %>%
  ggvis(y=~Civil.Service.grad, x=~share2,opacity=year_wrapper, stroke=~Date) %>%
#   layer_lines() %>%
  layer_points(fill=~Date, shape:='square')

visdata %>%
  ggvis(x=~Civil.Service.grad, y=~share2, opacity:=input_slider(min(visdata$Date), max(visdata$Date), step=1, 
                                                                map=function(x) ifelse(visdata$Date[visdata$Date==x & visdata$left==F] == x, 0, 1))) %>%
  layer_bars(fill:='red', width=1)

visdata1y <- visdata[visdata$Date==2014,]

visdata1y %>%
  ggvis(x=~Civil.Service.grad, y=~share2) %>%
  layer_bars(fill=~updown, stack=F, stroke:='none', width=1) %>%
  mark()

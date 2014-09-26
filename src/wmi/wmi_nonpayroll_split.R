
source('./src/wmi/wmi_processandplot.R')

loadcustomthemes(mycols = ifgbasecolours, 'Calibri')

# area: departmental breakdown of non-payroll staff numbers -----------------
monthslabels <- c('Mar\n2012','Sep\n2012','Mar\n2013','Sep\n2013','Mar\n2014')
monthsbreaks <- as.Date(c('2012-03-01','2012-09-01',
                          '2013-03-01','2013-09-01',
                          '2014-03-01'))
deptlist <- c('MoD','HO','MoJ','DH','BIS','CO','DfE','DECC','DWP','HMRC','DfID',
              'Defra','DfT','DCLG','DCMS','HMT')
scaledata <- data.frame(department=deptlist,
                        value = c(rep(3000,3),
                                  rep( 800,1),
                                  rep( 400,5),
                                  rep( 120,7)),
                        variable='agency',
                        month=as.Date('2012-03-01'))

wmipb$department <- factor(wmipb$department,levels=deptlist)

ggplot(wmipb, aes(month, value, fill=variable)) +
  geom_area(position='stack') + 
  facet_wrap(~department, scales='free_y') + 
  scale_fill_manual(values=ifgbasecolours,
                    labels=c('Agency / clerical',
                             'Interim managers',
                             'Specialist contractors',
                             'Consultants / consultancy staff')) + 
  scale_x_date(labels=monthslabels, breaks=monthsbreaks) +
  scale_y_continuous(labels=comma) + 
  geom_blank(data=scaledata) + 
  theme(legend.key.width=unit(.2,'cm'),legend.key.height=unit(.2,'cm'),
        axis.text = element_text(colour=ifgbasecolours[1]),
        strip.text=element_text(size=12),
        legend.text=element_text(size=12))

saveplot('Tempsbydept',plotformat = 'pdf',ploth = 11.5, plotw = 17.5,
         plotdir = './charts-output/', dpi=300)

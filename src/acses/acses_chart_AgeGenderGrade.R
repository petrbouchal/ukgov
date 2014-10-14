library(pbtools)
source('./src/lib/lib_acses.R')

origdata <- LoadAcsesData2014('ACSES_Gender_Dept_Age_Grade_data.tsv',location='ifg')

uu <- origdata %>%
  select(Date,Gender,Age.band,Civil.Service.grad,Organisation,count) %>%
  filter(Date==2014 & Civil.Service.grad!='Not reported' &
           Age.band!='Unknown age' & Gender!='Total') %>%
  AddOrgData(managedonly = TRUE) %>%
  filter(Group!='AGO' & Group!='NIO') %>%
  RelabelAgebands() %>%
  RelabelGrades() %>%
  filter(Group=='Whole Civil Service') %>%
  group_by(Date, Group, Age.band, Civil.Service.grad, Gender) %>%
  summarise(count=sum(count, na.rm=T)) %>%
  group_by(Date, Group, Age.band, Civil.Service.grad) %>%
  mutate(countbothgenders = sum(count),
            sharefemale = count/countbothgenders) %>%
  filter(Gender=='Female')

plot <- ggplot(uu, aes(x=Age.band, y=Civil.Service.grad, fill=sharefemale)) + 
  geom_tile() +
  facet_wrap(~Group) +
  scale_fill_distiller()
plot

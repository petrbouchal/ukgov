library(pbtools)
source('./src/lib/lib_acses.R')

origdata <- LoadAcsesData2014('ACSES_Gender_Dept_Age_Grade_data.tsv',location='ifg')

uu <- origdata %>%
  select(Date,Gender,Age.band,Civil.Service.grad,Organisation,count) %>%
  filter(Date==2014 & Civil.Service.grad!='Not reported' &
           Age.band!='Unknown age' & Gender=='Total') %>%
  AddOrgData(managedonly = TRUE) %>%
  filter(Group!='AGO' & Group!='NIO') %>%
  RelabelAgebands() %>%
  group_by(Date, Group, Age.band, Civil.Service.grad) %>%
  summarise(count=sum(count, na.rm=TRUE)) %>%
  group_by(Group, Civil.Service.grad) %>%
  mutate(total=count[Age.band=='Total'],
         share=count/total) %>%
  filter(Age.band!='Total' & Civil.Service.grad!='Total') %>%
  RelabelGrades()

plot <- ggplot(uu, aes(x=Age.band, y=Civil.Service.grad, fill=share)) + 
  geom_tile() +
  facet_wrap(~Group) +
  scale_fill_distiller()
plot
  
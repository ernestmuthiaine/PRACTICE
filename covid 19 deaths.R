#loading packages
library(tidyverse)
library(gganimate)
library(lubridate)


# Data source : https://covid.ourworldindata.org/data/owid-covid-data.xlsx

#loading the data
covid<-read.csv("C:\\Users\\niestos\\Desktop\\covid 19 dataset.csv")


#filtering data in Africa
Africa<-covid %>% 
  filter(continent=='Africa')


#which african country has the highest number of deaths(as of 12-03-2021)?
africa_deaths<-Africa %>% 
  select(location,date,new_deaths,total_deaths) %>% 
  group_by(location) %>% 
  summarise(total=sum(new_deaths,na.rm=T))

#plot of total deaths in africa 
plot1<-africa_deaths %>% 
  ggplot(aes(x=reorder(location,total),y=total,
             fill=(location %in% c("Kenya","South Africa","Uganda"))))+
  geom_col()+
  geom_text(aes(label=ifelse(location %in% c("Kenya","Uganda","Tanzania","South Africa"),total,' ')),
            size=3,hjust=0.1)+
  coord_flip()+
  labs(y='number of deaths',
       x='country',
       title='covid 19 deaths in african countries',
       subtitle = 'as of 12-03-2021',
       caption = 'Twitter: @ernestmuthiaine')+
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(color='maroon',hjust=0.5,face='bold'),
        plot.subtitle = element_text(face = 'italic',hjust=0.5,size=8),
        legend.position = 'none' )

ggsave('plot1.png')

#growth of covid 19 deaths in kenya
kenya_deaths<-covid %>% 
  select(location,date,total_deaths) %>% 
  filter(location=="Kenya") %>% 
  mutate(date=mdy(date)) %>% 
  drop_na() 


min <- as.Date("2020-03-13")
max <- as.Date("2021-04-1")

#visualizing growth of covid 19 deaths in kenya
plot2<-kenya_deaths %>% 
  ggplot(aes(x=date,y=total_deaths))+
  geom_point()+
  geom_line(color='red',size=1.5)+
  labs(title='growth of covid 19 deaths in kenya',
       subtitle='as of 12-03-2021',
       y='total deaths',
       x='date')+
  scale_x_date(limits = c(min,max))+
  transition_reveal(date)+
  theme(panel.background = element_rect(fill=NA),
        plot.title=element_text(color='maroon',hjust=0.5,face='bold'),
        plot.subtitle=element_text(face = 'italic',hjust=0.5,size=8))

ggsave('plot2.png')



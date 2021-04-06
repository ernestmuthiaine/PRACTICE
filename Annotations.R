# loading required package
library(tidyverse)
library(lubridate)


#data source
#https://storage.googleapis.com/kaggle-data-sets/951970/2084882/bundle/archive.zip?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20210404%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20210404T095559Z&X-Goog-Expires=259199&X-Goog-SignedHeaders=host&X-Goog-Signature=505ddcd8c59027f29c76fb128d44d72cfb0c6f36caf78f398ce691b6b288a6e2bdd87b2b52ab7a8b3ce00f7d55eb0b8b8262210f89f697c12bea15b85d2de7397012b8da7c58c33bbaca6dd3b5e7a1341472dcf5d983fd7ce12437141bc265baa32a89024bcfe96ec8620f368bd12858f67c70254b74de18c29b660c8e55408a8a22e34ca5453ca5a8aa8ca5b8a42849f7a994a49668a3ded6c360c0b50a1f2cd0d26ae614e04b5f3a496f1c98ba0e79d6626f180f7264ff98f479a10161ad9604cabb97848c9c437746d1741c88de69c2205e3917440b52fa9ddc6234195d1f5aa4163b5f31279827d43410511f34a8f317a92d54cf2b260360cf2a8b6d66cc

#loading in the data
covid<-read.csv('C:\\Users\\niestos\\Desktop\\Practice\\PRACTICE2\\global cases.csv')


#create a dataset for kenyan cases
covid %>% 
  select(`Country.Region`,Kenya)->kenya

#renaming country.region to date
kenya %>%
  filter(Kenya>=0) %>% 
  rename('date'='Country.Region','cases'='Kenya') %>% 
  mutate(date=mdy(date))->kenya 


plot<-kenya %>% 
  ggplot(aes(date,y=cases))+
  geom_line()

plot+
  annotate(geom='text',x=as.Date('2020-03-13'),y=1500,label='case 1',size=3)+
  annotate(geom='segment',
           x=as.Date('2020-03-13'),xend=as.Date('2020-03-13'),
           y=1450,yend=1,color='red',
           arrow=arrow(length = unit(0.1,'in')))+
  annotate(geom = 'curve',
           x=as.Date('2020-05-27'),xend=as.Date('2020-03-27'),
           y=1750,yend=50,curvature=.1,color='red',
           arrow=arrow(length = unit(0.3,'cm')))+
  annotate('text',x=as.Date('2020-05-27'),y=1790,
           label='curfew and lockdown imposed',size=3)+
  annotate('text',x=as.Date('2020-10-05'),y=2000,
           label='universities re-opened',size=3)+
  annotate('segment',x=as.Date('2020-10-05'),xend=as.Date('2020-10-05'),
           y=2000,yend = 400,
           arrow=arrow(length = unit(0.1,'cm')))+
  annotate('segment',x=as.Date('2020-07-06'),xend=as.Date('2020-07-06'),
           y=1450,yend=400,color='green',
           arrow=arrow(length = unit(0.1,'cm')))+
  annotate('text',x=as.Date('2020-07-06'),y=1500,
           label='lockdown lifted',size=3)+
  annotate('curve',x=as.Date('2021-03-01'),xend = as.Date('2021-03-26'),
           curvature=-0.2,y=2200,yend=2000,color='red',
           arrow=arrow(length = unit(0.1,'cm')))+
  annotate('text',x=as.Date('2021-02-01'),y=2300,
           label='lockdown
                re- imposed',size=3)+
  annotate('text',x=as.Date('2021-01-04'),1800,
           label='schools re-open',size=3)+
  annotate('segment',x=as.Date('2021-01-04'),xend = as.Date('2021-01-04'),
           y=1790,yend=250,
           arrow=arrow(length = unit(0.2,'cm')))+
  labs(title ='events since first case of covid19 in kenya',
       caption = 'Twitter: @MuthiaineErnest')+
  theme(panel.background = element_rect(fill=NA),
        plot.title = element_text(face = 'bold',hjust=0.5,size=14))

ggsave('plot.png',
       width=13,
       height=13,
       units=c('cm'))

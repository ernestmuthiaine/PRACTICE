# The data used is obtained from Movies on Netflix, Prime Video, Hulu and Disney+ 
#  dataset in Kaggle

#loading required libraries
library(tidyverse)
library(ggthemes)
library(patchwork)


#reading in the data
movies<-read.csv("C:\\Users\\niestos\\Desktop\\R\\MoviesOnStreamingPlatforms_updated.csv")

str(movies)  # to see the structure of the dataset


#recoding the levels in variables netflix,Hulu,Prime Video and Disney to yes and no
#and transforming the variables to factor variable

movies<-movies %>% 
  select(everything()) %>% 
  mutate(Netflix=as.factor(recode(Netflix,`1`='yes',`0`='no'))) %>% 
  mutate(Hulu=as.factor(recode(Hulu,`1`='yes',`0`='no'))) %>%
  mutate(Prime.Video=as.factor(recode(Prime.Video,`1`='yes',`0`='no'))) %>%
  mutate(Disney.=as.factor(recode(Disney.,`1`='yes',`0`='no')))

#distribution of movies released on the streaming platforms in 2019
plot1<-movies %>%   select(Year,Netflix:Disney.) %>%  
  pivot_longer(cols=c(Netflix:Disney.), 
               names_to='platform',
               values_to='available') %>% filter(Year==2019) %>% 
  filter(available=='yes')%>% 
  group_by(platform) %>% 
  count() %>% 
  ggplot(aes(platform,n,fill=platform))+
  geom_bar(stat='identity',width = 0.6)+
  geom_text(aes(label=n,vjust=1),size=4.5)+
  labs(title='movies on netflix,prime video,hulu and disney in 2019',
       y='number of movies',
       caption='Source: Kaggle')+
  theme_hc()+
  theme(legend.position = 'none')



#Movies Released each Year by each platform
  
movies1<-movies %>% 
  select(Title,Year,Netflix:Disney.) %>% 
  pivot_longer(cols=c(Netflix:Disney.),
               names_to='platform',
               values_to='available') %>% 
  arrange(Year) %>% 
  filter(available=='yes') %>% group_by(platform,Year)%>% 
  count()

plot2<-movies1 %>% 
  ggplot(aes(Year,n,color=platform))+
  geom_line(size=1)+
  xlim(c(1902,2020))+
  labs(title='movies released each year',
       subtitle = 'From 1902 to 2020',
       y='movies')+
  theme_clean()


#average movie ratings   on rotten tomatoes between 2010 and 2020
ratings<-movies %>% 
  select(Title,Year,Netflix:Disney.,Rotten.Tomatoes) %>% 
  pivot_longer(cols=c(Netflix:Disney.),
               names_to='platform',
               values_to='available') %>% 
  arrange(Year) %>% 
  filter(available=='yes') %>% 
  filter(Rotten.Tomatoes != '',Year>2010) %>% 
  mutate(Rotten.Tomatoes=as.numeric(gsub('%','',Rotten.Tomatoes))) %>% 
  group_by(platform,Year) %>% 
  summarise(rating=mean(Rotten.Tomatoes))


#visualizing the ratings with a line graph
plot3<-ratings %>% ggplot(aes(Year,rating,color=platform))+
  geom_line(size=1.5)+
  labs(title='Average movie ratings from 2010 to 2020',
       subtitle='Rotten Tomatoes Ratings',
       y='Rating',
       x='Year')+
  xlim(c(2010,2020))+
  ylim(c(50,100))+
  theme_fivethirtyeight()

plots<-(plot1+plot2)/(plot3)+
labs(caption='Source:kaggle -Movies on Netflix, Prime Video, Hulu and Disney+')

ggsave('plots.jpeg')













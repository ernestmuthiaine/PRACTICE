#importing the data into R.
pl<-read.csv(file.choose(),header=T)

#loading packages
library(tidyverse)
library(ggrepel)

view(pl)

# premier league teams and the number of goals scored by each
pl%>%
  select(Squad,GF)%>%
  ggplot(aes(x=reorder(Squad,-GF),y=GF))+
  geom_bar(stat='identity',fill='maroon')+
  labs(title='goals scored by each premier league team',
       x='team',
       y='goals scored')+
  theme(axis.text=element_text(angle=90),
        panel.background=element_rect(fill=NA))


#premier league teams and the number of goals conceded by each
pl%>%
  select(Squad,GA)%>%
  ggplot(aes(x=reorder(Squad,-GA),y=GA))+
  geom_bar(stat='identity',fill='maroon')+
  labs(title='goals conceded by each premier league team',
       x='team',
       y='goals conceded')+
  theme(axis.text=element_text(angle=90),
        panel.background=element_rect(fill=NA))


#leading scorers in each of the premier league teams 
pl%>%select(Top.Team.Scorer,Squad)%>%
  separate(col='Top.Team.Scorer',
           into=c('Scorer','goals'),sep='-')%>%mutate(goals=as.numeric(goals))%>%
  separate(col='Scorer',
           into=c('first_name','last_name'),sep=' ')%>%
  ggplot(aes(x=reorder(last_name,goals),y=goals))+
  geom_point(aes(alpha=0.7))+
  geom_text_repel(aes(label=paste(last_name,Squad,sep='-')),
                  angle=90,size=2.5,vjust=0.5)+
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        panel.background=element_rect(fill='white'),
        legend.position='none')

ggsave('club_topscorers.png')

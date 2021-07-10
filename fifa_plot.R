#devtools::install_github("ellisp/ggflags")
library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)
library(datasets)
library(ggflags)
library(countrycode)

#Data source 
fifa_men <- read_csv("Z:/ZW_R_DICT/Animated_plot/FIFA/fifa_men.csv")
fifa_men$Category<-'Men'
fifa_men

fifa_women <- read_csv("Z:/ZW_R_DICT/Animated_plot/FIFA/fifa_women.csv")
fifa_women$Category<-'Women'
fifa_women


#data prep
fifa_men%>%
  rename(points=`Total PointsPTS`)%>%
  mutate(date=as.Date(date,'%m/%d/%Y'),
         Team_abr=substr(Team,nchar(Team)-3+1, nchar(Team)),
         Team=substr(Team,1, nchar(Team)-3))%>%
  #This step is only for men, Mar 2010 has two records so drop one
  filter(date>'2000-01-01' & date!='2010-03-31')%>%
  arrange(date,RK)%>%
  #reset ties to ranks at default seq.
  group_by(Category,date)%>%
  slice(1:10)%>%
  mutate(ranking=row_number(),
         country=countrycode(Team_abr,origin = 'ioc',destination = 'iso2c'),
         country=if_else(Team_abr=='ENG','gb-eng',
                         if_else(Team_abr=='NGA','ng',
                                 if_else(Team_abr=='WAL','gb-wls',country))),
         country=ifelse(is.na(country),NA,tolower(country)))->fifa_men_2
View(fifa_men_2)


fifa_women%>%
  rename(points=`Total PointsPTS`)%>%
  mutate(date=as.Date(date,'%m/%d/%Y'),
         Team_abr=substr(Team,nchar(Team)-3+1, nchar(Team)),
         Team=substr(Team,1, nchar(Team)-3))%>%
  arrange(date,RK)%>%
  #reset ties to ranks at default seq.
  group_by(Category,date)%>%
  slice(1:10)%>%
  mutate(ranking=row_number(),
         country=countrycode(Team_abr,origin = 'ioc',destination = 'iso2c'),
         country=if_else(Team_abr=='ENG','gb-eng',
                         if_else(Team_abr=='NGA','ng',
                                 if_else(Team_abr=='WAL','gb-wls',country))),
         country=ifelse(is.na(country),NA,tolower(country)))->fifa_women_2
View(fifa_women_2)



fifa_men_2%>%
  #filter(date=='2005-01-19')%>%
  mutate(date_format = format(date, '%b %Y'),
         date_format=factor(date_format, unique(date_format)))%>%
  ggplot()+
  geom_col(aes(x=ranking,y=points,group=Team),fill='orange')+
  geom_text(aes(x=ranking,y=-10,label=Team,group=Team,fontface=2),hjust=1,size=4)+
  geom_text(aes(x=ranking,y=points+500,
                label=format(round(points,0),nsmall = 0),
                group=Team),hjust=0)+
  geom_flag(aes(x=ranking,y=points+300,country=country,group=Team),size=12)+
  labs(title='FIFA-Coca Cola Men\'s World Ranking', y = 'Points', x = NULL,
       subtitle = 'Date: {closest_state}')+
  coord_flip(clip = 'off')+
  scale_x_reverse()+
  theme_bw()+
  theme(text = element_text(face='bold'),
        axis.line.x = element_line(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = 'lightblue'),
        panel.background = element_rect(fill = 'lightblue'),
        legend.position = 'none',
        plot.margin = unit(c(1,1,1,2.5),units = 'cm'))+
  transition_states(date_format, transition_length = 10,state_length = 1,wrap = FALSE)+
  ease_aes('cubic-in-out')->fifa_men_anim

animate(fifa_men_anim,duration = 200, fps = 10, end_pause = 40,
        height=500,width=600, res=100,renderer = gifski_renderer())


###################WOMEN#######################################
fifa_women_2%>%
  #filter(date=='2003-07-16')%>%
  mutate(date_format = format(date, '%b %Y'),
         date_format=factor(date_format, unique(date_format)))%>%
  ggplot()+
  geom_col(aes(x=ranking,y=points,group=Team),fill='orange')+
  geom_text(aes(x=ranking,y=-10,label=Team,group=Team,fontface=2),hjust=1,size=4)+
  geom_text(aes(x=ranking,y=points+500,
                label=format(round(points,0),nsmall = 0),
                group=Team),hjust=0)+
  geom_flag(aes(x=ranking,y=points+300,country=country,group=Team),size=12)+
  labs(title='FIFA-Coca Cola Women\'s World Ranking', y = 'Points', x = NULL,
       subtitle = 'Date: {closest_state}')+
  coord_flip(clip = 'off')+
  scale_x_reverse()+
  theme_bw()+
  theme(text = element_text(face='bold'),
        axis.line.x = element_line(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = 'lightblue'),
        panel.background = element_rect(fill = 'lightblue'),
        legend.position = 'none',
        plot.margin = unit(c(1,1,1,2.5),units = 'cm'))+
  transition_states(date_format, transition_length = 10,state_length = 1,wrap = FALSE)+
  ease_aes('cubic-in-out')->fifa_women_anim

animate(fifa_women_anim,duration = 80, fps = 10, end_pause = 40,
        height=500,width=600, res=100,renderer = gifski_renderer())




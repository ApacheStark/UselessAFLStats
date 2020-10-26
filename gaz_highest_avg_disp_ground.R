# GAZ Highest avg at each ground
library(fitzRoy)
library(tidyverse)
library(lubridate)

afttable <- fitzRoy::get_afltables_stats(start_date = '2001-01-01')

gaz <- afttable %>% 
  filter(ID == 1105) %>% 
  select(ID, Date, First.name, Surname, Kicks, Handballs, Venue) %>% 
  mutate(Disposals = Kicks + Handballs) %>% 
  arrange((Date))

gaz %>% 
  group_by(Venue) %>% 
  summarise(Games.Played = n(),
            Avg.Disposals = mean(Disposals))  %>% 
  arrange(desc(Avg.Disposals))

gaz50 <- gaz[50:nrow(gaz),]


gaz50 %>% 
  group_by(Venue) %>% 
  summarise(Games.Played = n(),
            Avg.Disposals = mean(Disposals))  %>% 
  arrange(desc(Avg.Disposals))

gaz100last <- gaz[(nrow(gaz)-100):nrow(gaz),]

gaz100last %>% 
  group_by(Venue) %>% 
  summarise(Games.Played = n(),
            Avg.Disposals = mean(Disposals))  %>% 
  arrange(desc(Avg.Disposals))

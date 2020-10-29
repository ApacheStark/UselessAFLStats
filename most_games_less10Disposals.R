# most single digit games
library(fitzRoy)
library(tidyverse)
library(lubridate)

afltable <- fitzRoy::get_afltables_stats(start_date = '1990-01-01')

afltable %>%
  mutate(Disposals = Kicks + Handballs) %>% 
  group_by(ID, First.name, Surname) %>% 
  summarise(
            Total.less10 = sum(Disposals < 10),
            Total.Games = n(),
            PP = paste0(round(Total.less10/Total.Games*100,0),'%')) %>% 
  arrange(desc(Total.less10)) %>% 
  ungroup() %>% 
  select(-ID)

afltable %>%
  mutate(Disposals = Kicks + Handballs) %>% 
  group_by(ID, First.name, Surname) %>% 
  summarise(Total.Zero = sum(Disposals == 0),
            Total.Games = n(),
            PP = paste0(round(Total.Zero/Total.Games*100,0),'%')) %>% 
  arrange(desc(Total.Zero))


# most brownlow votes out of first names (Sam, Daniel, Brett)
library(fitzRoy)
library(tidyverse)
library(lubridate)

afltable <- fitzRoy::get_afltables_stats()
afltable_modern <- fitzRoy::get_afltables_stats(start_date = '1990-01-01')

afltable %>%
  mutate(Disposals = Kicks + Handballs) %>% 
  filter(str_detect(First.name, 'Sam|Daniel|Gavin|Michael')) %>% 
  group_by(ID, First.name, Surname) %>% 
  summarise(
    Total.Disp = sum(Disposals)) %>% 
  arrange(desc(Total.Disp)) %>% 
  ungroup() %>% 
  select(-ID)

afltable %>%
  mutate(Disposals = Kicks + Handballs) %>% 
  filter(str_detect(First.name, 'Pat')) %>%
  group_by(First.name) %>% 
  summarise(
    Total.Disp = sum(Disposals, na.rm = T)) %>% 
  arrange(desc(Total.Disp)) %>% 
  ungroup() 

afltable %>%
  mutate(Disposals = Kicks + Handballs) %>% 
  # filter(str_detect(First.name, 'Sam|Brett|Steve')) %>% 
  group_by(First.name) %>% 
  summarise(
    Total.Votes = sum(Brownlow.Votes, na.rm = T)) %>% 
  arrange(desc(Total.Votes)) %>% 
  ungroup() 


afltable_modern %>%
  mutate(Disposals = Kicks + Handballs) %>% 
  # filter(str_detect(First.name, 'Fred')) %>%
  group_by(First.name) %>% 
  summarise(
    Games = n(),
    Votes = sum(Brownlow.Votes, na.rm = T),
    Disposals = sum(Disposals, na.rm = T)) %>% 
  filter(Votes == 0) %>%
  mutate(
    GamesRatio = Games/Votes,
    DispRatio = Disposals/Votes
  ) %>% 
  arrange(desc(Games)) %>% 
  ungroup() 

afltable_modern %>%
  mutate(Disposals = Kicks + Handballs) %>% 
  # filter(str_detect(First.name, 'Denis')) %>%
  group_by(First.name) %>% 
  summarise(
    Games = n(),
    Votes = sum(Brownlow.Votes, na.rm = T),
    Disposals = sum(Disposals, na.rm = T)) %>% 
  filter(Votes > 0 & Disposals > 5000) %>% 
  mutate(
    GamesRatio = Games/Votes,
    DispRatio = Disposals/Votes
  ) %>% 
  arrange((GamesRatio)) %>% 
  ungroup() 


afltable %>%
  mutate(Disposals = Kicks + Handballs) %>% 
  filter(str_detect(First.name, 'Fred') & str_detect(Surname, 'Elliott')) %>% View
  

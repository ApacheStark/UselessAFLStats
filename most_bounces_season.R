# MOST BOUNCES IN A SEASON
library(fitzRoy)
library(tidyverse)
library(lubridate)

afttable <- fitzRoy::get_afltables_stats(start_date = '1990-01-01')

player_names <- afttable %>% 
  select(ID, Season, First.name, Surname, Playing.for) %>% 
  mutate(Name = paste(First.name, Surname)) %>% 
  select(ID, Name, Season, Playing.for) %>% 
  distinct()

most_bounces <- afttable %>% 
  group_by(Season, ID) %>% 
  summarise(Total.Bounces = sum(Bounces, na.rm = TRUE)) %>% 
  arrange(desc(Total.Bounces)) %>% 
  left_join(player_names) %>% 
  select(-ID)

most_bounces

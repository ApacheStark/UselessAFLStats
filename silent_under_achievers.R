# Silent achiever
# most brownlow votes with a best on ground
library(fitzRoy)
library(tidyverse)

afttable <- fitzRoy::get_afltables_stats(start_date = '1990-01-01')

afttable_tbl <- afttable %>% 
  as_tibble() %>% 
  mutate(Name = paste(First.name, Surname)) 
  
three_vote_players <- afttable_tbl %>% 
  select(Name ,Brownlow.Votes) %>% 
  filter(Brownlow.Votes == 3) %>% 
  select(Name) %>% 
  distinct()

silent_under_achiever <- afttable_tbl %>% 
  as_tibble() %>% 
  select(Name,Brownlow.Votes) %>% 
  filter(!(Name %in% three_vote_players$Name)) %>% 
  group_by(Name) %>% 
  summarise(Total.Votes = sum(Brownlow.Votes)) %>% 
  arrange(desc(Total.Votes))

silent_under_achiever

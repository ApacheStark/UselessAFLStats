# No achiever! v2
# most games without a vote!
library(fitzRoy)
library(tidyverse)
library(lubridate)

afttable <- fitzRoy::get_afltables_stats(start_date = '1990-01-01')

afttable %>% 
  filter(Brownlow.Votes > 0) %>% 
  mutate(Season = year(Date)) %>% 
  ggplot(., aes(y = Brownlow.Votes, x = Season)) +
  geom_bar(stat = 'identity')

afttable_tbl <- afttable %>% 
  as_tibble() %>% 
  mutate(Name = paste(First.name, Surname)) 

vote_players <- afttable_tbl %>% 
  select(Name ,Brownlow.Votes) %>% 
  filter(Brownlow.Votes > 0) %>% 
  select(Name) %>% 
  distinct()

no_achiever <- afttable_tbl %>% 
  as_tibble() %>% 
  select(Name,Brownlow.Votes) %>% 
  filter(!(Name %in% vote_players$Name)) %>% 
  count(Name) %>% 
  arrange(desc(n))

no_achiever

afttable_tbl %>% 
  filter(Name == 'Kevin Bartlett') %>% 
  select(Brownlow.Votes) %>% 
  ggplot(., aes(x = Brownlow.Votes)) +
    geom_density()

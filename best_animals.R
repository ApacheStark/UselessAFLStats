# BEST BIRDS
# BEST MAMMALS
# BEST HUMANOIDS

library(fitzRoy)
library(tidyverse)

match_results <- fitzRoy::get_match_results()

sort(unique(match_results$Home.Team))

match_results <- match_results %>% 
  mutate(Winning.Team = case_when(
    Home.Points > Away.Points ~ Home.Team,
    Away.Points > Home.Points ~ Away.Team,
    T ~ 'Draw'
  ),
  Losing.Team = case_when(
    Home.Points < Away.Points ~ Home.Team,
    Away.Points < Home.Points ~ Away.Team,
    T ~ 'Draw'
  ))

birds <- tibble(Team = c(
  'Collingwood',
  'Hawthorn',
  'West Coast',
  'Sydney',
  'Adelaide'
))

bird_results <- match_results %>% 
  filter(Home.Team %in% birds$Team) %>% 
  filter(Away.Team %in% birds$Team)


bird_results %>% 
  filter(Date > '1990-01-01' & Winning.Team != 'Draw') %>% 
  count(Winning.Team, Losing.Team) %>% 
  spread(key = Losing.Team, value = n)

match_results %>% 
  filter(Date > '1990-01-01' &
           !(Winning.Team  %in% c('Fitzroy', 'Gold Coast','GWS')) &
           !(Losing.Team %in% c('Fitzroy', 'Gold Coast','GWS'))) %>% 
  count(Winning.Team, Losing.Team, sort = T) %>% 
  arrange(n) %>% View



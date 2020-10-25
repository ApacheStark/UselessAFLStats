# Match chokers by decade (more scoring opportunities but loses)
library(fitzRoy)
library(tidyverse)

match_history <- fitzRoy::get_match_results()

match_history_chokes <- match_history %>% 
  as_tibble() %>% 
  mutate(Home.shots = Home.Goals + Home.Behinds,
         Away.shots = Away.Goals + Away.Behinds,
         Winner = case_when(Home.Points > Away.Points ~ Home.Team,
                            Away.Points > Home.Points ~ Away.Team),
         Loser = case_when(Home.Points < Away.Points ~ Home.Team,
                           Away.Points < Home.Points ~ Away.Team),
         Choke = case_when(
           Home.shots > Away.shots & Home.Team == Loser ~ TRUE,
           Home.shots < Away.shots & Away.Team == Loser ~ TRUE,
           TRUE ~ FALSE
         )) %>% 
  select(Date, Home.Team, Home.shots, Home.Points, Away.Team, Away.shots, Away.Points, Loser, Choke)

# All time
all_time_choke <- match_history_chokes %>% 
  filter(Choke) %>% 
  count(Loser) %>% 
  arrange(desc(n))

# AFL
afl_choke <- match_history_chokes %>% 
  filter(Choke & Date > '1990-01-01') %>% 
  count(Loser) %>% 
  arrange(desc(n))

# Saves
data_list <- 
  list(
    match_history_chokes,
    all_time_choke,
    afl_choke
  )

saveRDS(data_list, 'OUT/chokers/all_time_chokes.RDS')

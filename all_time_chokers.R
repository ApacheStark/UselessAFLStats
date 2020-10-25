# Match chokers by decade (more scoring opportunities but loses)
library(fitzRoy)
library(tidyverse)
library(lubridate)
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
         ),
         Season = lubridate::year(Date)) %>% 
  select(Date, Home.Team, Home.shots, Home.Points, 
         Away.Team, Away.shots, Away.Points, 
         Loser, Choke, Season)

seasons_by_team <- match_history_chokes %>% 
  select(Season, Team = Home.Team) %>% 
  distinct() %>% 
  group_by(Team) %>% 
  summarise(Seasons = n())


seasons_by_team_afl <- match_history_chokes %>% 
  filter(Date > '1990-01-01') %>% 
  select(Season, Team = Home.Team) %>% 
  distinct() %>% 
  group_by(Team) %>% 
  summarise(Seasons = n())


# All time
all_time_choke <- match_history_chokes %>% 
  filter(Choke) %>% 
  count(Loser) %>% 
  arrange(desc(n))

# All time average per season
all_time_choke_ratio <- all_time_choke %>% 
  left_join(seasons_by_team, by = c('Loser' = 'Team')) %>% 
  mutate(Avg.Chokes.Season = n/Seasons) %>% 
  arrange(desc(Avg.Chokes.Season))

# AFL
afl_choke <- match_history_chokes %>% 
  filter(Choke & Date > '1990-01-01') %>% 
  count(Loser) %>% 
  arrange(desc(n))

afl_choke_ratio <- afl_choke %>% 
  left_join(seasons_by_team_afl, by = c('Loser' = 'Team')) %>% 
  mutate(Avg.Chokes.Season = n/Seasons) %>% 
  arrange(desc(Avg.Chokes.Season))

# Saves
data_list <- 
  list(
    match_history_chokes,
    all_time_choke,
    all_time_choke_ratio,
    afl_choke,
    afl_choke_ratio
  )

saveRDS(data_list, 'OUT/chokers/all_time_chokes.RDS')

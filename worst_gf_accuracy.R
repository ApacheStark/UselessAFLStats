# WORST GF KICKING ACC
library(fitzRoy)
library(tidyverse)
library(lubridate)

afttable <- fitzRoy::get_afltables_stats()

afl_gf <- afttable %>% 
  filter(Round == 'GF') %>% 
  mutate(Shots = Goals + Behinds,
         Score.Potential = Shots * 6,
         Score.Deficit = (Goals * 6 + Behinds) - Score.Potential) %>% 
  select(Season, Home.team, Home.score, Away.team, Away.score,
         First.name, Surname, Playing.for, Goals, Behinds, Shots, 
         Score.Potential, Score.Deficit) %>% 
  arrange(Score.Deficit) %>% 
  distinct()

afl_gf2 <- afl_gf %>% mutate(
  Losing.Team = case_when(
    Home.score > Away.score ~ Away.team,
    Away.score > Home.score ~ Home.team,
    TRUE ~ 'Draw'
  ),
  Margin = abs(Home.score - Away.score),
  Miss.Out = case_when(
    Losing.Team == Playing.for & Margin < abs(Score.Deficit) ~ TRUE,
    TRUE ~ FALSE
  )
)

afl_gf2 %>% 
  filter(Miss.Out) %>% 
  arrange(Margin) %>% 
  View

afl_gf2 %>% 
  filter(Miss.Out) %>% 
  filter(Season == 1966)



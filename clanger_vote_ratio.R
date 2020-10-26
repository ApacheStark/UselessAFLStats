# learn from your mistakes
# most brownlow votes per clanger
library(fitzRoy)
library(tidyverse)
library(lubridate)

afttable <- fitzRoy::get_afltables_stats(start_date = '1990-01-01')

afttable_tbl <- afttable %>% 
  as_tibble() %>% 
  mutate(Name = paste(First.name, Surname)) 

vote_players <- afttable_tbl %>% 
  group_by(ID, Name ,Brownlow.Votes) %>%
  summarise(Total.Votes = sum(Brownlow.Votes, na.rm = TRUE)) %>% 
  filter(Total.Votes >= 100) %>% 
  select(ID, Name) %>% 
  distinct()

player_votes <- afttable_tbl %>% 
  select(ID,Name,Brownlow.Votes) %>% 
  filter((ID %in% vote_players$ID)) %>% 
  group_by(ID,Name) %>% 
  summarise(Total.Votes = sum(Brownlow.Votes, na.rm = TRUE)) %>% 
  arrange(desc(Total.Votes))

player_clangers <- afttable_tbl %>% 
  select(ID,Name,Clangers) %>% 
  filter((Name %in% vote_players$Name)) %>% 
  group_by(ID,Name) %>% 
  summarise(Total.Clangers = sum(Clangers)) %>% 
  arrange(desc(Total.Clangers))

vote_clanger_ratio <- player_votes %>% 
  left_join(player_clangers %>% select(-Name), by = 'ID') %>% 
  mutate(Clanger.Vote.Ratio = Total.Clangers/Total.Votes) %>% 
  arrange(desc(Clanger.Vote.Ratio))

vote_clanger_ratio

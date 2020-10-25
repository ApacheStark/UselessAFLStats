library('fitzRoy')

library(tidyverse)

afttable <- fitzRoy::get_afltables_stats()

afttable_df <- afttable %>% as_tibble()

brownlow_votes <- afttable_df %>% 
  select(First.name,Surname,Brownlow.Votes) %>% 
  filter(Brownlow.Votes>0) %>% 
  mutate(First.init = toupper(str_sub(First.name, 1, 1)),
         Second.init = toupper(str_sub(Surname, 1, 1)))

initials_votes <- brownlow_votes %>% 
  group_by(First.init, Second.init) %>% 
  summarise(Total.votes = sum(Brownlow.Votes, na.rm = TRUE)) %>% 
  arrange(desc(Total.votes))

bottom_initials <- initials_votes %>% 
  arrange(Total.votes)

top_names <- afttable_df_sub %>% 
  filter(First.init == initials_votes$First.init[1] &
           Second.init == initials_votes$Second.init[1]) %>% 
  group_by(First.name, Surname) %>% 
  summarise(Total.votes = sum(Brownlow.Votes, na.rm = T)) %>% 
  arrange(desc(Total.votes))

second_names <- afttable_df_sub %>% 
  filter(First.init == initials_votes$First.init[2] &
           Second.init == initials_votes$Second.init[2]) %>% 
  group_by(First.name, Surname) %>% 
  summarise(Total.votes = sum(Brownlow.Votes, na.rm = T)) %>% 
  arrange(desc(Total.votes))

third_names <- afttable_df_sub %>% 
  filter(First.init == initials_votes$First.init[3] &
           Second.init == initials_votes$Second.init[3]) %>% 
  group_by(First.name, Surname) %>% 
  summarise(Total.votes = sum(Brownlow.Votes, na.rm = T)) %>% 
  arrange(desc(Total.votes))

bottom_names <- afttable_df_sub %>% 
  filter(First.init == bottom_initials$First.init[1] &
           Second.init == bottom_initials$Second.init[1] | 
           First.init == bottom_initials$First.init[2] &
           Second.init == bottom_initials$Second.init[2] | 
           First.init == bottom_initials$First.init[3] &
           Second.init == bottom_initials$Second.init[3] | 
           First.init == bottom_initials$First.init[4] &
           Second.init == bottom_initials$Second.init[4] | 
           First.init == bottom_initials$First.init[5] &
           Second.init == bottom_initials$Second.init[5] | 
           First.init == bottom_initials$First.init[6] &
           Second.init == bottom_initials$Second.init[6] | 
           First.init == bottom_initials$First.init[7] &
           Second.init == bottom_initials$Second.init[7]) %>% 
  group_by(First.name, Surname) %>% 
  summarise(Total.votes = sum(Brownlow.Votes, na.rm = T)) %>% 
  arrange(desc(Total.votes))

data_list <- list(
  'Votes_Data' = brownlow_votes,
  'Top Initials' = initials_votes,
  'Bottom Initials' = bottom_initials,
  'Top Names' = top_names,
  '2nd Top Names'=  second_names,
  '3rd Top Names'= third_names,
  '1 Vote Initials' = bottom_names
)

saveRDS(data_list,
        'OUT/initials_brownlow/brownlow_initial_votes.RDS')

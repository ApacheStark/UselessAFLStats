# Best and worst colours of grand finals
library(fitzRoy)
library(tidyverse)

match_history <- fitzRoy::get_match_results()

match_history_df <- match_history %>% 
  as_tibble() %>% 
  filter(Round == 'GF') %>% 
  arrange(desc(Date))


teams <- c(unique(match_history_df$Home.Team),
            unique(match_history_df$Away.Team))
colour_df <- tibble(team = sort(unique(teams)),
                    col1 = c('red', 'red', 'blue', 'black', 
                             'red', 'red', 'red', 'purple','blue',
                             'white','brown', 'red', 'blue', 'black',
                             'black', 'red', 'red', 'blue'),
                    col2 = c('blue', 'yellow', NA, 'white',
                             'black', 'blue', 'blue', 'white','white',
                             'orange','yellow', 'blue', 'white', 'green',
                             'yellow', 'black', 'white', 'yellow'),
                    col3 = c('yellow', 'blue', NA, NA, NA, 'white', 'white',
                             NA, NA, 'grey', NA, NA, NA, 'white', NA, 'white',
                             NA, 'white'))

colour_df2 <- tibble(team = NA, colour = NA)

for (i in 1:nrow(colour_df)) {
  for (j in 2:4) {
    if (!is.na(colour_df[i,j])) {
      colour_df2 <- colour_df2 %>% 
        add_row(team = colour_df[i,1][[1]],
                colour = colour_df[i,j][[1]])
    }
  }
}

colour_df2 <- colour_df2 %>% filter(!is.na(team))

gf_winners_losers <- match_history_df %>% 
  mutate(winner = case_when(Home.Points > Away.Points ~ Home.Team,
                            Away.Points > Home.Points ~ Away.Team),
         loser = case_when(Home.Points < Away.Points ~ Home.Team,
                          Away.Points < Home.Points ~ Away.Team))

gf_winners_losers %>% 
  count(winner) %>% 
  left_join(colour_df2, by = c('winner' = 'team')) %>% 
  group_by(colour) %>% 
  summarise(total.wins = sum(n)) %>% 
  arrange(desc(total.wins))

gf_winners_losers %>% 
  count(loser) %>% 
  left_join(colour_df2, by = c('loser' = 'team')) %>% 
  group_by(colour) %>% 
  summarise(total.loses = sum(n)) %>% 
  arrange(desc(total.loses))





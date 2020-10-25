# Win/Loss Heatmap
library(fitzRoy)
library(tidyverse)
library(lubridate)
match_history <- fitzRoy::get_match_results()

filter_date <- '2000-01-01'

match_wins <- match_history %>% 
  mutate(Home.Team = case_when(
    str_trim(Home.Team) == 'Footscray' ~ 'WB',
    T ~ Home.Team
  ),
  Away.Team = case_when(
    str_trim(Away.Team) == 'Footscray' ~ 'WB',
    T ~ Away.Team
  )) %>% 
  mutate(Winner = case_when(Home.Points > Away.Points ~ Home.Team,
                             Away.Points > Home.Points ~ Away.Team),
        Loser = case_when(Home.Points < Away.Points ~ Home.Team,
                            Away.Points < Home.Points ~ Away.Team),
        Draw = ifelse(is.na(Winner) & is.na(Loser), TRUE, FALSE)) %>% 
  filter(!(Home.Team %in% c('University', 'Fitzroy')) & 
           ! Away.Team%in% c('University', 'Fitzroy') *
           !is.na(Winner) & !is.na(Loser) &
           Date > filter_date) 


match_played <- match_wins %>% 
  count(Home.Team, Away.Team)

match_played2 <- match_wins %>% 
  count(Home.Team, Away.Team) %>% 
  rename(Away.Team=Home.Team,
         Home.Team=Away.Team,
         n2=n)

match_played_spread <- match_played %>% 
  left_join(match_played2) %>% 
  mutate(Total = n + n2) %>% 
  select(Home.Team, Away.Team, Total) %>% 
  distinct() %>% 
  spread(key = Away.Team, value = Total)


match_win_mat <- match_wins %>% 
  count(Winner, Loser) %>% 
  spread(key = Loser, value = n)


match_win_percent <- tibble(Winner = match_win_mat$Winner[!is.na(match_win_mat$Winner)]) %>% 
  cbind(as_tibble(match_win_mat[1:(nrow(match_win_mat)),2:(ncol(match_win_mat))]/match_played_spread[,2:ncol(match_played_spread)])) %>% 
  as_tibble()

winner_order <- sort(unique(abbreviate(match_win_percent_long$Winner)))
loser_order <- rev(winner_order)
match_win_percent_long <- match_win_percent %>% 
  gather(key = Loser, value = Percent, -Winner) %>% 
  mutate(WinnerAbbrv = factor(abbreviate(Winner), levels = loser_order),
         LoserAbbrv = factor(abbreviate(Loser), levels = winner_order),
         Perc_lab = case_when(
           !is.na(Percent) ~ paste0(round(100*Percent,0),'%'),
           TRUE ~ ''))

ggplot(match_win_percent_long, aes(x = LoserAbbrv, y = WinnerAbbrv, fill = 100*Percent)) +
  geom_tile(col = '#454545') +
  geom_text(aes(x = LoserAbbrv, y = WinnerAbbrv, label = Perc_lab)) +
  scale_fill_gradientn(colours = c('#e92211','white','#1199ea'),
                       values = c(0,0.5,1)) +
  labs(
    fill = 'Win Percentage',
    title = 'AFL - Winning Percentage Heatmap',
    subtitle = glue::glue('From {year(filter_date)} to present'),
    x = 'Losing Team',
    y = 'Winning Team'
  ) +
  theme_minimal()











# GAZ Highest avg at each ground
library(fitzRoy)
library(tidyverse)
library(lubridate)

afttable <- fitzRoy::get_afltables_stats(start_date = '2001-01-01')

gaz <- afttable %>% 
  filter(ID == 1105) %>% 
  select(ID, Date, First.name, Surname, Kicks, Handballs, Playing.for, Venue) %>% 
  mutate(Disposals = Kicks + Handballs) %>% 
  arrange((Date))

gaz %>% 
  group_by(Venue) %>% 
  summarise(Games.Played = n(),
            Avg.Disposals = mean(Disposals))  %>% 
  arrange(desc(Avg.Disposals))

gaz50 <- gaz[50:nrow(gaz),]


gaz50 %>% 
  group_by(Venue) %>% 
  summarise(Games.Played = n(),
            Avg.Disposals = mean(Disposals))  %>% 
  arrange(desc(Avg.Disposals))

gaz100last <- gaz[(nrow(gaz)-100):nrow(gaz),]

gaz100last %>% 
  group_by(Venue) %>% 
  summarise(Games.Played = n(),
            Avg.Disposals = mean(Disposals))  %>% 
  arrange(desc(Avg.Disposals))


gas125to250 <- gaz[125:250,]
gas125to250 %>% 
  group_by(Venue) %>% 
  summarise(Games.Played = n(),
            Avg.Disposals = mean(Disposals))  %>% 
  arrange(desc(Avg.Disposals))


ggplot(gaz %>% mutate(Game.No = 1:n()), 
       aes(x = Game.No, y = Disposals, col = Playing.for)) +
  geom_point() + 
  geom_smooth() +
  geom_line(alpha = 0.2) +
  scale_color_manual(values = c('#112277', '#dd3311')) +
  theme_bw() +
  labs(
    title = 'Gary Ablett Jnr Disposals per Game'
  )

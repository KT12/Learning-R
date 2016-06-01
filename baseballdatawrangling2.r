library(dplyr)
library(ggplot2)

bavg <- inner_join(days, events, by=c('gameid')) %>%
  mutate(Year=substr(date, 1, 4)) %>%
  mutate(Month=substr(date, 5, 6)) %>%
  mutate(Day=substr(date, 7,8)) %>%
  mutate(RDate = as.Date(paste(Year, Month, Day, sep='-'))) %>%
  mutate(H = as.numeric(result >= 20 & result <= 23)) %>%
  mutate(AB = as.numeric(AB))

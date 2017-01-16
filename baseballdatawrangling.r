# Baseball Data Wrangling w/ Vagrant, R, and Retrosheet Data

bdat <- inner_join(events, days, by='gameid') %>%
  mutate(HR=as.numeric(result==23)) %>%
  mutate(Year=substr(date, 1, 4)) %>%
  mutate(Month=substr(date, 5, 6)) %>%
  mutate(Day=substr(date, 7,8)) %>%
  mutate(RDate = as.Date(paste(Year, Month, Day, sep='-')))

luzinski <- bdat %>%
  filter(batterid=='luzig001') %>%
  select(HR, RDate) %>%
  arrange(RDate) %>%
  mutate(Total_HR=cumsum(HR)) %>%
  mutate(name='Greg Luzinski')

schmidt <- bdat %>%
  filter(batterid=='schmm001') %>%
  select(HR, RDate) %>%
  arrange(RDate) %>%
  mutate(Total_HR=cumsum(HR)) %>%
  mutate(name='Mike Schmidt')

ggplot()+
  geom_line(data=luzinski, aes(x=RDate, y=Total_HR,color=name))+
  geom_line(data=schmidt, aes(x=RDate, y=Total_HR,color=name))+
  xlab('Date')+
  ylab('Home Runs')+
  ggtitle('Schmidt/Luzinski 1975 HR Race')
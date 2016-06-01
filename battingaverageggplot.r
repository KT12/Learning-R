# For use with baseballdatawrangling2.r

ggplot()+
  geom_line(data=dykstra,aes(x=RDate, y=AVG, color=player)) +
  geom_line(data=murray,aes(x=RDate, y=AVG, color=player)) +
  geom_line(data=brett,aes(x=RDate, y=AVG, color=player)) +
  xlab('Date') +
  ylab('Batting Average') +
  ggtitle('Batting Average Leaders, 1990') +
  geom_hline(yintercept = .4)
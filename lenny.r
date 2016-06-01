# For use with baseballdatawrangling2.r

dykstra <- bavg %>%
  filter(batterid=='dyksl001') %>%
  select(H,AB,RDate) %>%
  arrange(RDate) %>%
  mutate(Total_H=cumsum(H), Total_AB=cumsum(AB)) %>%
  mutate(AVG = round((Total_H / Total_AB),3)) %>%
  mutate(player='Lenny Dykstra')
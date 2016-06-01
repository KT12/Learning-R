library(dplyr)
library(Lahman)

#bdat <- Batting%>%
#    filter(AB > 500 & (HR >= 50 | SO < 25)) %>%      
#    select(playerID, HR, yearID) %>%
#    arrange(desc(HR))

#bdat <- Batting%>%
#  group_by(playerID) %>%
#  summarize(Career_HR = sum(HR,na.rm=TRUE)) %>%
#  arrange(desc(Career_HR))

#bdat <- Batting %>%
#  group_by(playerID) %>%
#  summarize(avg_season_H = round(mean(H, na.rm=TRUE),2)) %>%
#  arrange(desc(avg_season_H))

#bdat <- Batting %>%
#  group_by(playerID) %>%
#  summarize(max_career_hr = max(HR), min_career_SO = min(SO)) %>%
#  arrange(desc(max_career_hr))

#bdat <- Batting %>%
#  group_by(playerID) %>%
#  summarize(number_of_records = n())

#bdat <- Batting %>%
#  filter(AB >= 400) %>%
#  group_by(playerID) %>%
#  summarize(min_career_SO = min(SO, na.rm=TRUE)) %>%
#  filter(min_career_SO < 20) %>%
#  arrange(min_career_SO)

#bdat <- Batting %>%
#  filter(AB >= 400) %>%
  # can't use select() and do calculation there
#  mutate(BA = round(H/AB,3)) %>%
#  select(BA, playerID, yearID) %>%
#  arrange(desc(BA))

# Career BA Section 4 - 16
#bdat <- Batting %>%
#  group_by(playerID) %>%
#  summarize(career_H = sum(H,na.rm=TRUE), career_AB = sum(AB,na.rm=TRUE)) %>%
#  filter(career_AB > 1000) %>%
#  mutate(career_BA = round(career_H / career_AB, 4)) %>%
#  select(playerID, career_BA) %>%
#  arrange(desc(career_BA))

# Inner Joins Section 4 - 17

bdat <- inner_join(Batting, Master,by=c('playerID')) %>%
  filter(playerID=='ruthba01' | playerID=='aaronha01')

# Alternate way to do inner join  
bdat2 <- Batting %>%
  filter(playerID=='ruthba01' | playerID=='aaronha01')

bdat2 <- inner_join(bdat2, Master, by=c('playerID'))

# Query with inner join Section 4 - 18
bdat3 <- Batting %>%
  select(playerID, teamID, yearID, HR)

bdat3 <- inner_join(bdat3, Master, by=c('playerID')) %>%
  select(Given=nameFirst, Family=nameLast, teamID, yearID, HR)

# Joining on more than one field in dplyr

bdat4 <- Batting %>%
  filter(playerID=="ruthba01") %>%
  select(playerID, teamID, yearID, HR)

bdat4 <- inner_join(bdat4, Teams, by=c('teamID', 'yearID')) %>%
  select(playerID, name, yearID, HR.x)

#since both Batting and Teams have HR's, when they are merged, 
# HR.x and HR.y are the two new variables


# Joining three tables together Section 4 - 23

bdat5 <- Batting %>%
  filter(playerID=="ruthba01") %>%
  select(playerID, teamID, yearID, HR)

bdat5 <- inner_join(bdat5, Master, by=c("playerID")) %>%
  select(nameFirst, nameLast, teamID, yearID, HR)

bdat5 <- inner_join(bdat5, Teams, by=c("teamID", "yearID")) %>%
  select(Given=nameFirst, Family=nameLast, name,yearID, HR.x)

# Grouping and Joining two tables Section 4 - 25
bdat6 <- Batting %>%
  group_by(playerID) %>%
  summarize(career_HR = sum(HR,na.rm=TRUE))

bdat6 <- inner_join(bdat6, Master, by=c("playerID")) %>%
  select(nameFirst, nameLast, career_HR)

# Alternate method
bdat <- inner_join(Batting, Master, by=c('playerID')) %>%
  group_by(playerID) %>%
  summarize(Given=nameFirst[1], Family=nameLast[1], career_HR=sum(HR, na.rm=TRUE)) %>%
  select(Given, Family, career_HR)

# Find list of players who's home park was Petco in the Batting table
Petco <- Teams %>% filter(park=='Petco Park')
plist <- inner_join(Petco, Batting, by=c('teamID', 'yearID'))
names <- inner_join(plist, Master, by=c('playerID')) %>%
  group_by(playerID) %>% summarize(Given=nameFirst[1], Family=nameLast[1]) %>% select(Given, Family)

# Professor's solution
Petco <- Teams %>%
  filter(park=='Petco Park') %>% select(teamID, yearID)
Bats <- Batting %>%
  filter(teamID=='SDN', yearID>=2004, yearID<= 2014) %>% select(playerID)
Names <- inner_join(Master, Bats, by=c('playerID')) %>%
  group_by(playerID) %>% summarize(Given=nameFirst[1], Family=nameLast[1]) %>% select(Given,Family)

# List all players named Bob who averaged more than $1e6 salary for their career

# Find all Bob's
Bobs <- Master %>%
  filter(nameFirst=='Bob') %>%
  select(nameFirst, nameLast, playerID)

# Find all avg salaries > 1e6
AvSal <- Salaries %>%
  group_by(playerID) %>%
  summarize(sal = round(mean(salary, na.rm=TRUE))) %>%
  filter(sal > 1000000)

# Inner join the two together
Names <- inner_join(Bobs, AvSal, by='playerID') %>%
  select(Given=nameFirst, Family=nameLast, Average_Salary=sal)

###
# Professor's solution
sal <- Salaries %>%
  group_by(playerID) %>%
  summarize(avg_career_salary=mean(salary)) %>%
  filter(avg_career_salary > 1000000)

names <- inner_join(sal, Master, by=c('playerID')) %>%
  filter(nameFirst=='Bob') %>%
  select(nameFirst, nameLast)


#### Titanic data
titanic_data <- titanic %>%
  group_by(sex) %>%
  summarize(percent_surviving = mean(survived))
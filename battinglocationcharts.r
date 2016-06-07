library(ggplot2)
library(RMySQL)
library(pitchRx)

# Start CMD
# Go to correct directory C:/Hitting Charts/
# vagrant up
# vagrant ssh

# Connect to MySQL
#
#
#
### Insert username and password into code below ###
#
#

mydb <- dbConnect(MySQL(),dbname='baseball',host='localhost',port=3307)

# Scrape innings data for month of June 2015
scrape(start='2015-06-01',end='2015-06-30',suffix='inning/inning_hit.xml',connect=mydb)

# Scrape player data
scrape(start='2015-06-01',end='2015-06-30',suffix='players.xml',connect=mydb)

# Go back into MySQL and make sure the data was saved correctly in the DB

# Pull batted ball outcomes and locations from DB
bdat <- dbGetQuery(mydb,'SELECT x,y,des FROM hip WHERE batter=120074')

# Store batted ball outcomes into temp column
# Make changes outside dataframe

temp <- bdat$des

# Create vectors of indices where batted ball outcomes were hits
single <- which(temp=='Single')
double <- which(temp=='Double')
triple <- which(temp=='Triple')
homerun <- which(temp=='Home Run')

# Combine hit vectors into a hit index
indices <- c(single, double, triple, homerun)

# Assign 'no hit' to batted balls not in the hit index
temp[-indices] <- 'No hit'

# Assign temp to bdat dataframe
bdat$des2 <- temp

# Turn des2 into factor
# This will allow for easier labelling in ggplot
bdat$des2 <- factor(bdat$des2, levels=c('No hit', 'Single', 'Double', 'Home Run'))

ggplot() +
  geom_point(data=bdat, aes(x=x,y=y, color=des2, size=des2, shape=des2)) +
  coord_equal() + # x and y axes have same length
  # clear axes since x,y location measurements vary by park
  scale_x_continuous(breaks=NULL, name='') +
  scale_y_reverse(breaks=NULL, name='') +
  # set sizes for display
  scale_size_manual(values=c(1,4,6,9)) +
  scale_color_manual(values=c('red','green','blue','purple'))
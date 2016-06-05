library(pitchRx)

# get data from the no hitter game
bdat <- scrape(game.ids='gid_2015_06_20_pitmlb_wasmlb_1')

# get the at bat and pitch data from the scraped data

atbat <- bdat$atbat
pitch <- bdat$pitch

# inner join the atbat and pitch dataframes

nh <- inner_join(atbat, pitch, by='num') %>%
  filter(inning_side.x=='top') %>%
  select(num,start_tfs,stand,event,inning.x,batter_name, des, tfs,start_speed,px,pz,pitch_type)

x <- c(-0.95, 0.95, 0.95, -0.95, -0.95)
z <- c(1.6,1.6,3.5,3.5,1.6)
sz <- data.frame(x,z)

# plot the strikezone
ggplot()+
  # set up strike zone square
  geom_path(data=sz, aes(x=x,y=z)) +
  # len of units on x axis same as y axis
  coord_equal() + 
  xlab('feet from home plate') +
  ylab('feet above the ground') +
  # plot the pitches
  geom_point(data=nh, aes(x=px, y=pz,size=start_speed,color=pitch_description)) +
  # smallest points for slower pitches are too small, adjust scale
  # scale means size vs regular dot
  scale_size(range=c(2,4)) +
  # set range for different 'degrees' on color wheel with h
  # setting stauration by c=
  # set brightness/luminence with l=
  ### scale_color_hue(h=c(0,10),c=50, l=0)
  
  # can also set color palette
  ### scale_color_brewer(palette = 'Dark2')
  
  # can also set colors manually
  # check colors by using colors() or hexadecimals
  scale_color_manual(values=c('red','blue','green','#FFFF99','purple')) +

  # If using pitch types, must change pitch_description to factor vector
  # from a chr vector
  ### nh$pitch_description <- factor(nh$pitch_description, levels=c('Fastball', 'Cut fastball', 'Slider', 'Curveball', 'Change-up'))

  # want to see platoon splits
  facet_wrap(~stand)


# creating better pitch labels
temp <- nh$pitch_type
temp[which(temp=='FF')] <- 'Fastball'
temp[which(temp=='CU')] <- 'Curveball'
temp[which(temp=='CH')] <- 'Change-up'
temp[which(temp=='SL')] <- 'Slider'
temp[which(temp=='FF')] <- 'Fastball'
temp[which(temp=='FC')] <- 'Cut fastball'

nh$pitch_description <- temp

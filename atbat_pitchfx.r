bdat <- scrape(game.ids='gid_2015_06_20_pitmlb_wasmlb_1')

# get the at bat and pitch data from the scraped data

atbat <- bdat$atbat
pitch <- bdat$pitch

# inner join the atbat and pitch dataframes

nh <- inner_join(atbat, pitch, by='num') %>%
  filter(inning_side.x=='top') %>%
  select(num,start_tfs,stand,event,inning.x,batter_name, des, tfs,start_speed,px,pz,pitch_type)

# sort nh by tfs (time pitch was thrown)
nh <- nh %>% arrange(tfs)

# enumerate pitches by atbat
nh <- nh %>% arrange(tfs)

temp <- nh %>% group_by(num) %>% summarize(num_pitches = n())

nh$pitch_enum <- unlist(lapply(temp$num_pitches,seq))

x <- c(-0.95, 0.95, 0.95, -0.95, -0.95)
z <- c(1.6,1.6,3.5,3.5,1.6)
sz <- data.frame(x,z)

# creating better pitch labels
temp <- nh$pitch_type
temp[which(temp=='FF')] <- 'Fastball'
temp[which(temp=='CU')] <- 'Curveball'
temp[which(temp=='CH')] <- 'Change-up'
temp[which(temp=='SL')] <- 'Slider'
temp[which(temp=='FF')] <- 'Fastball'
temp[which(temp=='FC')] <- 'Cut fastball'

nh$pitch_description <- temp

des <- nh$des
event <- nh$event
indices <- which(des=='In play, out(s)')
des[indices] <- event[indices]
nh$des2 <- des

colors <- c('red','blue','green','#FFFF99','purple')
names(colors) <- c('Fastball', 'Slider', 'Change-up', 'Curveball', 'Cut fastball')

for(i in unique(nh$num)){
    
    # set the atbat filter by 'num'
    ab <- nh %>% filter(num==i)
    
    # only one batter and inning, take the first data point
    batter <- ab$batter_name[1]
    inning <- ab$inning.x[1]
    
    pitches <- unique(ab$pitch_description)
    
    zmax <- (max(ab$start_speed)-75.4)/22
    zmin <- (min(ab$start_speed)-75.4)/22
    
    stand_xcoord <- ab$stand
    stand_xcoord[which(stand_xcoord=='R')] <- -1.5
    stand_xcoord[which(stand_xcoord=='L')] <- 1.5
    stand_xcoord <- as.numeric(stand_xcoord)
    ab$stand_xcoord <- stand_xcoord
    
    # plot the strikezone
    plot <- ggplot()+
      # set up strike zone square
      geom_path(data=sz, aes(x=x,y=z)) +
      # len of units on x axis same as y axis
      coord_equal() + 
      xlab('feet from home plate') +
      ylab('feet above the ground') +
      # plot the pitches
      geom_point(data=ab, aes(x=px, y=pz,size=start_speed,color=pitch_description)) +
      # smallest points for slower pitches are too small, adjust scale
      # scale means size vs regular dot
      scale_size(range=c(3*zmin+2,3*zmax+2)) +
      # set range for different 'degrees' on color wheel with h
      # setting stauration by c=
      # set brightness/luminence with l=
      ### scale_color_hue(h=c(0,10),c=50, l=0)
      
      # can also set color palette
      ### scale_color_brewer(palette = 'Dark2')
      
      # can also set colors manually
      # check colors by using colors() or hexadecimals
      scale_color_manual(values=colors[pitches]) +
      
      # If using pitch types, must change pitch_description to factor vector
      # from a chr vector
      ### nh$pitch_description <- factor(nh$pitch_description, levels=c('Fastball', 'Cut fastball', 'Slider', 'Curveball', 'Change-up'))
    
      
      # adding text somehwere on the plot
      geom_text(data=ab, aes(label=stand, x=stand_xcoord), y=2.5, size=5) +
      
      xlim(-2,2) + 
      ylim(0,5) +
      ggtitle(paste('Inning ', inning, ': ', batter,sep='')) +
      # add in descriptions
      geom_text(data=ab, aes(label=des2, x=px, y=pz), vjust=1) +
      geom_text(data=ab, aes(label=pitch_enum, x=px, y=pz), vjust=3)
      
    ggsave(paste('atbat',i,'.png',sep=''), plot)

}
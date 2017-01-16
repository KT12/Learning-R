# Looking at Titanic data through R

# File in C:/R/Titanic.csv

# Import dplyr package
library(dplyr)

# Change working directory
setwd('C:/R')

# Save dataframe from CSV
Titanic <- read.csv('Titanic.csv')

head(Titanic)

# Average of survival rate = percentage
Data <- Titanic %>%
  group_by(sex) %>%
  summarize(percent_surviving=mean(survived))

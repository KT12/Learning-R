# R, Yelp and the Search for Good Indian Food

# Explore the `reviews` data eset with `summary()` 
summary(reviews)

# Explore the `users` data set with `summary()` 
summary(users)

# Explore the `businesses` data set with `summary()` 
summary(businesses)


# Make dplry package avaiable to use
library(dplyr)

# Combine the reviews and users data sets
ru  <- inner_join(reviews, users)

# combine the newly created data set with the businesses data set
rub <- inner_join(ru, businesses)

# Take a look at the combined data frame
summary(rub)


# Create indian review column
rub$is_indian <- grepl("Indian", rub$categories) == TRUE

# Select only reviews for Indian restaurants
indian <- subset(rub, is_indian == TRUE)


# The package dplyr is available ot use
# Generate a new data frame with the number of reviews by each reviewer
number_reviews_indian <- indian %>% 
  select(user_id,user_name) %>%
  group_by(user_id) %>% 
  summarise(total_reviews = n())

# Print the table of total_reviews
table(number_reviews_indian$total_reviews)

# Pring the average number of reviews per users
mean(number_reviews_indian$total_reviews)


# The package dplyr is available to use
# Combine number of Indian reviews with original data frame of Indian restaurant reviews
indian_plus_number <- inner_join(indian,number_reviews_indian)

# Display column names for the new data frame
names(indian_plus_number)


# Generate weighted_stars variable 
indian_plus_number$weighted_stars <- indian_plus_number$stars * indian_plus_number$total_reviews

# Create a new weighted review for each restaurant (Note: package dplyr is available to use)
new_review_indian <- indian_plus_number %>% 
  select(city, business_name, avg_stars, stars, total_reviews, weighted_stars) %>%
  group_by(city, business_name, avg_stars) %>%
  summarize(cnt = n(),
            avg = sum(stars) / cnt,
            new = sum(weighted_stars) / sum(total_reviews),
            diff = new - avg)


# Load the ggplot2 package into the environment
library(ggplot2)

# Plot the distribution of changes to reviews 
hist(new_review_indian$diff, main = "Changes in Star Reviews", xlab = "Change")

# Plot the changes in review per restaurant 
ggplot(new_review_indian, aes(x=1:nrow(new_review_indian), y=diff, fill=city)) +
    geom_bar(stat="identity", position=position_dodge()) + 
    theme_classic() + scale_fill_grey() + xlab("Businesses ID") + ylab("Change in Star Review")

# Display a summary of the 
summary(new_review_indian)


# Section 2 - Authenticity Ratings



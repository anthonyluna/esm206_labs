#---------------------------------------
# ESM 206 Lab 1
# Basic data import, wrangling, & viz
# Anthony Luna
#---------------------------------------


#---------------------------------------
# Attach Required Packages
#---------------------------------------


# Attach the tidyverse

library(tidyverse)


#---------------------------------------
# Read in the hp_aggression.csv
#---------------------------------------

# Use readr::read_csv()

hp_data <- read_csv("hp_aggression.csv")


#---------------------------------------
# Checking out the data
#---------------------------------------

# Open Data in a new tab
View(hp_data)

# Use names to see the column headers (variable names)
names(hp_data)

# Use summary() to get a quick summary of my data (classes, summary statistics, na, etc.)
summary(hp_data)

# Use head() or tail() to see the beginning or end of the data
head(hp_data)
tail(hp_data)

#---------------------------------------
# dplyr::select() to create subsets of the df by columns
#---------------------------------------

hp_ex_1 <- select(hp_data, character, book)

# Now let's do the same using the pipe operator: %>%
# Shortcut ctrl + shift + M

hp_ex_2 <- hp_data %>% 
  select(character,book)

# Example of selecting multiple sequential columns ':'
  
hp_ex_3 <- hp_data %>% 
  select(abb:aggressions)
  
hp_ex_4 <- hp_data %>% 
  select(character:aggressions,-book)
  
  
#---------------------------------------
# dplyr:filter() to conditionally subset by rows
#---------------------------------------
  
# Use filter() to set conditions to only keep observations from the book "The Goblet of Fire"

hp_ex_5 <- hp_data %>% 
  filter(book == "The Goblet of Fire")
  
  
# to keep observations that match multiple conditions, the long way ("OR" statement):

hp_ex_6 <- hp_data %>% 
  filter(abb == "harr" | abb == "vold")

# Or use a more concise way: %in%

hp_ex_7 <- hp_data %>% 
  filter(abb %in% c("harr","ronw","vold"))

# What if I only want to keep observations from hp_data where book == 'Deathly Hallows' and the aggressions column is >5


hp_ex_8 <- hp_data %>% 
  filter(book == "The Deathly Hallows", aggressions>5)

# Other operators also work! Like <=, >=, etc.


#---------------------------------------
# dplyr::mutate() to add new columns, while keeping the existing ones
#---------------------------------------


hp_ex_9 <- hp_data %>% 
  mutate(apm = aggressions/mentions)



#---------------------------------------
# dplyr::group_by() + summarize() to find summary statistics by group
#---------------------------------------


hp_ex_10 <- hp_data %>% 
  group_by(book, abb) %>% 
  summarize(
    tot_agg = sum(aggressions),
    max_agg = max(aggressions),
    mean_agg = mean(aggressions)
  )


#---------------------------------------
# Linking multiple wrangling steps with %>% 
#---------------------------------------

# Let's say we only want to keep rows that ontain ovservations for Harry Potter and Voldemort, we then want to only keep columns, for character, book, and mentions, then we want to find the total number of mentions for each character.


hp_ex_11 <- hp_data %>% 
  filter(abb %in% c("vold","harr","herm","snap")) %>% 
  select(character, book, mentions) %>% 
  group_by(character) %>% 
  summarize(
    total = sum(mentions)
  )



#---------------------------------------
# Creating graphs with ggplot2
#---------------------------------------


# Tell R 3 things for a bar minimum graph:
# 1. we're using ggplot 2
# 2. Which data we're using, including what our x/y variables are (as relevant)
# 3. what type of graph (geom) you want to create
#

ggplot(data = hp_ex_11, aes(x = character, y = total)) +
  geom_col() +
  labs(x = "Character",
       y = "Total mentions",
       title = "HP aggresions") +
  coord_flip()
  
# an example of a scatterplot: geom_point()

ggplot(data = hp_data, aes(x = mentions, y=aggressions)) +
  geom_point(aes(color = book)) +
  theme_bw()

# now lets make a historgram!

ggplot(data = hp_data, aes(x=aggressions))+
  geom_histogram()










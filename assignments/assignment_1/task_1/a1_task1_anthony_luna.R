#----------------------------
# Task 1. Global glacial volume loss and sea level rise
# Author: Anthony Luna
# Date: 2019-10-05
#----------------------------

#----------------------------
# Step One 
# Attach required pacakges
#----------------------------

# Attach tidyverse

library(tidyverse)

#----------------------------
# Step two
# Load Data
#----------------------------

# Read raw data (glacial_loss.csv) into object glacial_data

glacial_data <- read_csv("glacial_loss.csv")

# Look at glacial_data

View(glacial_data)

#----------------------------
# Step three
# Manipulate Data
#----------------------------

# Create object glacial_rise from glacial_data 
#
#   1. by selecting two columns year, cumulative,
#
#   2. then filtering the years for 1961 to 1990.
#
# (Steps Annotated below)

glacial_rise <- glacial_data %>% 
  select(year, cumulative_sea_level_rise ) %>% # 1.
  filter(1961 <= year & 1990 >= year) # 2.

# Look at glacial_rise

View(glacial_rise)

#----------------------------
# Step four
# Plot Data
#----------------------------

# Create scatter plot of Cumulative Seal Level Rise over years.

ggplot(data = glacial_rise,aes(x = year,y = cumulative_sea_level_rise)) +
  
# Set dots in plot to blue.

  geom_point(color = 'blue') +

# Change title, subtitle, caption, and x,y-axis labels.

  labs(
    title = 'Sea Level Rise due to Glacial Loss (LUNA)',
    subtitle = 'Years: 1961 to 1990',
    caption = 'Author: Anthony Luna',
    x = 'Year',
    y = 'Cumulative Sea Level Rise') +

# Use the minimal theme.
  
    theme_minimal() +

# Modify the minimal theme such that the title and subtitle are centered.
  
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

#----------------------------
# End of file
#----------------------------
#----------------------------
# TASK 2: Blood lead levels in children, St. Louis, MO (2010 – 2015 data)
# Author: Anthony Luna
# Date: 2019-10-05
#----------------------------

#----------------------------
# Step One
# Attach Required Packages
#----------------------------

# Attach tidyverse

library(tidyverse)

#----------------------------
# Step Two
# Load Data
#----------------------------

# Read raw data (stl_lead.csv) into object stl_lead_data

stl_lead_data <- read.csv('stl_lead.csv')

#----------------------------
# Step Three
# Manipulate Data
#----------------------------

# Create object stl_lead_black_data by 
#
#   1. selecting columns pctElevated, totalPop and black, 
#
#   2. then, adding a column 'pctBlack' which gives the estimated 
#      black population percentage per census tract.
#
# (Steps annotated below)

stl_lead_black_data <- stl_lead_data %>% 
  select( pctElevated, totalPop, black) # 1.
  mutate(pctBlack = (100 * black) / totalPop) # 2.

#----------------------------
# Step Four
# Plot Data
#----------------------------

# Create scatter plot of % of children with elevated blood lead levels 
# with Proportion of black population 

ggplot(data = stl_lead_black_data, aes(x = pctBlack, y = pctElevated)) +

# Set dot color to red and dot size to 3
  
  geom_point(color = 'red', size = 3) +
  
# Change title, subtitle, caption, and xy-axis lables
  
  labs(
    title = 'Children with Elevated Blood Lead Levels in Black communities (LUNA)',
    subtitle = 'Populations Based on 2015 Census Data',
    caption = 'Author: Anthony Luna',
    x = 'Percent of Black People in Census Tract',
    y = 'Percent of Children with Evelated Blood Lead Levels (>5mg/dl)'
    ) +
  
# Use the minimal theme
  
  theme_minimal() +
  
# Modify minimal theme to center title and subtitle

  theme(plot.title = element_text(hjust= 0.5),
        plot.subtitle = element_text(hjust= 0.5)) 

#----------------------------
# Commentary:
#
# Based on the chart produced, we can see that in communities where there is a 
# lower proportion of black people, there tends to be lower rates of children 
# with elevated blood lead levels. In communities where there is  a higher 
# proportion, there is a much higher variance in the rate of blood lead levels,
# and rate is generally higher. 
#
#----------------------------

#----------------------------
# End Of File
#----------------------------
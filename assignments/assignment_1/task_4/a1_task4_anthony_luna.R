#------------------------------------------------------- 
# Title: Task 4 - US Atlantic Salmon Imports 
# Author: Anthony Luna  
# Date: 2019-10-07 
#-------------------------------------------------------

#------------------------------------------------------- 
# Step 1 
# Attach required packages
#-------------------------------------------------------

# Attach tidyverse

library(tidyverse)

#------------------------------------------------------- 
# Step 2 
# Load Data
#-------------------------------------------------------

# Load atl_salmon_imports into object atl_salmon_imports

atl_salmon_imports <- read.csv("atl_salmon_imports.csv")

# Look at the data

View(atl_salmon_imports)

#------------------------------------------------------- 
# Step 3 
# Manipulate Data 
#-------------------------------------------------------

#------------------------------------------------------- 
# Step 3.1 
# Calculate total volume of Atlantic salmon that the US
# has imported from each country (10^3) for the 10 most
# recent years of data
#-------------------------------------------------------

# We calculate the total volume for each country over 
# the last 10 years using atl_salmon_imports by creating 
# atl_salmon_imports_10years with the following steps:
#   
#   1. filter the data for years between 2009 and 
#      2018,
#
#   2. group the data by country, and
#   
#   3. summarize the data with 'total' being equal to 
#      the sum of imported volume and ignoring NA values.
#

atl_salmon_imports_10years_sum <- atl_salmon_imports %>% 
  filter(between(year,2009,2018)) %>% # 1.
  group_by(country) %>% # 2.
  summarize(total = sum(volume, na.rm=TRUE)) # 3.

# Look at the data

View(atl_salmon_imports_10years_sum)

#------------------------------------------------------- 
# Step 3.2 
# Create dataframe with all available import years with
# only the top 5 importers in the last 10 years.
#-------------------------------------------------------

# We use atl_salmon_imports_10years_sum to create a list
# with the top 5, 10 year salmon importers by 
#
#   1. arranging atl_salmon_imports_10years_sum so it is 
#      decending is total, then
#
#   2. taking the top 5 values.

atl_salmon_countries_top5 <-  atl_salmon_imports_10years_sum %>%
  arrange(-total) %>%  # 1.
  head(5) # 2.

# Look at the data

View(atl_salmon_countries_top5)

# Then we can use the country column as a condition to 
# filter atl_salmon_imports which will give us the 
# dataframe we are looking for. For this we use '$' to select
# the country column, and %in% for the conditional.


atl_salmon_imports_top5 <- atl_salmon_imports %>% 
  filter(country %in% atl_salmon_countries_top5$country)

# Look at the data

View(atl_salmon_imports_top5)

#------------------------------------------------------- 
# Step 4 
# Plot Data
#-------------------------------------------------------

# Create line graph of the top 5 salmon importers over time.

ggplot(data = atl_salmon_imports_top5, aes(x = year, y = volume)) +

  # Set line size to 1 and the line color to correspond to the country
  
  geom_line(size = 1,aes(color = country)) + 
  
  # Change title, subtitle, caption, legend, and xy-axis lables. 
  # Using bquote() function in the y label to add a superscript.
  
  labs(
    title = 'Annual Salmon imports from top 5 importers by volume (LUNA)',
    subtitle = 'Volumes based on 2018 USDA ERS Aquaculture data (1989-2018)',
    caption = 'Author: Anthony Luna',
    col = "Country",
    x = 'Year',
    y = bquote('Volume of Salmon imported ('~10^3~ 'pounds)')
  ) +
  
  # Use the minimal theme
  
  theme_minimal() +
  
  # Modify minimal theme to center title and subtitle
  
  theme(plot.title = element_text(hjust= 0.5),
        plot.subtitle = element_text(hjust= 0.5)) 

#------------------------------------------------------- 
# End of File 
#-------------------------------------------------------





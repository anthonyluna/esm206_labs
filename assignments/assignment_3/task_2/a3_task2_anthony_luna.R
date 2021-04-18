#------------------------------------------------------- 
# Title: Task 2 - SPACE LAUNCHES 
# Author: Anthony Luna  
# Date: 2019-10-22 
#-------------------------------------------------------
#------------------------------------------------------- 
# 
# Prolouge: 
#
# With the immenent implementation of Brexit upon the UK
# government, the Prime Minister really needs to get a 
# win. They have asked an analyst at British Intellegence
# to show that they have a STELLAR space program. They are 
# not surprised to find... its not great. The analyst 
# thusly constructs the following graph for his next 
# meeting with the Prime Minister. They've done everything
# they can to keep the awful state of UK space capabilities
# from the Prime minister, and show the might of the 
# vast UK Empire.
#  
#-------------------------------------------------------
#------------------------------------------------------- 
# Part 1
#  Attach the required packages
#-------------------------------------------------------
  

library(tidyverse)
library(janitor)
library(scales)

#------------------------------------------------------- 
# Part 2 
#  Load the Data
#-------------------------------------------------------

space_data <- read_csv("space_launches.csv")

#------------------------------------------------------- 
# Part 3 
#  Manipulate the data
#-------------------------------------------------------
  
# Here we summarize by adding a tally but we are gonna
# make sure to NOT sort the data... this is used later
# for maximal confusion... (See 2. in Part 4)

space_summarized <- space_data %>% 
  group_by(state_code) %>% 
  filter(!is.na(state_code)) %>% 
  tally()

#------------------------------------------------------- 
# Part 4 
#  Create a (misleading and terrible) graph
# 
# Six+ Ways to make a bad graph
#
# 1. Dont speciy axis transformations,
# 2. Don't sort your factors,
# 3. Use awful, visually ambiguous, colors,
# 4. Make VERY long axis labels,
# 5. Emphasize certain results disengeniounsly
# 6. Rotate your factor axis labels upside down
# 
#-------------------------------------------------------

  # Here we use some awful colors (3)

ggplot(space_summarized, aes(x=state_code, y=n)) +
  geom_col(color = "yellow", fill = "#DFDD00") +
  
  # Emphasize the desired result, while making everything else hard to see.(5)
  
  annotate("rect", xmin = 15.5, xmax = 16.5, ymax = 2444 , ymin = 2, fill="white") +
  annotate("text",label = "UK number 1 best!!!", x = 16, y = 4 , fill="white", angle = 90) +
  
  # Here we use a reciprocal transformation on the y-axis to ensure
  # there is much confusion about who is actually the highest
  # ranking (1)
  
  scale_y_continuous(trans = "reciprocal") +
  
  # Here is where the very long axis labels are made.(4)
  
  labs( x = "This is the side of the graph with all of the different country codes",
    y = str_wrap("This is the side of the graph with information about the amount of launches",width=40),
    col = "") + 

  # Here we change the colors so that you can barely see
  # what is actually going on.(3)
  
  theme(plot.background = element_rect(fill='#DFDD00'),
        panel.background = element_rect(fill='#DFDD00'),
        
        # Rotate the axis labels upside down because... 
        # why not! (6)
        
        axis.text.x = element_text(angle = 180))

# Finally the analyst crosses their fingers, saves the graph and
# sends it off to the Prime Minister for review.

ggsave("a3_task2_anthony_luna.png")
  
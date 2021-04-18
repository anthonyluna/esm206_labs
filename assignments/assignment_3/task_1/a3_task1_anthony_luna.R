#------------------------------------------------------- 
# Title: Task 1  
# Author: Anthony Luna  
# Date: 2019-10-21 
#-------------------------------------------------------

#------------------------------------------------------- 
# Citations:
#
# 1. “Mono Basin Clearinghouse: Building a Digital Library 
# for Better Resource Management.” Mono Basin Clearinghouse: 
# Building a Digital Library for Better Resource Management, 
# 1999, Accessed 2019 www.monobasinresearch.org/. 
# 
# 2. “Mono Lake” The Mono Lake Committee, Accessed 2019.
# www.monobasinresearch.org/.
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
#  Load and Clean the data
#-------------------------------------------------------
  
# We will load the data into annual_lake_level Here we 
# skip the first 5 lines 

annual_lake_level <- read_csv("Mono Lake Annual Levels.csv", skip = 5) %>%
  
  # Now we will use clean_names so the headers are 
  # R friendly
  
  clean_names() %>% filter(year >=1900) 

#------------------------------------------------------- 
# Part 3
#  Defining Important historical events with regard to 
#  freshwater flow into Mono Lake
#  
# 1. 1941 - LADWP began diverting the fresh water source 
# feeding Mono Lake
#
# 2. 1978 - the Mono Lake committee is formed in response
# to the falling lake levels
#
# 3. 1984 - The California Supreme court issues a restraining
# order requiring LADWP to release 19cfs of freshwater
# into the lower Rush Creek.
#
# 4. 1994 - Based on a 1993 EIR finding, the Storm Water
# Resources Control Board issued decision 1631 to set the
# lake level to 6932 feet above sea level
#
# We will use these dates to define variables which will
# be used to place lables on our final graph
#
#-------------------------------------------------------

# We use str_wrap to make sure the labels are appropriately formated.

lab_1941 <- str_wrap("1941 - Los Angeles Department of Water and Power (LADWP) began diverting the fresh water source feeding Mono Lake.", width = 20)
lab_1978 <- str_wrap("1978 - The Mono Lake committee is formed in response to the falling lake levels.", width = 20)
lab_1984 <- str_wrap("1984 - The California Supreme court issues a restraining order requiring LADWP to release 19cfs of freshwater into the lower Rush Creek which feeds Mono Lake.", width = 30)
lab_1994 <- str_wrap("1994 - Based on a 1993 Environmental Impact Report finding, the Storm Water Resources Control Board (SWRCB) issued decision 1631 to set the lake level to 6,932 feet above sea level (shown below in blue). This recharge effort is projected to take 20 years.", width = 30)

# Here we create our variables which are the lake level 
# at each given date. Refer to the comment above for 
# historical details.

hist_1941 <- annual_lake_level$
  lake_level_feet_above_sea_level[annual_lake_level$year==1941] #1.
hist_1978 <- annual_lake_level$
  lake_level_feet_above_sea_level[annual_lake_level$year==1978] #2.
hist_1984 <- annual_lake_level$
  lake_level_feet_above_sea_level[annual_lake_level$year==1984] #3.
hist_1994 <- annual_lake_level$
  lake_level_feet_above_sea_level[annual_lake_level$year==1994] #5.


#------------------------------------------------------- 
# Part 4
#  Defining Risk boundaries for local bird populations
# 
# Now we are going to add a column which indicates the 
# bird population risk level:
# 
# 1. New Predator Risk - At 6,377ft above sea level a 
# land bridge will open, allowing new predators to access 
# the island. 
#  
# 2. Beginning food source Reduction - At 6,360ft above 
# sea level, the salinity (120 g/L) will begin to
# reduce brine shrimp population
#  
# 3. Severe food source Reduction - At 6,350ft above 
# sea level, the salinity (150 g/L) will severely
# reduce brine shrimp population
#  
#  We define these levels to include in our graph as filled
#   spaces below the line graph
#-------------------------------------------------------
     
risk_1 <- 6377
risk_2 <- 6360
risk_3 <- 6350

#------------------------------------------------------- 
# Part 5 
#  Define the Lake Level for Decision 1631
#-------------------------------------------------------

decision_level <- 6392

#------------------------------------------------------- 
# Part 6  
#  Graph the data
#-------------------------------------------------------
  
ggplot(annual_lake_level,
       aes(x = year, y = lake_level_feet_above_sea_level))+
  
  # Here we define the ribbons which will show our risk levels for the 
  # island bird populations
  
  geom_ribbon(aes(ymin=6340,ymax=risk_3), fill="#550000", alpha = .5)+
  geom_ribbon(aes(ymin=risk_3,ymax=risk_2), fill="#AA3939", alpha = .5)+
  geom_ribbon(aes(ymin=risk_2,ymax=risk_1), fill="#FFAAAA", alpha = .5)+
  
  # Here we define our decision level line based on the 
  # 1994 SWRCB decieion
  
  geom_segment(aes(x = min(annual_lake_level$year),
                   y = decision_level, 
                   xend = max(annual_lake_level$year),
                   yend = decision_level),
                   color = "blue")+

  # Here we define the lines connecting our points on the line 
  # graph to our labels
  
  geom_segment(aes(x = 1941 , y = hist_1941, xend = 1941, yend = 6440))+
  geom_segment(aes(x = 1978 , y = hist_1978, xend = 1978, yend = 6410))+
  geom_segment(aes(x = 1984 , y = hist_1984, xend = 1984, yend = 6350))+
  geom_segment(aes(x = 1994 , y = hist_1994, xend = 1994, yend = 6410))+
  
  # Here we add the label for the lake descision level.
  
  geom_text(label = "1994 SWRCB Decision Level",
            size = 3,
            aes(x=1900,y=6392),
            nudge_x = 12,nudge_y = 3)+
  
  # Here we add 4 historically significant points
  
  geom_point(aes(x = 1941 , y = hist_1941))+
  geom_point(aes(x = 1978 , y = hist_1978))+
  geom_point(aes(x = 1984 , y = hist_1984))+
  geom_point(aes(x = 1994 , y = hist_1994))+
  
  # Here we add the Risk zone labels
  
  annotate("text",size=3,x=1912,y=c(6368,6355,6345),
           label = c("Bird Population Risk Zone 1",
                     "Bird Population Risk Zone 2",
                     "Bird Population Risk Zone 3"),
           alpha = 1)+
  
  # Here we add the labels for the important historical dates.
  
  annotate("label",
           size=3,
           x=c(1941,1974,1984,1994),
           y=c(6440,6412,6355,6415),
           label = c(lab_1941,lab_1978,lab_1984,lab_1994))+
  
  # Here we expand our plot to fill the whole x-axis
  
  scale_x_continuous(expand= c(0,0))+
  
  # Here we assign our important lake levels to be show on the axis,
  # modify the format, and increase our axis limits so there is 
  # more room for labels.
  
  scale_y_continuous(labels = comma(c(6416,6392,6377,6360,6350)), 
                     breaks = c(6417,6392,6377,6360,6350), 
                     limits = c(6340,6460),
                     expand= c(0,0))+
  
  # Here is the main line graph of our data.
  
  geom_line()+
  
  # Here we updated our title, subtitle, caption and y-axis. 
  
  labs(title = "Mono Lake Water Level (1900-2017)",
    subtitle = "Author: Anthony Luna",
    caption = str_wrap("Labels signify historical dates important to freshwater 
                       inflow at Mono Lake. The Bird population risk zones show 
                       levels at which: a land bridge will allow new predation of 
                       island bird populations (Zone 1), and increasing salinity 
                       of the lake will begin to moderately and severly reduce  
                       populations of brine shrimp - an island bird food source
                       (Zone 2 & 3)",
                       width = 120) ,
    x = "",
    y = "Lake Level (ft above sea level)",
    col = "") + 
  
  # Using the Minimal Theme
  
  theme_minimal() + 
  
  # This eliminates grid lines, readjusts the caption, title and subtitle, and 
  # removes the x axis title and text (we know the dates range from 1900-2017 
  # based on the title, and the important dates are shown in the labels.)
  
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(vjust = -22),
        plot.subtitle = element_text(vjust = -30),
        plot.caption = element_text(hjust=0),      
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", color = "gray"),
        axis.text.x = element_blank(),
        axis.title.x=element_blank()) 

# Finally we save our graph.


ggsave("a3_task1_anthony_luna.png", units = "in", width =11 , height = 8.5)



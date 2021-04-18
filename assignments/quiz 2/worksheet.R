library(tidyverse)
library(janitor)
library(kableExtra)
library(scales)
library(here)
library(ggbeeswarm)
library(car)
library(effsize)

# Question 1 - 5

soil_A <- c(2.4, 3.1, 0.9, 8.6, 5.2, 5.4, 4.7)
soil_B <- c(7.1, 9.2, 4.5, 6.4, 2.9, 11.6, 8.7)

t.test(soil_A,soil_B,alternative = "two.sided")

effsize::cohen.d(soil_A,soil_B)

# Question 6 - 10

wingspan <- function(h,rsh,o){2.56 + 0.28*(h) - 0.9*(rsh) - 1.2*(o)}

wingspan(.9,1,0) # Q7

wingspan(1.4,0,0) # Q8

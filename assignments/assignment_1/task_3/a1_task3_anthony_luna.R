### -------------------------------
### Anthony Luna
### ESM 206 Assignment 1 - Task 3
### Troubleshooting errors in R
### -------------------------------

### The following script has a bunch of errors in it. Your goal is to correct
### everything, so that a summary table and graph is made successfully. It
### will use the existing 'diamonds' dataset in dplyr, so you will not need to
### read in any external data.

### -------------------------------
### Start exercise
### -------------------------------

### -------------------------------
### a. Attach the tidyverse package:
### -------------------------------

library(tidyverse) ### ERROR(S): Capital "T" in tidyverse.

### -------------------------------
### b. Check out the 'diamonds' data in dplyr:
### -------------------------------

View(diamonds) ### Open df in a new tab
?diamonds ### Read the R documentation on the data

### -------------------------------
### c. Do some wrangling to only keep carat, cut, color, clarity and price,
###    then only keep rows for diamonds with high clarities of "VVS2", "VVS1",
###    or "IF"
### -------------------------------

diamond_sub <- diamonds %>%
  select(carat:clarity, price) %>%  ### ERROR(S): '+' was used instead of %>%
  filter(clarity %in% c("VVS2","VVS1","IF")) ### ERROR(S): Capital "F" in filter. No c() function used for the list.

### -------------------------------
### d. For diamonds in the subset created above, make a summary table of the
###    mean price for diamonds, grouped by cut and clarity
### -------------------------------

diamond_summary <- diamond_sub %>%
  group_by(cut, clarity) %>% ### ERROR(S): Column named 'clarity' was misspelled as 'clartiy'
  summarize(
    mean_price = mean(price)) ### ERROR(S): Missing parenthesis for summarize(). Mean function takes column 'price' as argument not string "price" as an argument. 

### Look at the summary table:
View(diamond_summary)### ERROR(S): View() function was not used and the object 'diamond_summary' was misspelled as 'diamond-summary' 

### -------------------------------
### e. Now create a nice ggplot graph of the diamonds data
### -------------------------------

ggplot(diamonds, aes(x = cut, y = price)) + ### ERROR(S): Plot did not generate because it was assigned to an object. aes() function was missing for the variable designation. Function for plot is ggplot(), not ggplot2().
  geom_violin(aes(fill = cut)) + ### ERROR(S): aes() function missing for the variable designation.
  theme_bw() + ### ERROR(S): Function neds open and close parenthesis.
  labs(x = "Diamond cut", y = "Diamond price (USD)") +  ### ERROR(S): Pipe used instead of +
  coord_flip()

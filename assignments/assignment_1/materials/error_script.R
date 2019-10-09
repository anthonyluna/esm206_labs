### -------------------------------
### ADD YOUR NAME HERE
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

library(Tidyverse) ### ERROR(S):

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
  select(carat;clarity, price) + ### ERROR(S):
  Filter(clarity %in% "VVS2","VVS1","IF") ### ERROR(S):

### -------------------------------
### d. For diamonds in the subset created above, make a summary table of the
###    mean price for diamonds, grouped by cut and clarity
### -------------------------------

diamond_summary <- diamond_sub %>%
  group_by(cut, clartiy) %>% ### ERROR(S):
  summarize(
    mean_price = mean("price") ### ERROR(S):

### Look at the summary table:
diamond-summary ### ERROR(S):

### -------------------------------
### e. Now create a nice ggplot graph of the diamonds data
### -------------------------------

diamond_graph <- ggplot2(diamonds, x = cut, y = price) + ### ERROR(S):
  geom_violin(fill = cut) + ### ERROR(S):
  theme_bw + ### ERROR(S):
  label(x = "Diamond cut", y = "Diamond price (USD)") %>%  ### ERROR(S):
  coord_flip()

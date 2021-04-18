# Anthony Luna

library(tidyverse)

avocado <- read_csv("avocado_data.csv")


mean_prices <- avocado %>% 
  filter(region %in% c("Chicago","Portland","Denver")) %>% 
  group_by(region) %>% 
  summarise( mean_price = mean(price_per))


ggplot(data = mean_prices,  aes(x = region, y=mean_price)) +
  geom_col() +
  labs( x = "region",
        y="mean price ($)",
        title = "Anthony Luna")
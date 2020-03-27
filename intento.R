library(tidyverse)

data("diamonds")
diamonds %>% 
  head()
diamonds %>% 
  ggplot(aes(depth, price, colour = cut)) +
  geom_point()

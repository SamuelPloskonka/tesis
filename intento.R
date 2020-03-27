library(tidyverse)
data("diamonds")
diamonds %>% names()
diamonds %>% ggplot(aes(price, depth, colour = cut))+geom_point()
diamonds %>% ggplot(aes(price, depth, colour = cut))+geom_path()


diamonds %>% ggplot(aes(price, depth, colour = cut))+geom_point() +theme_bw()
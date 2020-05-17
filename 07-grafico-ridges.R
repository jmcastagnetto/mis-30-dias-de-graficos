library(tidyverse)
library(ggridges)
#View(diamonds)
ggplot(diamonds, aes(x = price, y = cut, fill = cut)) +
  geom_density_ridges()

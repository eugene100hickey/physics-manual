# https://www.youtube.com/watch?v=aF6upt4va4E

library(tidyverse)
library(ggridges)
library(viridis)

lincoln <- ggridges::lincoln_weather
write_csv(lincoln, "lincoln.csv")

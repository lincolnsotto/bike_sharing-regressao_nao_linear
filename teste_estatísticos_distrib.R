base_bikes <- read_csv("day.csv")
library(tidyverse)

hist(base_bikes$cnt)

qqnorm(base_bikes$cnt) +
qqline(base_bikes$cnt)


shapiro.test(base_bikes$cnt)

glimpse(base_bikes)
summary(base_bikes)
sd(base_bikes$cnt)

rm(list = ls())

getwd()

install.packages("ggmosaic")
library("ggmosaic")

install.packages("epitools")
library("epitools")


library("tidyverse")

tidyverse_update()

#1
model03 <- binom.test(x= 41, n=90, p=0.5, alternative = "greater", conf.level = 0.95 )
model03
#2

rm(list =ls())

getwd()

library("tidyverse")

tidyverse_update()

install.packages("DescTools")
library("DescTools")

library(readr)
O_Data <- read_csv("~/O_Data.csv")
View(O_Data)

Question 1

t.test(O_Data$Obliquity,
       alternative = "two.sided", mu = 23.4722, conf.level = 0.95)

Question 2

library(readr)
H_Data <- read_csv("datasets/demos/HeartAttack_short.csv",col_names = TRUE,
                   col_types = cols(
                     group = col_character() )
)
view(H_Data)

ggplot(H_Data)+
  geom_boxplot(aes(x = group, y = cholest), varwidth = TRUE)

ggplot(H_Data)+
  geom_histogram(aes(cholest), binwidth = 8)+
  facet_wrap(~group)

ggplot(H_Data)+
  geom_qq(aes(sample = cholest, color = group))

summ_H_Data <- H_Data  %>%
  group_by(group) %>% 
  summarise(mean_cholest = mean(cholest),
            sd_cholest = sd(cholest),
            n_cholest = n())
view(summ_H_Data)

ratio <-(max(summ_H_Data$sd_cholest))/(min(summ_H_Data$sd_cholest))

t.test(cholest ~ group, data = H_Data, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

Question 3

library(readr)
Fulmars <- read_csv("datasets/quinn/chpt3/furness.csv")
view(Fulmars)

summ_F_Data <- Fulmars  %>%
  group_by(SEX) %>% 
  summarise(mean_METRATE = mean(METRATE),
            sd_METRATE = sd(METRATE),
            n_METRATE = n())
view(summ_F_Data)

ggplot(Fulmars)+
  geom_boxplot(aes(x = SEX, y = METRATE), varwidth = TRUE)

ggplot(Fulmars)+
  geom_histogram(aes(METRATE), binwidth = 200)+
  facet_wrap(~SEX)

ggplot(Fulmars)+
  geom_qq(aes(sample = METRATE, color = SEX))
  

  wilcox.test(METRATE ~ SEX, data = Fulmars, alternative = "two.sided", conf.level = 0.95)

library(readr)
Spiders <- read_csv("datasets/quinn/chpt3/elgar.csv")
view(Spiders)

MS <- mutate(Spiders, diff = HORIZDIM - HORIZLIG)

ggplot(MS) +
  geom_histogram(aes(diff), binwidth = 20)

ggplot(MS) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(MS)+
  geom_qq(aes(sample = diff))

t.test(MS$HORIZDIM, MS$HORIZLIG, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)

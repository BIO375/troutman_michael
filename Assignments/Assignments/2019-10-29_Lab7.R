rm(list = ls())

getwd()


install.packages("DescTools")
library("DescTools")

install.packages("ggfortify")
library("ggfortify")

install.packages("multcomp")
library("multcomp")

install.packages("nlme")
library("nlme")

library("tidyverse")
tidyverse_update()

library(readr)

JF <- read_csv("~/Analyses/troutman_michael/datasets/demos/Jaffe.csv",
col_types = cols(
  Depth = col_factor() ))
View(JF)

#Aldrin 
ggplot(JF)+
  geom_boxplot(aes(x = Depth, y = Aldrin), varwidth = TRUE)
  ggplot(JF) +
  geom_histogram(aes(Aldrin), binwidth = 0.5)+
  facet_wrap(~Depth)
ggplot(JF)+
  geom_qq(aes(sample = Aldrin, color = Depth))

model01 <- lm(Aldrin~Depth, data = JF)

summ_A <- JF %>%
  group_by(Depth) %>% 
  summarise(mean_Aldrin = mean(Aldrin),
            sd_Aldrin = sd(Aldrin),
            n_Aldrin = n())
ratio <-(max(summ_A$sd_Aldrin))/(min(summ_A$sd_Aldrin))

autoplot(model01)
anova(model01)
#Transformed that data due to non-normality 
JF<-mutate(JF, log_Aldrin = log10(Aldrin))
model03 <- lm(log_Aldrin~Depth, data = JF)
anova(model03)

tukey_A <- glht(model03, linfct = mcp(Depth = "Tukey"))
summary(tukey_A)


#HCB 
ggplot(JF)+
  geom_boxplot(aes(x = Depth, y = HCB), varwidth = TRUE)
ggplot(JF) +
  geom_histogram(aes(HCB), binwidth = 0.3)+
  facet_wrap(~Depth)
ggplot(JF)+
  geom_qq(aes(sample = HCB, color = Depth))

model02 <- lm(HCB~Depth, data = JF)

summ_HCB <- JF %>%
  group_by(Depth) %>% 
  summarise(mean_HCB = mean(HCB),
            sd_HCB = sd(HCB),
            n_HCB = n())
ratio <-(max(summ_HCB$sd_HCB))/(min(summ_HCB$sd_HCB))

autoplot(model02)
anova(model02)

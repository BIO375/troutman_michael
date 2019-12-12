rm(list = ls())

getwd()

install.packages("tidyverse")
library(tidyverse)

tidyverse_update()

install.packages("DescTools")
library("DescTools")

#Question 20

library(readr)
Salmon <- read_csv("datasets/abd/chapter13/chap13q20SalmonColor.csv")
View(Salmon)

summ_S_Data <- Salmon  %>%
  group_by(species) %>% 
  summarise(mean_color = mean(skinColor),
            sd_color = sd(skinColor),
            n_color = n())
view(summ_S_Data)

ggplot(Salmon)+
  geom_boxplot(aes(x = species, y = skinColor), varwidth = TRUE)

ggplot(Salmon)+
  geom_histogram(aes(skinColor), binwidth = .1)+
  facet_wrap(~species)

ggplot(Salmon)+
  geom_qq(aes(sample = skinColor, color = species))

ratio <-(max(summ_S_Data$sd_color))/(min(summ_S_Data$sd_color))
#ratio = 4.207 > 3. 

#Since the variances are not equal, a Welch's t-test should be used.
#Alternatively, the data could be transformed by natural log and t-test

#Welch's Test - I left this in here because I already did it but it should not be used
#in this case because the power is so low.
t.test(skinColor ~ species, data = Salmon, alternative = "two.sided", conf.level = 0.95)

#Transformation
Salmon<-mutate(Salmon, log_skinColor = log(skinColor))
view(Salmon)

summ_Salmon <- Salmon  %>%
  group_by(species) %>% 
  summarise(mean_log_skinColor = mean(log_skinColor),
            sd_log_skinColor = sd(log_skinColor),
            n_log_skinColor = n())

view(summ_Salmon)

ratio <-(max(summ_Salmon$sd_log_skinColor))/(min(summ_Salmon$sd_log_skinColor))

#The ratio is transformed to 2.6 so now we can perform a two-sample, two-sided t-test.

t.test(log_skinColor ~ species, data = Salmon, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

#Since the t-statstic value is more extreme than the given 
#t-distribution value for our df, we reject the null hypothesis that there is no significant differece
#between the sockeye and kokanee salmon. 

#Question 25

library(readr)
CC <- read_csv("~/troutman_michael/datasets/abd/chapter13/chap13q25Clearcuts.csv")
View(CC)

summ_T_Data <- CC  %>%
  summarise(mean_Biomass = mean(biomassChange),
            sd_Biomass = sd(biomassChange),
            n_biomassChange = n())
view(summ_T_Data)




ggplot(CC)+
  geom_boxplot(aes(x = "", y = biomassChange), varwidth = TRUE)


ggplot(CC)+
  geom_histogram(aes(biomassChange), binwidth = 2)

ggplot(CC)+
  geom_qq(aes(sample = biomassChange, color = ""))

#Since this distribution does not appear normal I performed a sign test instead of a t-test.
#The boxplot had two low outliers that created a left-skew that was also visible on the histogram.
#Since the qq plot was also curved, I decided to do a sign test. 


SignTest(CC$biomassChange, 
         alternative = "two.sided", mu = 0, conf.level = 0.95)

#Since the p-value is .405 (>.05), we fail to reject the null hypothesis and thus there is
#not a significant difference between the biomass of the forest following clear cutting.

#Question 26

library(readr)
ZF <- read_csv("datasets/abd/chapter13/chap13q26ZebraFinchBeaks.csv")
View(ZF)

summ_ZF_Data <- ZF   %>%
  summarise(mean_preference = mean(preference),
            sd_preference = sd(preference),
            n_preference = n())
view(summ_ZF_Data)

ggplot(ZF)+
  geom_boxplot(aes(x = "", y = preference), varwidth = TRUE)


ggplot(ZF)+
  geom_histogram(aes(preference), binwidth = 6)

ggplot(ZF)+
  geom_qq(aes(sample = preference, color = ""))

#I transformed that data since the histogram and q-q plot were especially skewed
ZF<-mutate(ZF, log_preference = log(preference))


#I did a one tailed one-sample t-test after transforming the data since it was non-normal and wased based 
#on a single sample taken from pairs where individuals were not reused, thus making it single-sample and non-paired
#I made it one-sided since red beaks were assumed to be preferred.
t.test(ZF$log_preference, 
       alternative = "greater", mu = 0, conf.level = 0.95)
#Since the t-statistic is more extreme than the known value, we reject the null hypothesis
#that red beaks are less or equally preffered.

# Missing problem 16 from the Review chapter





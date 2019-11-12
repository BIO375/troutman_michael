#Exam 2 R

rm(list = ls())

getwd()

install.packages("tidyverse")
library(tidyverse)

tidyverse_update()

install.packages("DescTools")
library("DescTools")

#Question 9

library(readr)
feathers <- read_csv("datasets/exams/feathers.csv")
View(feathers)

library(readr)
Yellowness <- read_csv("Assignments/Yellowness.csv")
View(Yellowness)

#### CODE BREAKS BECAUSE THERE IS NO FILE YELLOWNESS, not pushed to Git ####

summ_Y_Data <- Yellowness  %>%
  summarise(mean_Feather_Difference = mean(Feather_Difference),
            sd_Feather_Difference = sd(Feather_Difference),
            n_Feather_Difference = n())
view(summ_Y_Data)

ggplot(Yellowness)+
  geom_boxplot(aes(x = "", y = Feather_Difference), varwidth = TRUE)

ggplot(Yellowness)+
  geom_histogram(aes(Feather_Difference), binwidth = .02)

ggplot(Yellowness)+
  geom_qq(aes(sample = Feather_Difference, color = ""))

Yellowness<-mutate(Yellowness, log_Feather = log(Feather_Difference+1))
view(Yellowness)

ggplot(Yellowness)+
  geom_boxplot(aes(x = "", y = log_Feather), varwidth = TRUE)

ggplot(Yellowness)+
  geom_histogram(aes(log_Feather), binwidth = .02)

ggplot(Yellowness)+
  geom_qq(aes(sample = log_Feather, color = ""))

#I transformed the data to make it more normal

feathers <- mutate(feathers, diff = log(odd+1)  - log(typical+1))

#### CAN'T DO THIS TRANSFORMATION.  LOG+1 IS TO DEAL WITH ZEROES, NOT NEGATIVES ####

#In order to do the test I transformed the original dataset (not the excel I made)
#because I finally remembered how. 

t.test(feathers$odd, feathers$typical, 
     alternative = "greater", paired =  TRUE, conf.level = 0.95)



#Question 10

library(readr)
baker <- read_csv("datasets/exams/baker.csv")
View(baker)

#### CODE BREAKS BECAUSE THERE IS NO FILE Baker_Difference.csv, not pushed to Git ####

library(readr)
Baker_D <- read_csv("Assignments/Baker_Difference.csv")
View(Baker_D)

summ_B_Data <- Baker_D  %>%
  summarise(mean_antibody = mean(Antibody_Difference),
            sd_antibody = sd(Antibody_Difference),
            n_antibody = n())
view(summ_B_Data)

ggplot(Baker_D)+
  geom_boxplot(aes(x = "", y = Antibody_Difference), varwidth = TRUE)

ggplot(Baker_D)+
  geom_histogram(aes(Antibody_Difference), binwidth = .5)

ggplot(Baker_D)+
  geom_qq(aes(sample = Antibody_Difference, color = ""))

Baker_D<-mutate(Baker_D, squareroot_Antibody = sqrt(abs(Antibody_Difference)))
view(Baker_D)

ggplot(Baker_D)+
  geom_boxplot(aes(x = "", y = squareroot_Antibody), varwidth = TRUE)

ggplot(Baker_D)+
  geom_histogram(aes(squareroot_Antibody), binwidth = .5)

ggplot(Baker_D)+
  geom_qq(aes(sample = squareroot_Antibody, color = ""))

#Since the data is still extremely skewed, I will perform a sign test. 

SignTest(Baker_D$Antibody_Difference, 
         alternative = "two.sided", mu = 0, conf.level = 0.95)
#Question 11

library(readr)

#### CODE BREAKS BECAUSE THERE IS NO FILE Fossil_Fuels, not pushed to Git ####

Fossil_Fuels <- read_csv("Assignments/Fossil_Fuels.csv")
View(Fossil_Fuels)

summ_FF_Data <- Fossil_Fuels  %>%
  group_by(Treatment) %>% 
  summarise(mean_Growth_Rate = mean(Growth_Rate),
            sd_Growth_Rate = sd(Growth_Rate),
            n_Growth_Rate = n())
view(summ_FF_Data)


ggplot(Fossil_Fuels)+
  geom_boxplot(aes(x = Treatment, y = Growth_Rate), varwidth = TRUE)

ggplot(Fossil_Fuels)+
  geom_histogram(aes(Growth_Rate), binwidth = .1)+
  facet_wrap(~Treatment)

ggplot(Fossil_Fuels)+
  geom_qq(aes(sample = Growth_Rate, color = Treatment))

#Since both are fairly normal, a 2-sample t-test can be conducted.

t.test(Growth_Rate ~ Treatment, data = Fossil_Fuels, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

#### Code breaks 3 times 3/6 pts ####

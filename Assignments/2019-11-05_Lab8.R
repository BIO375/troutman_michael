rm(list = ls())

getwd()


library("ggfortify")

library("multcomp")

library("nlme")

library("tidyverse")

tidyverse_update()
 
#Question 22

library(readr)
WSH <- read_csv("datasets/abd/chapter15/chap15q22WalkingStickHeads.csv", 
                col_types = cols(specimen = col_factor(levels = c("1", 
                                                                  "2", "3", "4", "5", "6", "7", "8", 
                                                                  "9", "10", "11", "12", "13", "14", 
                                                                  "15", "16", "17", "18", "19", "20", 
                                                                  "21", "22", "23", "24", "25"))))
View(WSH)

summ_W <- WSH %>%
  group_by(specimen) %>% 
  summarise(mean_W = mean(headwidth),
            sd_W = sd(headwidth),
            n_W = n())

ggplot(WSH)+
  geom_boxplot(aes(x = "", y = headwidth), varwidth = TRUE)
ggplot(WSH) +
  geom_histogram(aes(headwidth), binwidth = 0.1)
ggplot(WSH)+
  geom_qq(aes(sample = headwidth, color = ""))

model01 <- lme(fixed = headwidth ~ 1,
               random = ~1|specimen, data = WSH)
model01_varcomp <- VarCorr(model01)
model01_varcomp

varAmong  <- as.numeric( model01_varcomp[1,1] )
varWithin <- as.numeric( model01_varcomp[2,1] )
#A VarWithin = .0002
#B. VarAmong = .0002
repeatability <- varAmong / (varAmong + varWithin)
#C. Repeatability = .597
#D. The repeatability in is higher in the 15.6 example. Since
#repeatability is higher, the error is higher since MS error is used to
#calculate repeatability. 

#Question 23

library(readr)
LPC <- read_csv("~/troutman_michael/datasets/abd/chapter15/chap15q23LodgepolePineCones.csv", 
                                        col_types = cols(habitat = col_factor(levels = c("island.absent", 
                                                                                         "island.present", "mainland.present"))))
View(LPC)

summ_L <- LPC %>%
  group_by(habitat) %>% 
  summarise(mean_cm = mean(conemass),
            sd_cm = sd(conemass),
            n_cm = n())
ggplot(LPC)+
  geom_boxplot(aes(x = habitat, y = conemass), varwidth = TRUE)
ggplot(LPC) +
  geom_histogram(aes(conemass), binwidth = 0.2)+
  facet_wrap(~habitat)
ggplot(LPC)+
  geom_qq(aes(sample = conemass, color = habitat))

ratio <-(max(summ_L$sd_cm))/(min(summ_L$sd_cm))
#A. This is a planned comparison
planned <- glht(model02, linfct = 
                  mcp(habitat = c("island.present - island.absent = 0")))
                                   
confint(planned)
summary(planned)

#This part is unneccesary since we didn't have to do C.
model02 <- lm(conemass~habitat, data = LPC)
autoplot(model02)

anova(model02)
summary(model02)

#Question 26

library(readr)

MFV <- read_csv("~/troutman_michael/datasets/abd/chapter15/chap15q26MalariaFungusVenom.csv", 
                                        col_types = cols(treatmentGroup = col_factor(levels = c("Control", 
                                                                                                "WT", "Scorpine"))))
View(MFV)

model03 <- lm(logSporozoiteNumbers~treatmentGroup, data = MFV)

summ_M <- MFV %>%
  group_by(treatmentGroup) %>% 
  summarise(mean_LSN = mean(logSporozoiteNumbers),
            sd_LSN = sd(logSporozoiteNumbers),
            n_LSN = n())
ggplot(MFV)+
  geom_boxplot(aes(x = treatmentGroup, y = logSporozoiteNumbers), varwidth = TRUE)
ggplot(MFV) +
  geom_histogram(aes(logSporozoiteNumbers), binwidth = 0.2)+
  facet_wrap(~treatmentGroup)
ggplot(MFV)+
  geom_qq(aes(sample = logSporozoiteNumbers, color = treatmentGroup))

ratio <-(max(summ_M$sd_LSN))/(min(summ_M$sd_LSN))

tukey <- glht(model03, linfct = mcp(treatmentGroup = "Tukey"))
summary(tukey)

#Question 30

library(readr)
crab <- read_csv("~/troutman_michael/datasets/abd/chapter15/chap15q30FiddlerCrabFans.csv", 
                                     col_types = cols(crabType = col_factor(levels = c("female", 
                                                                                       "intact male", "male minor removed", 
                                                                                     "male major removed"))))

crab <- crab %>%
mutate(crabType = fct_recode(crabType, IM = "intact male",
                             M_Minor = "male minor removed",
                             M_Major = "male major removed"
))
crab <- slice(crab, -85) 
view(crab)

model04 <- lm(bodyTemperature~crabType, data = crab)

summ_C <- crab %>%
  group_by(crabType) %>% 
  summarise(mean_crabt = mean(bodyTemperature),
            sd_crabt = sd(bodyTemperature),
            n_crabt = n())
view(summ_C)

ggplot(crab)+
  geom_boxplot(aes(x = crabType, y = bodyTemperature), varwidth = TRUE)
ggplot(crab) +
  geom_histogram(aes(bodyTemperature), binwidth = 0.1)+
  facet_wrap(~crabType)
ggplot(crab)+
  geom_qq(aes(sample = bodyTemperature, color = crabType))

ratio <-(max(summ_C$sd_crabt))/(min(summ_C$sd_crabt))
autoplot(model04)
anova(model04)

#Question 31
planned <- glht(model04, linfct = 
                  mcp(crabType = c("M_Major - M_Minor = 0")))

confint(planned)
summary(planned)





rm(list = ls())

getwd()


library("ggfortify")

install.packages("broom")
library("broom")

library("tidyverse")
tidyverse_update()

library("nlme")

#Question 10
library(readr)
A <- read_csv("datasets/exams/aphids.csv")
View(A)

model02 <- lme(fixed = thorax_length ~ 1,
               random = ~1|gall_number, data = A)

model02_varcomp <- VarCorr(model02)
model02_varcomp
gall_number = pdLogChol(1)

varAmong  <- as.numeric( model02_varcomp[1,1] )
varWithin <- as.numeric( model02_varcomp[2,1] )

repeatability <- varAmong / (varAmong + varWithin)


summary(model02)

#Question 11

library(readr)
glucose <- read_csv("datasets/exams/glucose.csv")
View(glucose)


#Question 12
library(readr)
DV <- read_csv("datasets/exams/DriverVision.csv")
View(DV)

model03 <- lm(Age ~ Distance, data = DV)
autoplot(model03, smooth.colour = NA)

ggplot(data = DV)+
  geom_point(aes(x = Age, y = resid(model03)))

summary(model03)

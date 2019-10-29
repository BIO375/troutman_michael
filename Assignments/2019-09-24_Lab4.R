rm(list = ls())

getwd()

install.packages("tidyverse")

library("tidyverse")

tidyverse_update()

library(readr)

library(readr)
B_D <- read_csv("~/B_D.csv")
View(B_D)

sum_B_D <- B_D %>%
  summarize(mean_Birth_Difference = mean(Birth_Difference),
            median_Birth_Difference = median(Birth_Difference),
            sd_Birth_Difference = sd(Birth_Difference),
            var_Birth_Difference = var(Birth_Difference), 
            n_Birth_Difference = n())
view(sum_B_D)

ggplot(B_D) +
  geom_boxplot(aes(x = "", y = Birth_Difference), notch = FALSE, varwidth = TRUE)

ggplot(B_D) + 
  geom_histogram(aes(Birth_Difference), binwidth = .5)


Scenario 2

data01 <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")

data01 <- data01 %>% slice(-105)

library(readr)
chap12e3HornedLizards <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")
View(chap12e3HornedLizards)

sum_data01 <- data01 %>%
  group_by (Survival) %>%
  summarize(mean_squamosalHornLength = mean(squamosalHornLength),
            median_squamosalHornLength = median(squamosalHornLength),
            sd_squamosalHornLength = sd(squamosalHornLength),
            var_squamosalHornLength = var(squamosalHornLength), 
            n_squamosalHornLength = n())

view(sum_data01)

ggplot(data01) +
  geom_boxplot(aes(x = Survival, y = squamosalHornLength), notch = FALSE, varwidth = TRUE)

ggplot(data01) + 
  geom_histogram(aes(squamosalHornLength), binwidth = 2)+
  facet_wrap(~Survival)



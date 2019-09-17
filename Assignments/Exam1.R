rm(list = ls())

getwd()

install.packages("tidyr")

library("tidyverse")

tidyverse_update()


library(readr)
chap13e5SagebrushCrickets <- read_csv("datasets/abd/chapter13/chap13e5SagebrushCrickets.csv", col_names=TRUE)
col_types = cols(
  feedingStatus= col_character())

View(chap13e5SagebrushCrickets)

                 





summ_feedingstatus <- chap13e5SagebrushCrickets %>%
group_by(feedingStatus) %>%
  summarise(n_feedingstatus = n(),
            mean_timeToMating = mean(timeToMating),
            median_timeToMating = median(timeToMating),
            IQR_timeToMating = IQR(timeToMating),
            sd_timeToMating = sd(timeToMating),
            var_timeToMating = var(timeToMating),
            n_timeToMating = n())
view(summ_feedingstatus)


Histograms


ggplot(chap13e5SagebrushCrickets) + 
  geom_histogram(aes(timeToMating), binwidth = 3)+
  facet_wrap(~feedingStatus)

chap13e5SagebrushCrickets<-mutate(chap13e5SagebrushCrickets, log_Cricket = log(timeToMating))

ggplot(chap13e5SagebrushCrickets) +
  geom_histogram(aes(log_Cricket), binwidth = .1)

ggplot(chap13e5SagebrushCrickets) +
  geom_histogram(aes(timeToMating), binwidth = 2)

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
# MICHAEL: Line 14 is not within the parentheses of the function
# read_csv and therefore is not being executed
# It doesn't throw up an error message, it just doesn't do
# what you think it does

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

# When you are leaving notes to yourself like below, use
# a hashtag to keep them as comments.  Otherwise they can
# break your code
Histograms


ggplot(chap13e5SagebrushCrickets) + 
  geom_histogram(aes(timeToMating), binwidth = 3)+
  facet_wrap(~feedingStatus)

chap13e5SagebrushCrickets<-mutate(chap13e5SagebrushCrickets, log_Cricket = log(timeToMating))
# The two plots below do not have a facet wrap and so they
# show data from both feedingStatus treatments lumped together
ggplot(chap13e5SagebrushCrickets) +
  geom_histogram(aes(log_Cricket), binwidth = .1)

ggplot(chap13e5SagebrushCrickets) +
  geom_histogram(aes(timeToMating), binwidth = 2)


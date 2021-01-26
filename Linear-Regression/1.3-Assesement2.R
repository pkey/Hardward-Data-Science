library(tidyverse)

set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

mean_mom <- mean(female_heights$mother)
mean_daughter <- mean(female_heights$daughter)

sd_mom <- sd(female_heights$mother)
sd_daughter <- sd(female_heights$daughter)

r <- cor(female_heights$mother, female_heights$daughter)
mean_mom
sd_mom

m_1 <-  r * sd_daughter / sd_mom
b_1 <- mean_daughter - m_1*mean_mom

r^2 * 100

b_1 + m_1 * 60

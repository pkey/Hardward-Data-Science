library(tidyverse)
library(HistData)
data("GaltonFamilies")

rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    .$coef 
})

cor(lse[1,], lse[2,])

lse %>% summarise(cor(beta_0, beta_1))

B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})

cor(lse[1,], lse[2,]) 

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)


library(Lahman)

FilteredTeams <- Teams %>% filter(yearID %in% 1961:2001)


FilteredTeams %>% mutate(rpg = R/G, bpg = BB / G, hpg = HR/ G) %>% 
lm(rpg ~ bpg + hpg, data = .)

help(lm)

#### Assesment

set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

lm(female_heights$mother ~ female_heights$daughter, data = female_heights)

44.18 + 0.31 * female_heights$daughter[0]

## Assesement part 2

library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

mean_singles <- bat_01 %>%
  group_by(playerID) %>% 
  summarise(mean_singles = mean(singles))

mean_bb <- bat_01 %>%
  group_by(playerID) %>% 
  summarise(mean_bb = mean(bb))

joined_1 <- inner_join(bat_02, mean_singles)
joined <- inner_join(joined_1, mean_bb)

cor(joined$singles, joined$mean_singles)
cor(joined$bb, joined$mean_bb)

joined %>% ggplot(aes(mean_singles, singles)) + 
  geom_point(alpha = 0.5)

joined %>% ggplot(aes(mean_bb, bb)) + 
  geom_point(alpha = 0.5)

lm(joined$singles ~ joined$mean_singles, data = joined)
lm(joined$bb ~ joined$mean_bb, data = joined)

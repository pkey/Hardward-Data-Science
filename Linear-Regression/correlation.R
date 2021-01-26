# create the dataset
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# means and standard deviations
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

# scatterplot of father and son heights
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)

## correlation
rho <- mean(scale(x)*scale(y))
galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)

## with a sample size of 25
R <- sample_n(galton_heights, 25, replace = TRUE) %>% 
  summarize(r = cor(father, son)) %>% pull(r)

## monte carlo on 25
B <- 1000
N <- 50
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    summarize(r=cor(father, son)) %>% 
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))

mean(R)
sd(R)

ggplot(aes(sample=R), data = data.frame(R)) + 
  stat_qq() + 
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))


library(Lahman)


FilteredTeams <- Teams %>% filter(yearID %in% 1961:2001)

# Correlation between runs per game and bats per game
R <- FilteredTeams %>% mutate(R_per_game = R / G, AB_per_game = AB / G) %>%
  summarize(r = cor(R_per_game, AB_per_game)) %>% 
  pull(r)

#Correlation between win rate and number of errors per game
R <- FilteredTeams %>% mutate(Wpg = W / G, Epg = E / G) %>%
  summarize(r = cor(Wpg, Epg)) %>% 
  pull(r)
R

#Correlation between doubles and triples per game
R <- FilteredTeams %>% mutate(x2pg = X2B / G, x3pg = X3B / G) %>%
  summarize(r = cor(x2pg, x3pg)) %>% 
  pull(r)
R



# Exploring the currency cross-rates data
library(tourr)
library(tidyverse)
library(GGally)
library(woylier)

# Read data
rates <- read_csv("data-raw/rates_Nov19_Mar20.csv")
rates <- rates %>% select(date, ARS, AUD, EUR, JPY, KRW, MYR)#, NZD, GBP)

rates_std <- apply(rates[,-1], 2, function(x) (x-mean(x))/sd(x))
# animate_xy(rates_std, tour_path = guided_tour(splines2d()))
#rates <- rates %>% select(date, ARS, AUD, EUR, JPY, MYR, ISK)#, NZD, GBP)
animate_xy(rates[,-1])
animate_xy(rates[,-1], tour_path = guided_tour(splines2d()))
pp1 <- animate_xy(rates_std[,-1], tour_path = guided_tour_givens(splines2d(), search_f = search_better))

basis1 <- as.matrix(pp1[737,]$basis[[1]], ncol = 2, byrow=TRUE)

pp1x <- as_tibble(as.matrix(rates_std[,-1])%*%basis1)

ggplot(pp1x, aes(x=V1, y=V2))+
  geom_point()

# Check PCA: why doesn't PCA detect correlation
# Correlation is between NZD and GBP
rates_pca <- prcomp(rates[,-1], scale. = TRUE)
ggscatmat(rates_pca$x)

# Check standardised data - not necessary
# rates_std <- apply(rates[,-1], 2, function(x) (x-mean(x))/sd(x))
# animate_xy(rates_std, tour_path = guided_tour(splines2d()))

# Data checks
ggplot(rates, aes(x=NZD, y=GBP)) + geom_point()
ggplot(rates, aes(x=NZD, y=AUD)) + geom_point()

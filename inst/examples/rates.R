# Exploring the currency cross-rates data
library(tourr)
library(tidyverse)
library(GGally)
library(woylier)

set.seed(202212)

# Read data
rates <- read_csv("data-raw/rates_Nov19_Mar20.csv")
rates <- rates %>% select(date, ARS, AUD, EUR, JPY, KRW, MYR)#, NZD, GBP)

rates_std <- apply(rates[,-1], 2, function(x) (x-mean(x))/sd(x))
# animate_xy(rates_std, tour_path = guided_tour(splines2d()))
#rates <- rates %>% select(date, ARS, AUD, EUR, JPY, MYR, ISK)#, NZD, GBP)
animate_xy(rates[,-1])
animate_xy(rates[,-1], tour_path = guided_tour(splines2d()))
pp_geo <- animate_xy(rates_std[,-1], tour_path = guided_tour(splines2d()))
pp_giv1 <- animate_xy(rates_std[,-1], tour_path = guided_tour_givens(splines2d(), search_f = search_better))
pp_giv2 <- animate_xy(rates_std[,-1], tour_path = guided_tour_givens(splines2d(), search_f = search_better_random))

basis1 <- as.matrix(pp_giv1[737,]$basis[[1]], ncol = 2, byrow=TRUE)

pp1x <- as_tibble(as.matrix(rates_std[,-1])%*%basis1)

ggplot(pp1x, aes(x=V1, y=V2))+
  geom_point()

# Check PCA: why doesn't PCA detect correlation
# Correlation is between NZD and GBP
# Use PCA to remove linear dependence
rates_pca <- prcomp(rates[,-1], scale. = TRUE)
ggscatmat(rates_pca$x)

# standardise the pca
rates_pca_sd <-  apply(rates_pca$x, 2, function(x) (x-mean(x))/sd(x))
summary(rates_pca$x)
animate_xy(rates_pca_sd)

# the index value of the givens does not go as high as geodesic

animate_xy(rates_pca_sd[,1:4], tour_path = guided_tour_givens(splines2d(), search_f = search_better))

animate_xy(rates_pca_sd[,1:4], tour_path = guided_tour_givens(splines2d(), search_f = search_better_random, max.tries = 100))

animate_xy(rates_pca_sd[,1:4], tour_path = guided_tour(splines2d()))

# modified the splines2d
new_splines2d <- function ()
{
  function(mat) {
    mat <- as.data.frame(mat)
    colnames(mat) <- c("x", "y")
    kx <- ifelse(length(unique(mat$x[!is.na(mat$x)])) < 20,
                 3, 10)
    mgam1 <- mgcv::gam(y ~ s(x, bs = "cr", k = kx), data = mat)
    measure <- 1 - var(residuals(mgam1), na.rm = T)/var(mat$y, na.rm = T)
    return(measure)
  }
}

# try modified splines index
animate_xy(rates_pca_sd[,1:4], tour_path = guided_tour_givens(new_splines2d(), search_f = search_better, max.tries = 1000))

animate_xy(rates_pca_sd[,1:4], tour_path = guided_tour_givens(new_splines2d(), search_f = search_better_random, max.tries = 1000))

animate_xy(rates_pca_sd[,1:4], tour_path = guided_tour(new_splines2d()))

set.seed(202212)
basis2 <- basis_random(n=4, d = 2)
#
r1 <- animate_xy(rates_pca_sd[,1:4], tour_path = guided_tour(new_splines2d(), current = basis2, search_f = search_better, max.tries = 1000), rescale=FALSE)
r1$basis[2619]
# index value 0.696
r2 <- animate_xy(rates_pca_sd[,1:4], tour_path = guided_tour_givens(new_splines2d(), current = basis2, search_f = search_better, max.tries = 1000), rescale=FALSE)
# index value 0.762859
r3 <- animate_xy(rates_pca_sd[,1:4], tour_path = guided_tour_givens(new_splines2d(), current = basis2, search_f = search_better_random, max.tries = 1000), rescale=FALSE)
# index value 0.808788

mat <- data.frame(rates_pca_sd[,2:1])
mat_idx <- round(new_splines2d()(mat), 2)
mat_idx

mat_rot <- data.frame(x = cos(pi/4) * mat$PC1 +
                        sin(pi/4) * mat$PC2 ,
                      y = -sin(pi/4) * mat$PC1  +
                        cos(pi/4) * mat$PC2)

mat_idx2 <- round(new_splines2d()(mat_rot), 2)
mat_idx2


ggplot(as_tibble(rates_pca_sd), aes(x = PC2, y= PC1)) +
  geom_point()

# This code is for examining the rotations
library(tourr)
library(tidyverse)

set.seed(1970)
path_geo_1d_1 <- save_history(flea[, 1:3], grand_tour(1), max = 3)
path_geo_1d_1 <- interpolate(path_geo_1d_1)
#path_geo_1d_1 <-
#  cbind(path_geo_1d_1, group = rep("1", nrow(path_geo_1d_1)))
path_geo_1d_1_m <- t(apply(path_geo_1d_1, 3, c)) %>%
  as.data.frame() %>%
  mutate(id = "path")

# generating spheres
library(geozoo)
d <- sphere.hollow(p = 3, n = 1000)
#d <- sphere.hollow(p = 5, n = 1000)
d <- data.frame(d$points)
animate_xy(d, axes="off")
animate_slice(d, axes="off")

# Add path to sphere
d <- d %>%
  rename(V1=X1, V2=X2, V3=X3) %>%
  mutate(id = "sphere")

path_sphere <- bind_rows(path_geo_1d_1_m, d)

animate_xy(path_sphere[,1:3], col=path_sphere$id, axes="off")

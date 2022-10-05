library(tourr)
library(tidyverse)
library(woylier)

# Generate 1D example

set.seed(2022)
p <- 4
base1 <- tourr::basis_random(p, d=1)
base2 <- tourr::basis_random(p, d=1)
base3 <- tourr::basis_random(p, d=1)

# First example

frames <- givens_full_path(base1, base2, nsteps = 10)

sp <- generate_space_view(p=p)

sp_path <- add_path(sp, frames) 

point1 <- as.data.frame(t(base1)) 
point1$type <- "point1"

point2 <- as.data.frame(t(base2))
point2$type <- "point2"

sp_path <- rbind(sp_path, point1, point2) 

#tourr::animate_xy(sp_path[,1:p], col=sp_path$type, axes="bottomleft")

# second example

frames <- givens_full_path(base2, base3, nsteps = 10)

frames <- as.data.frame(t(apply(frames, 3, c)))

frames$type <- "path"
sp_path <- rbind(sp_path, frames)

point3 <- as.data.frame(t(base3))
point3$type <- "point3"

sp_path <- rbind(sp_path, point3) 

tourr::animate_xy(sp_path[,1:p], col=sp_path$type, 
                  axes="bottomleft")

# Generate 2D example

set.seed(2022)
p <- 3
base1 <- tourr::basis_random(p, d=2)
base2 <- tourr::basis_random(p, d=2)

frames_2d <- givens_full_path(base1, base2, 10)

# Check if last matrix of interpolation is the target frame

frames_2d[,,10]

base2

# Testing torus construction

n <- 1000
p <- 3
d <- 2

#sp <- data.frame(geozoo::torus.flat(p = p*2, n=n)$points)
#tourr::animate_xy(sp, axes="bottomleft")
#tourr::animate_slice(sp)

# Check on torus

proj_2d <- map(1:n, ~basis_random(n = p,  d=d)) %>%
  purrr::flatten_dbl() %>% 
  matrix(ncol = p*2, byrow = TRUE) %>%
  as_tibble()

tourr::animate_xy(proj_2d, axes="bottomleft")
tourr::animate_slice(proj_2d, axes="bottomleft")

# Path
path_2d <- t(apply(frames_2d, 3, c)) %>% 
  as.data.frame()

tourr::animate_xy(path_2d)

# Join
proj_2d <- proj_2d %>% 
  mutate(type="torus")

path_2d <- path_2d %>% 
  mutate(type="path")

proj_path <- bind_rows(proj_2d, path_2d)

sp_path <- rbind(sp_path, point1, point2) 

tourr::animate_xy(proj_path[,1:6], 
                  col=proj_path$type, 
                  axes="bottomleft")

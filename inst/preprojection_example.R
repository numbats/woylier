library(tourr)
library(tidyverse)

# example
set.seed(2022)
p <- 4
base1 <- tourr::orthonormalise(tourr::basis_random(p, d=1))
base2 <- tourr::orthonormalise(tourr::basis_random(p, d=1))
base3 <- tourr::orthonormalise(tourr::basis_random(p, d=1))

# first example

b <- preprojection(base1, base2)

Wa <- construct_preframe(base1, b) 
Wz <- construct_preframe(base2, b) 
#Wz <- construct_Wz(base2, b) 

tau <- calculate_tau(Wz, Wa)

frames <- givens_full_path(b, Wa, tau, nsteps=10)

sp <- generate_space_view(p=p)

sp_path <- add_path(sp, frames) 

point1 <- as.data.frame(t(base1)) 
point1$type <- "point1"

point2 <- as.data.frame(t(base2))
point2$type <- "point2"

sp_path <- rbind(sp_path, point1, point2) 

#tourr::animate_xy(sp_path[,1:p], col=sp_path$type, axes="bottomleft")

# second example

b <- preprojection(base2, base3)

Wa <- construct_preframe(base2, b) 
Wz <- construct_preframe(base3, b) 

tau <- calculate_tau(Wz, Wa)

frames <- givens_full_path(b, Wa, tau, nsteps=10)

frames <- as.data.frame(t(apply(frames, 3, c)))

frames$type <- "path"
sp_path <- rbind(sp_path, frames)

point3 <- as.data.frame(t(base3))
point3$type <- "point3"

sp_path <- rbind(sp_path, point3) 

tourr::animate_xy(sp_path[,1:p], col=sp_path$type, 
                  axes="bottomleft")

library(tourr)
library(tidyverse)

# example
set.seed(2022)
base1 <- tourr::orthonormalise(tourr::basis_random(10, d=1))
base2 <- tourr::orthonormalise(tourr::basis_random(10, d=1))
base3 <- tourr::orthonormalise(tourr::basis_random(10, d=1))

b <- preprojection(base1, base2)

Wa <- construct_Wa(base1, b) #%>% round(3)
Wz <- construct_Wz(base2, b) #%>% round(3)

tau <- calculate_tau(Wz, Wa)

frames <- givens_full_path(b, Wa, tau, nsteps=10)

sp <- generate_space_view(p=10)

sp_path <- add_path(sp, frames)

b <- preprojection(base2, base3)

Wa <- construct_Wa(base2, b) 
Wz <- construct_Wz(base3, b) 

tau <- calculate_tau(Wz, Wa)

frames <- givens_full_path(b, Wa, tau, nsteps=10)
frames <- as.data.frame(t(apply(frames, 3, c)))
frames$type <- "path"
sp_path <- rbind(sp_path, frames)

tourr::animate_xy(sp_path[,1:10], col=sp_path$type, 
                  axes="bottomleft")


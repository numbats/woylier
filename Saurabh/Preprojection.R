# ----- testing code
library(tourr)
library(tidyverse)

Fa <- matrix(runif(8), ncol = 2) %>% orthonormalise()
Fz <- matrix(runif(8), ncol = 2) %>% orthonormalise()

Fnorm <- orthonormalise_by(Fz, Fa)
Fi <- cbind(Fnorm[,2], Fnorm[,1])
Fnorm <- orthonormalise_by(Fi, Fa)
t(Fnorm) %*% Fa
preprojection(Fa, Fz)

# ------ function
preprojection <- function(Fa, Fz) {
  Fnorm <- orthonormalise_by(Fz, Fa)
  Fi <- cbind(Fnorm[,2], Fnorm[,1])
  Fnorm <- orthonormalise_by(Fi, Fa)
  B <- cbind(Fa, Fnorm)
  Wa <- t(B) %*% Fa
  Wz <- t(B) %*% Fz
  
  list("Wa" = Wa, "Wz" = Wz, "B" = B)
}


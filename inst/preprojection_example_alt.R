library(tourr)
library(tidyverse)
preprojection <- function(Fa, Fz) {
  # check both are matrices are both correct size
  stopifnot("Your inputs do not have the same number of columns!" = ncol(Fa) == ncol(Fz))
  stopifnot("Your inputs do not have the same number of row!" = nrow(Fa) == nrow(Fz))
  # check each is orthonormal
  #stopifnot("The current frame must be orthonormal!" = is_orthonormal(Fa))
  #stopifnot("The target frame must be orthonormal!" = is_orthonormal(Fz))
  # stopifnot with message
  Fa <- orthonormalise(Fa)
  Fz <- orthonormalise(Fz)
  Fz_star <- tourr::orthonormalise_by(Fz, Fa)
  B <- cbind(Fa, Fz_star)
  return(B)
}

construct_Wa <- function(Fa, B) {
  Wa <- t(B) %*% Fa
  return(Wa)
}

construct_Wz <- function(Fz, B) {
  Wz <- t(B) %*% Fz
  return(Wz)
}

find_rotation <- function(Wz) {
  a = Wz[1]
  b = Wz[2]
  if (b==0) {
    c = 1
    s = 0
  } else {
    if (abs(b) > abs(a)) {
      t = -a/b
      s = 1/sqrt(1+t^2)
      c = s*t
    }
    else{
      t = -b/a
      c = 1/sqrt(1+t^2)
      s = c*t
    }
  }
  rotation_matrix <- matrix(c(c,-s,s, c), nrow = 2, ncol = 2, byrow = TRUE)
  angle = acos(c)
  print(angle)
  return(rotation_matrix)
}

set.seed(2022)
base1 <- orthonormalise(tourr::basis_random(10, d=1))
base2 <- orthonormalise(tourr::basis_random(10, d=1))

b <- preprojection(base1, base2)

Wa <- construct_Wa(base1, b) %>% round(3)
Wz <- construct_Wz(base2, b) %>% round(3)

find_rotation(Wz)%*%Wz

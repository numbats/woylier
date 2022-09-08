library(tourr)
library(tidyverse)

# general
preprojection <- function(Fa, Fz) {
  # check both are matrices are both correct size
  stopifnot("Your inputs do not have the same number of columns!" = ncol(Fa) == ncol(Fz))
  stopifnot("Your inputs do not have the same number of row!" = nrow(Fa) == nrow(Fz))
  # check each is orthonormal
  stopifnot("The current frame must be orthonormal!" = is_orthonormal(Fa))
  stopifnot("The target frame must be orthonormal!" = is_orthonormal(Fz))
  # stopifnot with message
  Fa <- orthonormalise(Fa)
  Fz <- orthonormalise(Fz)
  Fz_star <- tourr::orthonormalise_by(Fz, Fa)
  B <- cbind(Fa, Fz_star)
  return(B)
}

# general
construct_Wa <- function(Fa, B) {
  Wa <- t(B) %*% Fa
  return(Wa)
}

# general
construct_Wz <- function(Fz, B) {
  Wz <- t(B) %*% Fz
  return(Wz)
}

# general

calculate_tau <- function(F1, F2) {
  # takes 2 vectors with 2 elements and calculate angle between them 
  # This needs to be generalised to frames instead of vectors
  # calculate the angle between 2 vectors 360 degrees
  tau <- atan2(F1[2], F1[1]) - atan2(F2[2], F2[1]) 
  return(tau)
}

# general

rotation_matrix <- function(theta){ 
  # rotate a 2d vector by given angle
  if (theta>0) {
    rotation_matrix <- matrix(c(cos(theta),-sin(theta),sin(theta), cos(theta)), nrow = 2, ncol = 2, byrow = TRUE)
    #x_rotated <- rotation_matrix%*%x
  }
  # clockwise
  rotation_matrix <- matrix(c(cos(theta),sin(theta),-sin(theta), cos(theta)), nrow = 2, ncol = 2, byrow = TRUE)
  #x_rotated <- rotation_matrix%*%x
  return(rotation_matrix)
}

# specific case of d=1
compute_rotation <- function(Wz, tau, nsteps) {
  # For now this will be a single rotation matrix
  # but at some generalised
  # this should compute an increment 
  # apply k (nsteps) times
  fraction <- 1/nsteps
  df <- data.frame(matrix(ncol = 2, nrow = nsteps)) # creates dataframe for plotting
  df[1,] <-  Wz
  a1 <- Wa # starts with the vector 1
  for (i in 1:nsteps) {
    theta = tau*fraction*i
    rotated <- rotation_matrix(theta) %*% Wz
    df[i+1,1] <- rotated[1]
    df[i+1,2] <- rotated[2]# update the dataframe
  }
  return(df)
  }

# general
revert_to_original <- function(df, B) {
  result <- array(dim = c(nrow(B), ncol(B)/2, nrow(df)))
  for (i in 1:nrow(df)) {
    Wi = matrix(c(df[i, 1], df[i, 2]), nrow = 2, ncol = 1, byrow =TRUE)
    Fi = B %*% Wi
    result[,,i] <- Fi
  }
  print(result)
}

# example
set.seed(2022)
base1 <- orthonormalise(tourr::basis_random(10, d=1))
base2 <- orthonormalise(tourr::basis_random(10, d=1))

b <- preprojection(base1, base2)

Wa <- construct_Wa(base1, b) %>% round(3)
Wz <- construct_Wz(base2, b) %>% round(3)

tau <- calculate_tau(Wz, Wa)

w <- compute_rotation(Wz, tau, 10)

revert_to_original(w, b)

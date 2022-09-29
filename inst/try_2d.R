library(tourr)
library(tidyverse)

angle2 <-  function(x, y){ 
  # calculate the angle between 2d vectors 360 degrees
  theta <- atan2(x[2], x[1]) - atan2(y[2], y[1]) 
  return(theta)
}

orthonormalise_by <- function(x, by) {
  stopifnot(ncol(x) == ncol(by))
  stopifnot(nrow(x) == nrow(by))
  
  x <- normalise(x)
  by <- normalise(by)
  
  for (j in seq_len(ncol(x))) {
    x[, j] <- x[, j] - as.vector(crossprod(x[, j], by[, j])) * by[, j]
    for (k in seq_len(ncol(by))) {
      x[, j] <- x[, j] - as.vector(crossprod(x[, j], by[, k])) * by[, k]
      x[, j] <- normalise(x[, j])
    }
  }
  normalise(x)
  # Last step, columns new matrix to orthonormal
  if (ncol(x) > 1) {
    for (j in 2:ncol(x)) {
      x[, j] <- x[, j] - as.vector(crossprod(x[, j], x[, j-1])) * x[, j-1]
      normalise(x[, j])
    }
  }
  return(x)
}

preprojection <- function(Fa, Fz) {
  # check both are matrices are both correct size
  stopifnot("Your inputs do not have the same number of columns!" = ncol(Fa) == ncol(Fz))
  stopifnot("Your inputs do not have the same number of row!" = nrow(Fa) == nrow(Fz))
  
  # check each is orthonormal
  stopifnot("The current frame must be orthonormal!" = tourr::is_orthonormal(Fa))
  stopifnot("The target frame must be orthonormal!" = tourr::is_orthonormal(Fz))
  
  Fz_star <- orthonormalise_by(Fz, Fa)
  B <- cbind(Fa, Fz_star)
  return(B)
}

construct_preframe <- function(Fr, B) {
  W <- t(B) %*% Fr
  return(W)
}

set.seed(2022)
p <- 6
base1 <- tourr::basis_random(p, d=2)
base2 <- tourr::basis_random(p, d=2)

b <- preprojection(base1, base2)

Wa <- construct_preframe(base1, b) %>% round(3)
Wz <- construct_preframe(base2, b) 


# Trying to build rotation matrices manually and multiply

x1 <- as.matrix(c(Wa[1, 1], Wa[2, 1]))
y1 <- as.matrix(c(Wz[1, 1], Wz[2, 1]))

theta1 <- angle2(x1, y1)

g1 <- matrix(c(cos(theta1), -sin(theta1), 0, 0, sin(theta1), cos(theta1), 0, 0, 0, 0, 1, 0, 0, 0, 0, 1), nrow = 4, ncol = 4, byrow = TRUE)

w1 <- g1 %*% Wz

w1

x2 <- as.matrix(c(Wa[1, 1], Wa[3, 1]))
y2 <- as.matrix(c(w1[1, 1], w1[3, 1]))

theta2 <- angle2(x2, y2)

g2 <- matrix(c(cos(theta2), 0, -sin(theta2), 0, 0, 1, 0, 0, sin(theta2), 0, cos(theta2), 0, 0, 0, 0, 1), nrow = 4, ncol = 4, byrow = TRUE)

w2 <- g2 %*% w1
w2

x3 <- as.matrix(c(Wa[1, 1], Wa[4, 1]))
y3 <- as.matrix(c(w2[1, 1], w2[4, 1]))

theta3 <- angle2(x3, y3)

g3 <- matrix(c(cos(theta3), 0, 0, -sin(theta3), 0, 1, 0, 0, 0, 0, 1, 0, sin(theta3), 0, 0, cos(theta3)), nrow = 4, ncol = 4, byrow = TRUE)

w3 <- g3 %*% w2
w3

x4 <- as.matrix(c(Wa[2, 2], Wa[3, 2]))
y4 <- as.matrix(c(w3[2, 2], w3[3, 2]))

theta4 <- angle2(x4, y4)

g4 <- matrix(c(1, 0, 0, 0, 0, cos(theta4), -sin(theta4), 0, 0, sin(theta4), cos(theta4), 0, 0, 0, 0, 1), nrow = 4, ncol = 4, byrow = TRUE)

w4 <- g4 %*% w3
w4

x5 <- as.matrix(c(Wa[2, 2], Wa[4, 2]))
y5 <- as.matrix(c(w4[2, 2], w4[4, 2]))

theta5 <- angle2(x5, y5)

g5 <- matrix(c(1, 0, 0, 0, 0, cos(theta5), 0, -sin(theta5), 0, 0, 1, 0, 0, sin(theta5), 0, cos(theta5)), nrow = 4, ncol = 4, byrow = TRUE)

w5 <- g5 %*% w4
w5 %>% round(3)

Wz

# ends manual multiplication 

# trying simple structure of rotation matrix 5.1.9 of Matrix computations

row_rot <- function(a, i, k, theta) {
  # takes i and k th row of a matrix and rotate matrix by theta angle
  # requires matrix a to we 2*q matrix
  n <- ncol(a)
  for (q in 1:n){
    x = a[i, q]
    y = a[k, q]
    a[i, q] = cos(theta)*x - sin(theta)*y
    a[k, q] = sin(theta)*x + cos(theta)*y
  }
  return(a)
}

row_rot(Wz, 1, 2, theta1)

row_rot(w1, 1, 3, theta2)

row_rot(w2, 1, 4, theta3)

row_rot(w3, 2, 3, theta4)

row_rot(w4, 2, 4, theta5)

# putting above in a loop

wi = Wz
for (col in 1:ncol(Wz)) {
  for (row in col:(nrow(Wz)-1)){
    x <- as.matrix(c(Wa[col, col], Wa[row+1, col]))
    y <- as.matrix(c(wi[col, col], wi[row+1, col]))
    theta = angle2(x, y)
    wi = row_rot(wi, col, row+1, theta)
  }
}

# reversing to order of rotation

w_1 <- row_rot(Wa, 2, 4, -theta5)
w_2 <- row_rot(w_1, 2, 3, -theta4)
w_3 <- row_rot(w_2, 1, 4, -theta3)
w_4 <- row_rot(w_3, 1, 3, -theta2)
w_5 <- row_rot(w_4, 1, 2, -theta1)
w_5
Wz

wi = Wz
angles = list()

for (col in 1:ncol(Wz)) {
  for (row in col:(nrow(Wz)-1)){
    x <- as.matrix(c(Wa[col, col], Wa[row+1, col]))
    y <- as.matrix(c(wi[col, col], wi[row+1, col]))
    theta = angle2(x, y)
    angles[paste0(col, row +1)] = theta
    wi = row_rot(wi, col, row+1, theta)
  }
}

angles

w_i = Wa
for (col in ncol(Wa):1) {
  for (row in (nrow(Wa)-1):col){
    index = paste0(col, row+1)
    theta = - as.numeric(angles[index])
    w_i = row_rot(w_i, col, row+1, theta)
  }
}
w_i
Wz


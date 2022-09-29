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

# ends manual multiplication 

row_rot <- function(a, theta) {
  # takes a matrix and angle rotate rows by given angle
  n <- ncol(a)
  for (i in 1:n){
    x = a[1, i]
    print(x)
    y = a[2, i]
    print(y)
    a[1, i] = cos(theta)*x - sin(theta)*y
    a[2, i] = cos(theta)*x + sin(theta)*y
  }
  return(a)
}




row_rot(Wz, theta1)
Wz

w1

col_rot <- function(a, theta) {
  # takes a matrix and angle rotate columns by given angle
  n <- nrow(a)
  for (i in 1:n){
    x = a[i, 1]
    y = a[i, 2]
    a[i, 1] = cos(theta)*x - sin(theta)*y
    a[i, 2] = cos(theta)*x + sin(theta)*y
  }
  return(a)
}

col_rot(Wz, theta1)

w1

givens_angle <- function(a, b) {
  if (b == 0) {
    c = 1
    s = 0
  }
  else {
    if (abs(b) > abs(a)) {
      tau = -a/b
      s = 1/sqrt(a + tau^2)
      c = s*tau
    }
    else {
      tau = -b/a
      c = 1/sqrt(a + tau^2)
      s = c*tau
    }
  }
  return(c(c, s))
}

givens_angle(0.1898, -0.0973)

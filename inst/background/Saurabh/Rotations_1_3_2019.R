library(tourr)

orthonormaliseFixed <- function(x) {
  x <- normalise(x) # to be conservative
  
  if (ncol(x) > 1) {
    for (j in seq_len(ncol(x))) {
      for (i in seq_len(j - 1)) {
        x[, j] <- x[, j] - as.vector(crossprod(x[, j], x[, i])/crossprod(x[, i], x[, i])) * x[, i]
      }
    }
  }
  
  normalise(x)
}


preprojection <- function(Fa, Fz) {
  Fall <- cbind(Fa, Fz)
  Fnorm <- orthonormaliseFixed(Fall)
  Fstar <- Fnorm[,3:4]
  B <- cbind(Fa, Fstar)
  Wa <- t(B) %*% Fa
  Wz <- t(B) %*% Fz
  list(B, Wz, Wa)
}

find_taus <- function(Wz) {
  #Wz <- orthonormalise(Wz) #Unsure is Wz is meant to be orthonormal or not, as Givens paths only work if it is
  
  taus <- matrix(nrow = 5, ncol = 3)
  for (i in 2:4) {
    taus[i-1,] <- c(1, i, get_tau(Wz, 1, i))
    #Wz <- do.call(create.R, as.list(taus[i-1,])) %*% Wz
  }
  
  for(i in 3:4) {
    taus[i+1,] <- c(2, i, get_tau(Wz, 2, i))
    #Wz <- do.call(create.R, as.list(taus[i+1,])) %*% Wz
  }
  taus <- taus[nrow(taus):1,]
  taus[, 3] <- taus[, 3] * -1
  taus
  
}


get_rotations <- function(taus, B) {
  r <- diag(nrow = 4)
  for(i in 1:5) {
    r <- do.call(create.R, as.list(taus[i,])) %*% r
  }
  r <- solve(B %*% t(B)) %*% B %*% r
  r
}

get_sequence <- function(Fa, Fz, t) {
  preProj <- preprojection(Fa, Fz)
  Wa <- preProj[[3]]
  Wz <- preProj[[2]]
  B <- preProj[[1]]
  taus <- find_taus(Wz)
  temp <- taus
  sequence <- list(Fa)
  for (i in 1:t) {
    temp[, 3] <- taus[, 3] * i/t
    sequence[[i + 1]] <- get_rotations(temp, B) %*% Wa
  }
  sequence
}

get_tau <- function(X, i, j) {
  Arg(X[i,i]/(sqrt(X[i,i] ^ 2 + X[j,i] ^ 2)) + 1i * X[j,i]/(sqrt(X[i,i] ^ 2 + X[j,i] ^ 2)))
}

create.R <- function(i, j, tau, p=4) {
  # initialise R as an identity matrix of size p
  R <- diag(nrow = p)
  # replace elements to create a Givens rotation matrix
  R[i, i] <- cos(tau)
  R[j, j] <- cos(tau)
  R[i, j] <- sin(tau)
  R[j, i] <- -sin(tau)
  R
}

testRot <- function(Fa, Fz){
  preProj <- preprojection(Fa, Fz)
  Wa <- preProj[[3]]
  Wz <- preProj[[2]]
  B <- preProj[[1]]
  for(i in c(1,2)){
    for(j in (i+1):4){
      Wz <- create.R(i, j, get_tau(Wz, i, j)) %*% Wz
    }
  }
  print(Wz)
}


Fa <- basis_random(4)
Fz <- basis_random(4)
testRot(Fa, Fz)

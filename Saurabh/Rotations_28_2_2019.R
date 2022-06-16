preprojection <- function(Fa, Fz) {
  Fall <- cbind(Fa, Fz)
  Fnorm <- orthonormalise1(Fall)
  Fstar <- Fnorm[,3:4]
  B <- cbind(Fa, Fstar)
  Wa <- t(B) %*% Fa
  Wz <- t(B) %*% Fz
  list(B, Wz, Wa)
}

orthonormalise1 <- function(x) {
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

get_tau <- function(X, i, j) {
  Arg(X[i,i]/(sqrt(X[i,i] ^ 2 + X[j,i] ^ 2)) + 1i * X[j,i]/(sqrt(X[i,i] ^ 2 + X[j,i] ^ 2)))
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
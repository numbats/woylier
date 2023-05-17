#' Build a d-dimensional pre-projection space by orthonormalizing Fz with regard to Fa
#' @keywords internal
#' @param Fa starting pxd frame
#' @param Fz ending pxd frame
#' @returns B pre-projection px2d matrix
preprojection <- function(Fa, Fz) {
  # check both are matrices are both correct size
  stopifnot("Your inputs do not have the same number of columns!" = ncol(Fa) == ncol(Fz))
  stopifnot("Your inputs do not have the same number of row!" = nrow(Fa) == nrow(Fz))
  # check each is orthonormal
  stopifnot("The current frame must be orthonormal!" = tourr::is_orthonormal(Fa))
  stopifnot("The target frame must be orthonormal!" = tourr::is_orthonormal(Fz))
  Fz_star <- tourr::orthonormalise_by(Fz, Fa)
  B <- cbind(Fa, Fz_star)
  return(B)
}

#' Construct preprojected frames
#' @keywords internal
#' @param Fr Orthonormal frame
#' @param B pre-projection px2d matrix
#' @returns Preprojected 2dxd frame on preprojection space (first dxd entry of this matrix is identity matrix by construction)
construct_preframe <- function(Fr, B) {
  W <- t(B) %*% Fr
  return(W)
}

#' Compute the angle between 2d vectors 360 degrees
#' @keywords internal
#' @param x vector with length 2
#' @param y vector with length 2
#' @return angle in radians
angle2 <-  function(x, y){
  theta <- atan2(x[2], x[1]) - atan2(y[2], y[1])
  return(theta)
}
#' Takes i and k-th row of a matrix and rotate matrix by theta angle (requires matrix a to be 2*q matrix)
#' @keywords internal
#' @param a matrix
#' @param i row
#' @param k row that we want to zero the element
#' @param theta  angle between them
#' @return rotated matrix a
#' refer to Algorithm 5.1.6 of Matrix computation (Golub, Van)
row_rot <- function(a, i, k, theta) {
  n <- ncol(a)
  for (q in 1:n){
    x = a[i, q]
    y = a[k, q]
    a[i, q] = cos(theta)*x - sin(theta)*y
    a[k, q] = sin(theta)*x + cos(theta)*y
  }
  return(a)
}

#' Calculate angles of required rotations to map Wz to Wa
#' @keywords internal
#' @param Wa starting preprojected frame
#' @param Wz target preprojected frame
#' @return named list of angles
calculate_angles <- function(Wa, Wz) {
  angles = list()
  wi = Wz
  for (col in 1:ncol(Wz)) {
    for (row in col:(nrow(Wz)-1)){
      # store angles in a named list
      x <- as.matrix(c(Wa[col, col], Wa[row+1, col]))
      y <- as.matrix(c(wi[col, col], wi[row+1, col]))
      theta = angle2(x, y)
      angles[paste0(col, row +1)] = theta
      wi = row_rot(wi, col, row+1, theta)
    }
  }
  return(angles)
}


#' It implements series of Givens rotations that maps Wa to Wz
#' @keywords internal
#' @param Wa starting preprojected frame
#' @param angles angles of required rotations to map Wz to Wa
#' @param stepfraction for the interpolation of rotations
#' @return Givens path by stepfraction in pre-projected space
givens_rotation <- function(Wa, angles, stepfraction) {
  w_i = Wa
  for (col in ncol(Wa):1) {
    for (row in (nrow(Wa)-1):col){
      # rotating in reverse order
      index = paste0(col, row+1)
      theta = - as.numeric(angles[index])
      w_i = row_rot(w_i, col, row+1, theta*stepfraction)
    }
  }
  return(w_i)
}

#' Reconstruct interpolated frames using pre-projection
#' @keywords internal
#' @param B pre-projection px2d matrix
#' @param Wt A givens path by stepfraction
#' @returns A frame of on the step of interpolation

construct_moving_frame <- function(Wt, B) {
  Ft = B %*% Wt
  return(Ft)
}

#' Construct full interpolated frames
#' @param nsteps number of steps of interpolation
#' @param Fa starting pxd frame
#' @param Fz target pxd frame
#' @returns array with nsteps matrix. Each matrix is interpolated frame in between starting and target frames.
#' @export
#' @examples
#' p <- 4
#' base1 <- tourr::orthonormalise(tourr::basis_random(p, d=1))
#' base2 <- tourr::orthonormalise(tourr::basis_random(p, d=1))
#' path <- givens_full_path(base1, base2, nsteps=10)

givens_full_path <- function(Fa, Fz, nsteps) {
  B <- preprojection(Fa, Fz)
  Wa <- construct_preframe(Fa, B)
  Wz <- construct_preframe(Fz, B)
  angles <- calculate_angles(Wa, Wz)
  path <- array(dim = c(nrow(B), ncol(Wa), nsteps))
  for (i in 1:nsteps) {
    stepfraction <- i/nsteps
    Wt = givens_rotation(Wa, angles, stepfraction)
    Ft = construct_moving_frame(Wt, B)
    path[,,i] <- Ft
  }
  return(path)
}

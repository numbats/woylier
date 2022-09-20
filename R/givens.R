#' Build a d-dimensional pre-projection space by orthonormalizing Fz with regard to Fa
#'
#' @param Fa starting pxd frame
#' @param Fz ending pxd frame
#'
#' @returns B pre-projection px2d matrix 
#' @export
#'
#' @examples
#' 
#' p <- 4
#' base1 <- tourr::basis_random(p, d=1)
#' base2 <- tourr::basis_random(p, d=1)
#' b <- preprojection(base1, base2)

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
#'
#' @param Fr Orthonormal frame
#' @param B pre-projection px2d matrix 
#'
#' @returns Preprojected 2dxd frame on preprojection space (first dxd entry of this matrix is identity matrix by construction)
#' @export
#'
#' @examples 
#' 
#' p <- 4
#' base1 <- tourr::basis_random(p, d=1)
#' base2 <- tourr::basis_random(p, d=1)
#' b <- preprojection(base1, base2)
#' Wa <- construct_preframe(base1, b) 
#' Wz <- construct_preframe(base2, b) 
construct_preframe <- function(Fr, B) {
  W <- t(B) %*% Fr
  return(W)
}

#' Compute angle between two d-dimensional frames in p-space
#'
#' @param F1 pxd frame
#' @param F2 pxd frame
#'
#' @returns tau angle in radians
#' @export
#'
#' @examples 
#' 
#' p <- 4
#' base1 <- tourr::basis_random(p, d=1)
#' base2 <- tourr::basis_random(p, d=1)
#' b <- preprojection(base1, base2)
#' Wa <- construct_preframe(base1, b) 
#' Wz <- construct_preframe(base2, b) 
#' calculate_tau(Wz, Wa)
#' 
calculate_tau <- function(F1, F2) {
  # takes 2 vectors with 2 elements and calculate angle between them 
  # This needs to be generalised to frames instead of vectors
  # calculate the angle between 2 vectors 360 degrees in radians
  tau <- atan2(F1[2], F1[1]) - atan2(F2[2], F2[1]) 
  return(-tau)
}

#' Construct rotation matrix in given angle
#'
#' @param theta angle  of rotation
#' @returns rotation matrix
#' @export
#'
#' @examples 
#' 
#' theta <- 1
#' construct_rotation_matrix(theta)

construct_rotation_matrix <- function(theta){ 
  # rotate a 2d vector by given angle
  if (theta>0) {
    rotation_matrix <- matrix(c(cos(theta),-sin(theta),sin(theta), cos(theta)), nrow = 2, ncol = 2, byrow = TRUE)
  }
  # clockwise
  rotation_matrix <- matrix(c(cos(theta),sin(theta),-sin(theta), cos(theta)), nrow = 2, ncol = 2, byrow = TRUE)

  return(rotation_matrix)
}

#' construct tour path
#'
#' @param Wa starting basis in pre-projected space
#' @param tau angle between starting and target basis
#' @param stepfraction stepfraction in each interpolation
#'
#' @returns A givens path by stepfraction in pre-projected space
#' @export
#'
#' @examples 
#' p <- 4
#' base1 <- tourr::basis_random(p, d=1)
#' base2 <- tourr::basis_random(p, d=1)
#' b <- preprojection(base1, base2)
#' Wa <- construct_preframe(base1, b) 
#' Wz <- construct_preframe(base2, b) 
#' tau <- calculate_tau(Wz, Wa)
#' givens_path(Wa, tau, stepfraction=0.1)
givens_path <- function(Wa, tau, stepfraction) {
  # For now this will be a single rotation matrix
  # but at some generalised
  # this should compute an increment 
  # apply k (nsteps) times
  theta = tau*stepfraction
  Wt <- construct_rotation_matrix(theta) %*% Wa
  return(Wt)
}

#' Reconstruct interpolated frames using pre-projection
#'
#' @param B pre-projection px2d matrix 
#' @param Wt A givens path by stepfraction
#'
#' @returns A frame of on the step of interpolation
#' 
#' @export
#'
#' @examples 
#' p <- 4
#' base1 <- tourr::basis_random(p, d=1)
#' base2 <- tourr::basis_random(p, d=1)
#' b <- preprojection(base1, base2)
#' Wa <- construct_preframe(base1, b) 
#' Wz <- construct_preframe(base2, b) 
#' tau <- calculate_tau(Wz, Wa)
#' Wt <- givens_path(Wa, tau, stepfraction=0.1)
#' construct_moving_frame(Wt, b)
construct_moving_frame <- function(Wt, B) {
  Ft = B %*% Wt
  return(Ft)
}

#' Construct full interpolated frames
#'
#' @param nsteps number of steps of interpolation
#' @param Fa starting pxd frame
#' @param Fz target pxd frame
#'
#' @returns array with nsteps matrix. Each matrix is interpolated frame in between starting and target frames. 
#' @export
#'
#' @examples 
#' p <- 4
#' base1 <- tourr::orthonormalise(tourr::basis_random(p, d=1))
#' base2 <- tourr::orthonormalise(tourr::basis_random(p, d=1))
#' path <- givens_full_path(base1, base2, nsteps=10)

givens_full_path <- function(Fa, Fz, nsteps) {
  B <- preprojection(Fa, Fz)
  Wa <- construct_preframe(Fa, B)
  Wz <- construct_preframe(Fz, B)
  tau <- calculate_tau(Wz, Wa)
  path <- array(dim = c(nrow(B), ncol(Wa), nsteps))
  for (i in 1:nsteps) {
    stepfraction <- i/nsteps
    Wt <- givens_path(Wa, tau=tau, stepfraction)
    Ft = construct_moving_frame(Wt, B)
    path[,,i] <- Ft
  }
  return(path)
}
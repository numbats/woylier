#' Make a d-dimensional pre-projection space by orthonormalizing Fz with regard to Fa
#'
#' @param current starting pxd frame
#' @param target ending pxd frame
#'
#' @return B pre-projection px2d matrix 
#' @export
#'
#' @examples
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

#' Construct starting basis from pre-projection
#'
#' @param Fa starting pxd frame
#' @param B pre-projection px2d matrix 
#'
#' @return Wa starting 2dxd frame on preprojection space (first dxd entry of this matrix is identity matrix by construction)
#' @export
#'
#' @examples
construct_Wa <- function(Fa, B) {
  Wa <- t(B) %*% Fa
  return(Wa)
}

#' Construct target basis from pre-projection
#'
#' @param Fz target pxd frame
#' @param B pre-projection px2d matrix 
#'
#' @return Wz target 2dxd frame on preprojection space 
#' @export
#'
#' @examples
construct_Wz <- function(Fz, B) {
  Wz <- t(B) %*% Fz
  return(Wz)
}

#' Compute angle between two d-dimensional frames in p-space
#'
#' @param F1 pxd frame
#' @param F2 pxd frame
#'
#' @return tau angle in radians
#' @export
#'
#' @examples
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
#' @return rotation matrix
#' @export
#'
#' @examples
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
#' @return A givens path by stepfraction in pre-projected space
#' @export
#'
#' @examples
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
#' @return A frame of on the step of interpolation
#' @export
#'
#' @examples
construct_frame <- function(Wt, B) {
  Ft = B %*% Wt
  return(Ft)
}

#' Construct full interpolated frames
#'
#' @param B pre-projection px2d matrix 
#' @param Wa starting basis in pre-projected space
#' @param tau  angle between starting and target basis
#' @param nsteps number of steps of interpolation
#'
#' @return return array with nsteps matrix. Each matrix is interpolated frame in between starting and target frames. 
#' @export
#'
#' @examples
givens_full_path <- function(B, Wa, tau, nsteps) {
    path <- array(dim = c(nrow(B), ncol(Wa), nsteps))
    for (i in 1:nsteps) {
    stepfraction <- i/nsteps
    Wt <- givens_path(Wa, tau, stepfraction)
    Ft = construct_frame(Wt, B)
    path[,,i] <- Ft
  }
  return(path)
}



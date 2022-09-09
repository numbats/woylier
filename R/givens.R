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

  # stopifnot with message - dont need this because it is tested
  # Fa <- tourr::orthonormalise(Fa)
  # Fz <- tourr::orthonormalise(Fz)
  Fz_star <- tourr::orthonormalise_by(Fz, Fa)
  B <- cbind(Fa, Fz_star)
  return(B)
}

#' Construct starting basis from pre-projection
#'
#' @param Fa starting pxd frame
#' @param B pre-projection px2d matrix 
#'
#' @return Wa starting 2dxd frame on preprojection space (first dxd entry of this matrix is identity by construction)
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
  # calculate the angle between 2 vectors 360 degrees
  tau <- atan2(F1[2], F1[1]) - atan2(F2[2], F2[1]) 
  return(-tau)
}

#' construct rotation matrix
#'
#' @param theta angle  of rotation
#' @return
#' @export
#'
#' @examples
construct_rotation_matrix <- function(theta){ 
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

#' construct tour path
#'
#' @param Wa 
#' @param tau 
#' @param stepfraction 
#'
#' @return
#' @export
#'
#' @examples
givens_path <- function(Wa, tau, stepfraction) {
  # For now this will be a single rotation matrix
  # but at some generalised
  # this should compute an increment 
  # apply k (nsteps) times
  #fraction <- 1/nsteps
  #df <- data.frame(matrix(ncol = 2, nrow = nsteps)) # creates dataframe for plotting
  #df[1,] <-  Wz
  #a1 <- Wa # starts with the vector 1
  #for (i in 1:nsteps) {
  theta = tau*stepfraction
  Wt <- construct_rotation_matrix(theta) %*% Wa
  #  df[i+1,1] <- rotated[1]
  #  df[i+1,2] <- rotated[2]# update the dataframe
  #}
  return(Wt)
}

#' Reconstruct interpolated frames using pre-projection
#'
#' @param df nstep number of rotated basis
#' @param B pre-projection px2d matrix 
#'
#' @return array with c(p, d, nstep) dimensions
#' @export
#'
#' @examples
construct_frame <- function(Wt, B) {
  #result <- array(dim = c(nrow(B), ncol(B)/2, nrow(df)))
  #for (i in 1:nrow(df)) {
    #Wi = matrix(c(df[i, 1], df[i, 2]), nrow = 2, ncol = 1, byrow =TRUE)
    Ft = B %*% Wt
    #result[,,i] <- Fi
  #}
  return(Ft)
}

givens_full_path <- function(B, Wa, tau, nsteps) {
    path <- array(dim = c(nrow(B), ncol(Wa), nsteps))
    for (i in 1:nsteps) {
    stepfraction <- i/nsteps
    Wt <- givens_path(Wa, tau, stepfraction)
    Ft = B %*% Wt
    path[,,i] <- Ft
  }
  return(path)
}



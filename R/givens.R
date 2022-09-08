#' Make a d-dimenionsal preprojection space by orthonormalizing Fz with regard to Fa
#'
#' @param current starting pxd frame
#' @param target ending pxd frame
#'
#' @return B pre-projection 2dxd matrix 
#' @export
#'
#' @examples
preprojection <- function(Fa, Fz) {
  # check both are matrices are both correct size
  stopifnot("Your inputs do not have the same number of columns!" = ncol(Fa) == ncol(Fz))
  stopifnot("Your inputs do not have the same number of row!" = nrow(Fa) == nrow(Fz))
  # check each is orthonormal
  stopifnot("The current frame must be orthonormal!" = is_orthonormal(Fa))
  stopifnot("The target frame must be orthonormal!" = is_orthonormal(Fz))
  # stopifnot with message
  Fz_star <- tourr::orthonormalise_by(Fz, Fa)
  B <- cbind(Fa, Fz_star)
  return(B)
}

#' Construct starting basis from pre-projection
#'
#' @param Fa starting pxd frame
#' @param B pre-projection 2dxd matrix 
#'
#' @return Wa starting 2dxd frame on preprojection space
#' @export
#'
#' @examples
construct_Wa <- function(Fa, B) {
  Wa <- t(B) %*% Fa
  return(Wa)
}

#' Construct starting basis from pre-projection
#'
#' @param Fz starting pxd frame
#' @param B pre-projection 2dxd matrix 
#'
#' @return Wz starting 2dxd frame on preprojection space
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
  # This needs to be generalised to frames instead of vectors
  # calculate the angle between 2 vectors 360 degrees
  tau <- atan2(F1[2], F1[1]) - atan2(F2[2], F2[1]) 
  return(tau)
}

compute_rotation <- function(Wa, tau, nsteps, stepfraction) {
  # For now this will be a single rotation matrix
  # but at some generalised
  # this should compute an increment 
  # apply k (nsteps) times
}

revert_to_original <- function() {
  # Fa = BWa
  # Fz = BWz increment
}



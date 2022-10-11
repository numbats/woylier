#' Interpolation format for tourr
#' 
#' Generates the interpolation in the form needed for
#' tourr, modelled on geodesic_path()
#'
#' @param current starting frame
#' @param target target frame
#' @param frozen indicator whether some dimensions fixed 
#' @param ... 
#' @keywords internal
#' @export
#' @return
#'   \item{interpolate}{A function with single parameter in [0, 1] that
#'     returns an interpolated frame between the current and future frames.
#'     0 gives the current plane, 1 gives the new target frame in plane of
#'     current frame.}
#'   \item{dist}{The distance, in radians, between the current and target
#'     frames.}
#'  \item{Fa}{The current frame.}
#'  \item{Fz}{The new target frame.}
#'  \item{tau}{The principle angles between the current and target frames.}
#'  \item{Ga}{The current plane.}
#'  \item{Gz}{The target plane.}
#'
givens_path <- function (current, target, frozen = NULL, ...) {
  if (is.null(frozen)) {
    # Regular geodesic
    givens <- givens_info(current, target)
    
    interpolate <- function(pos) {
      givens_step_fraction(givens, pos)
    }
  } else {
    cat("Givens path cannot handle frozen yet \n")
  }
  
  list(
    interpolate = interpolate,
    Fa = current,
    Fz = target,
    Ga = givens$Wa,
    Gz = givens$Wz,
    tau = givens$tau,
    dist = proj_dist(current, target)
  )
}

#' Calculate information needed for Givens interpolation
#' 
#' The methdology is outlined in
#' \url{http://www-stat.wharton.upenn.edu/~buja/PAPERS/paper-dyn-proj-algs.pdf}
#'
#' @param Fa starting frame, will be orthonormalised if necessary
#' @param Fz target frame, will be orthonormalised if necessary
#' @keywords internal
#'
givens_info <- function(Fa, Fz) {
  if (!is_orthonormal(Fa)) {
    # message("Orthonormalising Fa")
    Fa <- orthonormalise(Fa)
  }
  if (!is_orthonormal(Fz)) {
    # message("Orthonormalising Fz")
    Fz <- orthonormalise(Fz)
  }
  B <- preprojection(Fa, Fz)
  Wa <- construct_preframe(Fa, B)
  Wz <- construct_preframe(Fz, B)
  angles <- calculate_angles(Wa, Wz)
  
  list(B = B, Wa = Wa, Wz = Wz, tau = angles)
}

#' Step along a Givens interpolated path by fraction of path length.
#'
#' @keywords internal
#' @param interp interpolated path
#' @param fraction fraction of distance between start and end frames
#' 
givens_step_fraction <- function(interp, fraction) {
  # Interpolate between starting and end frames
  #  - must multiply column wise (hence all the transposes)
  Wt = givens_rotation(interp$Wa, interp$angles, fraction)
  Ft = construct_moving_frame(Wt, interp$B)
  
  return(Ft)
}
#' Interpolation format for tourr
#'
#' Generates the interpolation in the form needed for
#' tourr, modelled on geodesic_path()
#' @param current starting frame
#' @param target target frame
#' @param frozen indicator whether some dimensions fixed
#' @param ... arguments sent to later functions
#' @keywords internal
#' @export
#' @return
#'   \item{interpolate}{A function with single parameter in \[0, 1\] that
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
givens_path <- function (current, target, frozen = NULL, ...) {
  if (is.null(frozen)) {
    # Regular geodesic
    givens <- givens_info(current, target)

    interpolate <- function(pos) {
      givens_step_fraction(givens, pos)
    }
  } else {
    message("Givens path cannot handle frozen yet \n")
  }

  list(
    interpolate = interpolate,
    Fa = current,
    Fz = target,
    Ga = givens$Wa,
    Gz = givens$Wz,
    tau = givens$tau,
    dist = tourr::proj_dist(current, target)
  )
}

#' Calculate information needed for Givens interpolation
#' The methodology is outlined in
#' \url{http://www-stat.wharton.upenn.edu/~buja/PAPERS/paper-dyn-proj-algs.pdf}
#' @param Fa starting frame, will be orthonormalized if necessary
#' @param Fz target frame, will be orthonormalized if necessary
#' @keywords internal
givens_info <- function(Fa, Fz) {
  if (!tourr::is_orthonormal(Fa)) {
    # message("Orthonormalising Fa")
    Fa <- tourr::orthonormalise(Fa)
  }
  if (!tourr::is_orthonormal(Fz)) {
    # message("Orthonormalising Fz")
    Fz <- tourr::orthonormalise(Fz)
  }
  B <- preprojection(Fa, Fz)
  Wa <- construct_preframe(Fa, B)
  Wz <- construct_preframe(Fz, B)
  angles <- calculate_angles(Wa, Wz)

  list(B = B, Wa = Wa, Wz = Wz, tau = angles)
}

#' Step along a Givens interpolated path by fraction of path length.
#' @param interp interpolated path
#' @param fraction fraction of distance between start and end frames
#' @keywords internal
givens_step_fraction <- function(interp, fraction) {
  # Interpolate between starting and end frames
  #  - must multiply column wise (hence all the transposes)
  Wt = givens_rotation(interp$Wa, interp$tau, fraction)
  Ft = construct_moving_frame(Wt, interp$B)
  return(Ft)
}

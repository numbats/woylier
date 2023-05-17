#' Generate the background sphere or torus
#' @param n number of points on the sphere
#' @param p dimension of data
#' @param d dimension of projection
#' @return n number of points on the surface of sphere
#' @export
#' @examples
#' p <- 4
#' sp <- generate_space_view(p=p)
generate_space_view <- function(n=1000, p=3, d=1) {
# this will generate the background sphere or torus
  if (d == 1) {
    proj_space <- geozoo::sphere.hollow(n=n, p=p)$points
  }
  else {
    # proj_space <- geozoo::torus(n, p=2*d)$points
  }
  proj_space <- as.data.frame(proj_space)
  return(proj_space)
}

#' Overlay paths of interpolation on the sphere
#' @param proj_space  n number of points on the surface of sphere
#' @param path interpolated path
#' @return data frame with interpolated path and points on sphere surface
#' @export
#' @examples
#' p <- 4
#' base1 <- tourr::basis_random(p, d=1)
#' base2 <- tourr::basis_random(p, d=1)
#' path <- givens_full_path(base1, base2, nsteps=10)
#' sp <- generate_space_view(p=p)
#' sp_path <- add_path(sp, path)
#' tourr::animate_xy(sp_path[,1:4], col=sp_path$type)
add_path <- function(proj_space, path) {
  # overlay sequence of dots for the path
  proj_space$type <- "proj_space"
  path <- as.data.frame(t(apply(path, 3, c)))
  path$type <- "path"
  space_and_path <- rbind(proj_space, path)
  return(space_and_path)
}

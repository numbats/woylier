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

add_path <- function(proj_space, path) {
  # overlay sequence of dots for the path
  proj_space$type <- "proj_space"
  path <- as.data.frame(t(apply(path, 3, c)))
  path$type <- "path"
  space_and_path <- rbind(proj_space, path)
}
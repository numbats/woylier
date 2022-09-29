library(tourr)

# example
set.seed(2022)
p <- 4
base1 <- tourr::basis_random(p, d=1)
base2 <- tourr::basis_random(p, d=1)
base3 <- tourr::basis_random(p, d=1)

# first example

frames <- givens_full_path(base1, base2, nsteps = 10)

sp <- generate_space_view(p=p)

sp_path <- add_path(sp, frames) 

point1 <- as.data.frame(t(base1)) 
point1$type <- "point1"

point2 <- as.data.frame(t(base2))
point2$type <- "point2"

sp_path <- rbind(sp_path, point1, point2) 

#tourr::animate_xy(sp_path[,1:p], col=sp_path$type, axes="bottomleft")

# second example

frames <- givens_full_path(base2, base3, nsteps = 10)

frames <- as.data.frame(t(apply(frames, 3, c)))

frames$type <- "path"
sp_path <- rbind(sp_path, frames)

point3 <- as.data.frame(t(base3))
point3$type <- "point3"

sp_path <- rbind(sp_path, point3) 

tourr::animate_xy(sp_path[,1:p], col=sp_path$type, 
                  axes="bottomleft")

# Generate 2D example

orthonormalise_by <- function(x, by) {
  stopifnot(ncol(x) == ncol(by))
  stopifnot(nrow(x) == nrow(by))
  
  x <- normalise(x)
  by <- normalise(by)
  
  for (j in seq_len(ncol(x))) {
    x[, j] <- x[, j] - as.vector(crossprod(x[, j], by[, j])) * by[, j]
    for (k in seq_len(ncol(by))) {
      x[, j] <- x[, j] - as.vector(crossprod(x[, j], by[, k])) * by[, k]
      x[, j] <- normalise(x[, j])
    }
  }
  normalise(x)
  # Last step, columns new matrix to orthonormal
  if (ncol(x) > 1) {
    for (j in 2:ncol(x)) {
      x[, j] <- x[, j] - as.vector(crossprod(x[, j], x[, j-1])) * x[, j-1]
      normalise(x[, j])
    }
  }
  return(x)
}

set.seed(2022)
p <- 6
base1 <- tourr::basis_random(p, d=2)
base2 <- tourr::basis_random(p, d=2)

frames_2d <- givens_full_path(base1, base2, 10)

frames_2d[,,10]

base2

# Check numbers
sum(b[,1]^2)
sum(b[,2]^2)
sum(b[,3]^2)
sum(b[,4]^2)
t(b[,1]%*%b[,3])
t(b[,2]%*%b[,4])
det(t(b[,1:2])%*%b[,3:4])

Wa <- construct_preframe(base1, b) 
Wz <- construct_preframe(base2, b) 
sum(Wa[,1]^2)
sum(Wa[,2]^2)
sum(Wz[,1]^2)

# Testing torus construction
n <- 1000
p <- 6
d <- 2

sp <- data.frame(geozoo::torus.flat(p = p, n=n)$points)
tourr::animate_xy(sp)
#tourr::animate_slice(sp)

# Check on torus
proj_2d <- map(1:n, ~basis_random(n = p,  d=d)) %>%
  purrr::flatten_dbl() %>% 
  matrix(ncol = p, byrow = TRUE) %>%
  as_tibble()
tourr::animate_xy(proj_2d)

# Path
path_2d <- apply(frames_2d, 1, c) %>% as.data.frame()
tourr::animate_xy(path_2d)

# Join
proj_2d <- proj_2d %>% mutate(type="torus")
path_2d <- path_2d %>% mutate(type="path")
proj_path <- bind_rows(proj_2d, path_2d)
tourr::animate_xy(proj_path[,1:6], col=proj_path$type)

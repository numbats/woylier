
<!-- README.md is generated from README.Rmd. Please edit that file -->

# woylier

<!-- badges: start -->
<!-- badges: end -->

The "woylier" package provides alternative method for generating a tour path by interpolating between d-D frames in p-D space rather than d-D planes. 

## Installation

You can install the development version of woylier from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("numbats/woylier")
```

## Example

This example illustrates 1D visualization of the interpolation of 3 points in 6D. 

```{r setup}
library(woylier)
```

```{r}
# Generate 1D example

set.seed(2022)
p <- 6
base1 <- tourr::basis_random(p, d=1)
base2 <- tourr::basis_random(p, d=1)
base3 <- tourr::basis_random(p, d=1)

# First example

frames <- givens_full_path(base1, base2, nsteps = 10)

sp <- generate_space_view(p=p)

sp_path <- add_path(sp, frames) 

point1 <- as.data.frame(t(base1)) 
point1$type <- "point1"

point2 <- as.data.frame(t(base2))
point2$type <- "point2"

sp_path <- rbind(sp_path, point1, point2) 

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
```
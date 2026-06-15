woylier: tour methods for multivariate data visualisation
================
Zola Batsaikhan, Dianne Cook, Ursula Laa
<br> June 15, 2026

<!-- README.md is generated from README.Rmd. Please edit that file -->

# woylier <a href='https://numbats.github.io/woylier/'><img src='man/figures/logo.png' align="right" height="138.5" /></a>

<!-- badges: start -->

<!-- badges: end -->

The “woylier” package provides alternative method for generating a tour
path by interpolating between d-D frames in p-D space rather than d-D
planes. A tour path is a sequence of projection and we use interpolation
method to produce the path. The package uses geodesic interpolation
between planes. Geodesic interpolation path is the locally shortest path
between planes with no within-plane spin. As a result of this method,
the rendered target plane could be the rotated version of the target
plane we wanted. This is not a problem when the structure we are looking
can be identified without turning the axis around.

The “woylier” package implements the Givens interpolation paths method
proposed by [Buja et
al. (2005)](https://doi.org/10.1016/S0169-7161(04)24014-7) in R. This
algorithm adapts Given’s matrix decomposition technique which allows the
interpolation to be between frames rather than planes.

## Installation

The package is available on CRAN, and you can install the development
version of woylier from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("numbats/woylier")
```

## Getting started

``` r
library(woylier)
library(geozoo)
library(ggplot2)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(purrr)
```

# Path 1

In this example, we have 2 random 1D basis in 6D data space and the
`givens_full_path()` function returns the intermediate interpolation
step projections in given number of steps. The code chunk below
demonstrates the interpolation between 2 random basis in 5 steps.

``` r
# Generate 1D example

set.seed(2022)
p <- 6
base1 <- tourr::basis_random(p, d=1)
base2 <- tourr::basis_random(p, d=1)

base1
#>             [,1]
#> [1,]  0.24406482
#> [2,] -0.31814139
#> [3,] -0.24334450
#> [4,] -0.39166263
#> [5,] -0.08975114
#> [6,] -0.78647758

base2
#>            [,1]
#> [1,] -0.6256883
#> [2,]  0.1641842
#> [3,]  0.4427114
#> [4,]  0.1426996
#> [5,]  0.5943406
#> [6,] -0.1093633

givens_full_path(base1, base2, nsteps = 5)
#> , , 1
#> 
#>             [,1]
#> [1,]  0.24406482
#> [2,] -0.31814139
#> [3,] -0.24334450
#> [4,] -0.39166263
#> [5,] -0.08975114
#> [6,] -0.78647758
#> 
#> , , 2
#> 
#>             [,1]
#> [1,]  0.01086858
#> [2,] -0.27240617
#> [3,] -0.08264232
#> [4,] -0.35891689
#> [5,]  0.14040479
#> [6,] -0.87767429
#> 
#> , , 3
#> 
#>             [,1]
#> [1,] -0.22389989
#> [2,] -0.18726515
#> [3,]  0.09001476
#> [4,] -0.27425086
#> [5,]  0.35025000
#> [6,] -0.84190816
#> 
#> , , 4
#> 
#>             [,1]
#> [1,] -0.42627939
#> [2,] -0.07503468
#> [3,]  0.24965046
#> [4,] -0.14991218
#> [5,]  0.50942866
#> [6,] -0.68435306
#> 
#> , , 5
#> 
#>              [,1]
#> [1,] -0.566994057
#> [2,]  0.048050183
#> [3,]  0.373172156
#> [4,] -0.003887462
#> [5,]  0.594914254
#> [6,] -0.427800630
#> 
#> , , 6
#> 
#>            [,1]
#> [1,] -0.6256883
#> [2,]  0.1641842
#> [3,]  0.4427114
#> [4,]  0.1426996
#> [5,]  0.5943406
#> [6,] -0.1093633
```

# Path 2

In this example, we have 2 random 2D basis in 6D data space and the
`givens_full_path()` function returns the intermediate interpolation
step projections in given number of steps. The code chunk below
demonstrates the interpolation between 2 random basis in 5 steps.

``` r
# Generate 2D example

set.seed(2022)
p <- 6
base3 <- tourr::basis_random(p, d=2)
base4 <- tourr::basis_random(p, d=2)

base3
#>             [,1]        [,2]
#> [1,]  0.24406482 -0.57724655
#> [2,] -0.31814139  0.06085804
#> [3,] -0.24334450  0.38323969
#> [4,] -0.39166263  0.01182949
#> [5,] -0.08975114  0.59899558
#> [6,] -0.78647758 -0.39657839

base4
#>             [,1]        [,2]
#> [1,] -0.64550501 -0.17034478
#> [2,]  0.06108262  0.87051018
#> [3,] -0.03470326  0.26771612
#> [4,] -0.05281183  0.25452167
#> [5,] -0.43004248  0.27472455
#> [6,] -0.62502981  0.03560765

givens_full_path(base3, base4, nsteps = 5)
#> , , 1
#> 
#>             [,1]        [,2]
#> [1,]  0.24406482 -0.57724655
#> [2,] -0.31814139  0.06085804
#> [3,] -0.24334450  0.38323969
#> [4,] -0.39166263  0.01182949
#> [5,] -0.08975114  0.59899558
#> [6,] -0.78647758 -0.39657839
#> 
#> , , 2
#> 
#>             [,1]        [,2]
#> [1,]  0.02498501 -0.57109655
#> [2,] -0.26080833  0.26281744
#> [3,] -0.19820064  0.40439307
#> [4,] -0.35542927  0.08342651
#> [5,] -0.14433023  0.57634009
#> [6,] -0.86308174 -0.31955295
#> 
#> , , 3
#> 
#>            [,1]       [,2]
#> [1,] -0.1909937 -0.5292778
#> [2,] -0.1874044  0.4552848
#> [3,] -0.1459678  0.4048872
#> [4,] -0.2970111  0.1523640
#> [5,] -0.2003186  0.5263904
#> [6,] -0.8824688 -0.2198760
#> 
#> , , 4
#> 
#>             [,1]       [,2]
#> [1,] -0.38527579 -0.4462372
#> [2,] -0.10411664  0.6265334
#> [3,] -0.09577045  0.3815665
#> [4,] -0.22183655  0.2092198
#> [5,] -0.26412984  0.4538619
#> [6,] -0.84414115 -0.1138933
#> 
#> , , 5
#> 
#>             [,1]       [,2]
#> [1,] -0.54115467 -0.3240743
#> [2,] -0.01855341  0.7665407
#> [3,] -0.05630484  0.3348197
#> [4,] -0.13748432  0.2454803
#> [5,] -0.34020920  0.3668276
#> [6,] -0.75431619 -0.0215393
#> 
#> , , 6
#> 
#>             [,1]        [,2]
#> [1,] -0.64550501 -0.17349072
#> [2,]  0.06108262  0.86870187
#> [3,] -0.03470326  0.26920160
#> [4,] -0.05281183  0.25552018
#> [5,] -0.43004248  0.27581107
#> [6,] -0.62502981  0.03776552
```

# Examples

To see how to use this for examining projections of multivariate data,
see [Batsaikhan, Cook, Laa
(2024)](https://doi.org/10.48550/arXiv.2311.08181).

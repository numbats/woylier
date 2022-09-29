% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/givens.R
\name{construct_preframe}
\alias{construct_preframe}
\title{Construct preprojected frames}
\usage{
construct_preframe(Fr, B)
}
\arguments{
\item{Fr}{Orthonormal frame}

\item{B}{pre-projection px2d matrix}
}
\value{
Preprojected 2dxd frame on preprojection space (first dxd entry of this matrix is identity matrix by construction)
}
\description{
Construct preprojected frames
}
\examples{

p <- 4
base1 <- tourr::basis_random(p, d=1)
base2 <- tourr::basis_random(p, d=1)
b <- preprojection(base1, base2)
Wa <- construct_preframe(base1, b) 
Wz <- construct_preframe(base2, b) 
}
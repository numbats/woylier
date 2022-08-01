# woylier

The purpose of this R package is to provide an alternative to geodesic interpolation between planes in a tour, to interpolate between specific bases. 

Background reading:

- Buja et al "Computational Methods
for High-Dimensional Rotations in Data Visualization" (background/paper-dyn-proj-algs.pdf) 
- also check the wikipedia page on Givens rotations https://en.wikipedia.org/wiki/Givens_rotation
- https://github.com/uschiLaa/paper-ppi _main.pdf application of using the splines index to find nonlinear relationships
- maybe https://www.tandfonline.com/doi/abs/10.1080/1351847X.2019.1647864 or https://link.springer.com/article/10.1007/s10957-020-01664-3
- Read the geodesic.R code of the tourr package, which matches (https://github.com/ggobi/tourr)
- Request at https://github.com/ggobi/tourr/issues/110 motivating this project

R functions: 

- Saurabh/Rotations_1_3_2019.R contains code to do the basis to basis rotation

Tasks:

- Create an R package file structure in the repo
- Add function that matches structure of geodesic_path() from tourr, so this method of interpolation can be swapped in
- Apply to projection pursuit of splines index to search for nonlinear associations
- Write a short R Journal format article describing the interpolation methods, and gives an example use

# Generate sine wave data
library(tidyverse)
set.seed(5543)
sinData <- function(n, p){
  vName <- paste0("V",n)
  vNameM1 <- paste0("V",n-1)
  expr <- paste0(vName,"=sin(",vNameM1,")") # need string expression if I want to use tibble here
  dRet <- as_tibble(matrix(rnorm((n-1)*p), ncol=(n-1))) #generate normal distributed n-1 dim data
  dRet <- mutate_(dRet, expr) #string evaluation calculates var(n) as tan(var(n-1))
  colnames(dRet)[n] <- vName #correct name of new variable
  dRet[vName] <- jitter(dRet[[vName]]) #adding noise
  return(dRet)
}

sine_curve <- sinData(500, 6)
library(GGally)
ggscatmat(sine_curve)

save(sine_curve, file="data/sine_curve.rda")

# Test out calculations
library(ggplot2)
library(woylier)
library(tourr)
library(patchwork)
data("sine_curve")
mat <- sine_curve[,5:6]
mat_idx <- round(tourr::splines2d()(mat), 2)
mat_rot <- data.frame(x = cos(pi/6) * sine_curve$V5 + 
                          sin(pi/6) * sine_curve$V6,
                      y = -sin(pi/6) * sine_curve$V5 + 
                           cos(pi/6) * sine_curve$V6)
mat_rot_idx <- round(tourr::splines2d()(mat_rot), 2)

p1 <- ggplot(mat, aes(x=V5, y=V6)) + 
  geom_point() + 
  ggtitle(paste("Splines index = ", mat_idx)) +
  theme(aspect.ratio=1)

p2 <- ggplot(mat_rot, aes(x=x, y=y)) + 
  geom_point() + 
  xlab("Rotated 1") + ylab("Rotated 2") +
  ggtitle(paste("Splines index = ", mat_rot_idx)) +
  theme(aspect.ratio=1)

p1+p2


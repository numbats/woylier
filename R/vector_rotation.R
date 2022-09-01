library(plotly)
library(ggplot2)
library(geozoo)
library(tourr)
library(tidyverse)

angle2 <-  function(x, y){ 
  # calculate the angle between 2d vectors 360 degrees
  theta <- atan2(x[2], x[1]) - atan2(y[2], y[1]) 
  return(theta)
}

simple_rotation <- function(x, theta){ 
  # rotate a 2d vector by given angle
  if (theta>0) {
    rotation_matrix <- matrix(c(cos(theta),-sin(theta),sin(theta), cos(theta)), nrow = 2, ncol = 2, byrow = TRUE)
    x_rotated <- rotation_matrix%*%x
  }
  # clockwise
  rotation_matrix <- matrix(c(cos(theta),sin(theta),-sin(theta), cos(theta)), nrow = 2, ncol = 2, byrow = TRUE)
  x_rotated <- rotation_matrix%*%x
  return(x_rotated)
}

rotate2_2d <- function(v1, v2, k, i, j){
  v1 <- normalise(v1)
  v2 <- normalise(v2)
  a <- as.matrix(c(v1[i],v1[j]))
  b <- as.matrix(c(v2[i],v2[j]))
  a_len <- sqrt(sum(a^2))
  b_len <- sqrt(sum(b^2))
  a_norm <- a/a_len
  b_norm <- b/b_len
  theta <- angle2(a, b) # calculate the angle between the two
  df <- data.frame(matrix(ncol = 2, nrow = k)) # creates dataframe for plotting
  df[1,] <-  a
  a1 <- a # starts with the vector 1
  for (m in 2:(k+1)){
    # rotates k times 
    a1 <- simple_rotation(a1, theta/k)
    df[m,1] <- a1[1]
    df[m,2] <- a1[2]# update the dataframe
  }
  colnames(df) <-c(paste0("X", i), paste0("X", j)) # i and j th elements as plane cooordinates
  return(df)
}

pairwise_rotation <- function(v1, v2, k) {
  p = length(v1)
  if ((p %% 2) == 0){
    df = data.frame(matrix(nrow = k+1)) # creates dataframe for plotting
    for (n in 1:(p/2)){
      df_each <- rotate2_2d(v1, v2, k, (2*n-1), (2*n))
      df <- cbind(df, df_each) 
    }
    df <- df[,-1]
    return(df)
  }
  else {
    print("odd")
  }
}

set.seed(2022)
base1 <- tourr::basis_random(4, d=1)
base2 <- tourr::basis_random(4, d=1)

# create sphere
sp <- sphere.hollow(p = 4, n = 6000)
sp <- data.frame(sp$points)
sp <- sp %>%
  mutate(id = "sphere")

# create path df
df <- pairwise_rotation(base1, base2, 80) %>% 
  mutate(id ="path")

point1 <- data.frame(t(base1)) %>% 
  mutate(id ="point1")

point2 <- data.frame(t(base2)) %>% 
  mutate(id ="point2")
  
path_sphere <- bind_rows(sp, df, point1, point2) 
# plot
animate_xy(path_sphere[,1:4], col=path_sphere$id, axes = "off")


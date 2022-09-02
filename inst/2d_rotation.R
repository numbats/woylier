library(plotly)
library(ggplot2)
library(geozoo)
library(tourr)
library(tidyverse)

angle2 <-  function(x, y){ 
  # calculate the angle between 2 vectors 360 degrees
  theta <- atan2(x[2], x[1]) - atan2(y[2], y[1]) 
  return(theta)
}

simple_rotation <- function(x, theta){ 
  if (theta>0) {
  # rotate a vector by given radian counterclockwise
  x_1 <- x[1]*cos(theta) - x[2]*sin(theta)
  x_2 <- x[1]*sin(theta) + x[2]*cos(theta)
  }
  # rotate a vector clockwise
  x_1 <- x[1]*cos(theta) + x[2]*sin(theta)
  x_2 <- - x[1]*sin(theta) + x[2]*cos(theta)
  x_rotated <- as.matrix(c(x_1, x_2))
  return(x_rotated)
}

rotate_2d_alt <- function(a, b, k){
  theta <- angle2(a, b)
  df <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(df) <-c("x", "y")
  df[1,] <- a
  a1 <- a
  for (i in 2:k){
    a1 <- simple_rotation(a1, theta/(k-1))
    df[i,] <- a1
  }
  fig <- df %>%
    plot_ly(
      x = ~x,
      y = ~y,
      type = 'scatter',
      mode = 'markers',
      showlegend = F
    )
  fig
}

x <- as.matrix(c(1,0))
y <- as.matrix(c(0,1))
t <- as.matrix(c(3,1))

#z <- simple_rotation(x, a)


rotate_2d_alt(x, y, 10)


rotate_2_element <- function(v1, v2, k, i, j){
  # rotates i and jth elements of vector v1 to the corresponding elements of vector v2 with k steps
  # v1 and v2 are given vectors
  # i and j are the elements we want to rotate
  # k is the number of steps
  a <- normalise(as.matrix(c(v1[i],v1[j])))
  b <- normalise(as.matrix(c(v2[i],v2[j])))
  theta <- angle2(a, b) # calculate the angle between the two
  df <- data.frame(matrix(ncol = 2, nrow = 0)) # creates dataframe for plotting
  colnames(df) <-c("x", "y") # i and j th elements as plane cooordinates
  df[1,] <- a
  a1 <- a # starts with the vector 1
  for (i in 2:k){
    # rotates k times 
    a1 <- simple_rotation(a1, theta/(k-1))
    df[i,] <- a1 # update the dataframe
  }
  fig <- df %>%
    # plotting
    plot_ly(x = ~x,y = ~y, type = 'scatter', mode = 'markers', showlegend = F
    ) %>% 
    add_segments(xend = ~x, yend = ~y, x = 0, y = 0)
  print(fig)
}

vector_1 <- as.matrix(c(2,2, 3, 7))
vector_2 <- as.matrix(c(2,100, 4, 8))

rotate_2_element(vector_1, vector_2, 10, 2, 3)

vector_1 <- as.matrix(c(2,2, 3))
vector_2 <- as.matrix(c(2,10, 4))
angle2(vector_2, vector_1)
angle2(normalise(vector_1), normalise(vector_2))


row_rot <- function(a, theta) {
  # takes a matrix and angle rotate rows by given angle
  n <- ncol(a)
  for (i in 1:n){
    x = a[1, i]
    y = a[2, i]
    a[1, i] = cos(theta)*x - sin(theta)*y
    a[2, i] = cos(theta)*x + sin(theta)*y
  }
  return(a)
}

col_rot <- function(a, theta) {
  # takes a matrix and angle rotate columns by given angle
  n <- nrow(a)
  for (i in 1:n){
    x = a[i, 1]
    y = a[i, 2]
    a[i, 1] = cos(theta)*x - sin(theta)*y
    a[i, 2] = cos(theta)*x + sin(theta)*y
  }
  return(a)
}

rotate2 <- function(v1, v2, k, i, j){
  v1 <- normalise(v1)
  v2 <- normalise(v2)
  a <- as.matrix(c(v1[i],v1[j]))
  b <- as.matrix(c(v2[i],v2[j]))
  theta <- angle2(a, b) # calculate the angle between the two
  df <- data.frame(matrix(ncol = 2, nrow = k)) # creates dataframe for plotting
  colnames(df) <-c("V1", "V2") # i and j th elements as plane cooordinates
  df[1,] <- a
  a1 <- a # starts with the vector 1
  for (m in 2:k){
    # rotates k times 
    a1 <- simple_rotation(a1, theta/(k-1))
    df[m,i] <- a1[1]
    df[m,j] <- a1[2]# update the dataframe
  }
  df <- df %>% 
    mutate(V3 = v1[3]) 
  return(df)
}

v1 <- normalise(as.matrix(c(0.1961161, 0.9805807, 0.7276069)))
v2 <- normalise(as.matrix(c(0.7071068,0.7071068, 0.3651484)))
v3 <- normalise(as.matrix(c(1, 1000, 1)))
v4 <- as.matrix(c(1,0, 0))

# create sphere
sp <- sphere.hollow(p = 4, n = 1500)
sp <- data.frame(sp$points)
sp <- sp %>%
  rename(V1=X1, V2=X2, V3=X3) %>%
  mutate(id = "sphere")

# create path df
df <- rotate2(v2, v1, 100, 1, 2) %>% 
  mutate(id ="path")

path_sphere <- bind_rows(sp, df)

# plot
animate_xy(path_sphere[,1:3], col=path_sphere$id)

df2 <- rotate2(v3, v4, 100, 1, 2) %>% 
  mutate(id ="path")

path_sphere2 <- bind_rows(sp, df2)

animate_xy(path_sphere2[,1:3], col=path_sphere$id)

path_sphere <- path_sphere %>% 
  mutate(length = sqrt(V1^2 + V2^2 +V3^2))

path_sphere %>% 
  filter(id == "path")

seed(2022)
tourr::basis_random(10, d=1)
tourr::basis_random(10, d=1)

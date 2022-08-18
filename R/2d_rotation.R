library(plotly)
library(ggplot2)

angle <-  function(x, y){ 
  # calculate the angle between 2 vectors
  theta <- acos(  sum(x*y) / ( sqrt(sum(x * x)) * sqrt(sum(y * y))))
  return(theta)
}

simple_rotation <- function(x, theta){ 
  # rotate a vector by given radian
  x_1 <- x[1]*cos(theta) - x[2]*sin(theta)
  x_2 <- x[1]*sin(theta) + x[2]*cos(theta)
  x_rotated <- as.matrix(c(x_1, x_2))
  return(x_rotated)
}

rotate_2d <- function(a, b, k){
  theta <- angle(a, b)
  df <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(df) <-c("x", "y")
  df[1,] <- a
  a1 <- a
  for (i in 2:k){
    a1 <- simple_rotation(a1, theta/(k-1))
    df[i,] <- a1
    print(ggplot(df, aes(x=x, y=y)) + geom_point())
    Sys.sleep(1)
  }
}

rotate_2d_alt <- function(a, b, k){
  theta <- angle(a, b)
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

x <- as.matrix(c(2,1))
y <- as.matrix(c(1,2))
t <- as.matrix(c(3,1))

z <- simple_rotation(x, a)

a <- angle(x, y)

rotate_2d(x, t, 10)

rotate_2d_alt(x, y, 100)


rotate_2_element <- function(v1, v2, k, i, j){
  # rotates i and jth elements of vector v1 to the corresponding elements of vector v2 with k steps
  # v1 and v2 are given vectors
  # i and j are the elements we want to rotate
  # k is the number of steps
  a <- as.matrix(c(v1[i],v1[j]))
  b <- as.matrix(c(v2[i],v2[j]))
  theta <- angle(a, b) # calculate the angle between the two
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
vector_2 <- as.matrix(c(1,-1.5, 4, 8))

rotate_2_element(vector_1, vector_2, 100, 2, 3)

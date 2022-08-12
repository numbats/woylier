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

rotate_2d_alt(x, t, 100)

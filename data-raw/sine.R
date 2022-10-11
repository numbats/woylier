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

sine_curve <- as_tibble(t(sinData(500, 6)))
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

# Generate a sample interpolation
set.seed(5543)
base1 <- tourr::orthonormalise(tourr::basis_random(6, d=2))
base2 <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1), ncol=2, byrow=T)
sine_path <- givens_full_path(base1, base2, nsteps=100)
#class(sine_path) <- "history_array"

#animate_xy(sine_curve, planned_tour(sine_path))
# Doesn't respect the specific frame

# Try with plotly
library(plotly)
library(tidyverse)
sine_plotly <- NULL
for (i in 1:dim(sine_path)[3]) {
  d <- as.matrix(sine_curve) %*% as.matrix(sine_path[,,i])
  d <- data.frame(d)
  d$idx <- round(tourr::splines2d()(d), 2)
  d$frame <- i
  sine_plotly <- bind_rows(sine_plotly, d)
}
sine_label <- sine_plotly %>%
  #select(idx, frame) %>%
  #distinct() %>%
  mutate(labelX = -3.5, labelY = 2.4, label_idx = paste0("spl=", idx))

sine_anim <- ggplot(sine_label) +
                geom_point(aes(x=X1, y=X2, 
                               frame=frame)) +
                geom_text(aes(x=labelX, y=labelY, 
                              frame=frame, 
                              label=label_idx)) +
                xlab("") + ylab("") 
ggplotly(sine_anim) # text not showing

# Or with gganimate
library(gganimate)
library(gapminder)

sine_anim <- ggplot(sine_label) +
  geom_point(aes(x=X1, y=X2)) +
  geom_text(aes(x=labelX, y=labelY, 
                label=label_idx), size=5) +
  xlab("") + ylab("") +
  transition_time(frame)
sine_anim
anim_save("sine_anim.gif")

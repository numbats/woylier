library(tidyverse)
library(tourr)
library(patchwork)
library(woylier)
data("sine_curve")
library(GGally)
ggscatmat(sine_curve)

# Test out calculations
mat <- sine_curve[,5:6]
colnames(mat) <- c("x", "y")
mat_idx <- round(tourr::splines2d()(mat), 2)
mat_rot <- data.frame(x = cos(pi/6) * mat$x +
                          sin(pi/6) * mat$y,
                      y = -sin(pi/6) * mat$x+
                           cos(pi/6) * mat$y)
mat_rot_idx <- round(tourr::splines2d()(mat_rot), 2)

p1 <- ggplot(mat, aes(x=x, y=y)) +
  geom_point() +
  ggtitle(paste("Splines index = ", mat_idx)) +
  theme(aspect.ratio=1)

p2 <- ggplot(mat_rot, aes(x=x, y=y)) +
  geom_point() +
  xlab("Rotated 1") + ylab("Rotated 2") +
  ggtitle(paste("Splines index = ", mat_rot_idx)) +
  theme(aspect.ratio=1)

p1+p2

# Try a different way to show this
mat <- mat %>%
  mutate(type = "sine")
mat_rot <- mat_rot %>%
  mutate(type = "rotated")
mat_all <- bind_rows(mat, mat_rot)
mat_all <- mat_all %>%
  mutate(type = factor(type, levels=c("sine", "rotated")))
mat_all_label <- data.frame(x=c(0, 0.5),
                            y=c(0.75, -0.25),
     type=factor(c("sine", "rotated"), levels=c("sine", "rotated")),
     label=c(paste0("spl=",mat_idx),
             paste0("spl=",mat_rot_idx)))

ggplot(mat_all, aes(x=x, y=y, colour=type)) +
  geom_point() +
  xlab("") + ylab("") +
  scale_colour_manual("", values=c("black", "orange")) +
  geom_text(data=mat_all_label, aes(x=x, y=y, label=label, colour=type)) +
  theme(aspect.ratio=1)

# Generate a sample interpolation
set.seed(5543)
base1 <- tourr::orthonormalise(tourr::basis_random(6, d=2))
base2 <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1), ncol=2, byrow=T)
sine_path <- givens_full_path(base1, base2, nsteps=100)
sine_all <- NULL
sine_proj <- NULL
for (i in 1:dim(sine_path)[3]) {
  d <- as.matrix(sine_curve) %*% as.matrix(sine_path[,,i])
  d <- data.frame(d)
  d$idx <- round(tourr::splines2d()(d), 2)
  d$frame <- i
  sine_all <- bind_rows(sine_all, d)
  prj <- as.data.frame(sine_path[,,i])
  prj$frame <- i
  prj$names <- colnames(sine_curve)
  sine_proj <- bind_rows(sine_proj, prj)
}
sine_label <- sine_all %>%
  mutate(labelX = -1, labelY = 1.3, label_idx = paste0("spl=", format(idx, digits=2)))
sine_proj <- sine_proj %>%
  mutate(cx = 0, cy = 0)

# With gganimate
library(gganimate)

sine_anim <- ggplot() +
  geom_segment(data=sine_proj, aes(x=V1, y=V2,
                                   xend=cx, yend=cy,
                                   group=frame),
               colour="grey60") +
  geom_text(data=sine_proj, aes(x=V1, y=V2,
                                label=names,
                                group=frame),
               colour="grey60") +
  geom_point(data=sine_label, aes(x=X1, y=X2)) +
  geom_text(data=sine_label, aes(x=labelX, y=labelY,
                label=label_idx), size=10) +
  xlab("") + ylab("") +
  transition_time(frame) +
  theme_void() +
  theme(aspect.ratio=1,
        plot.background = element_rect(fill=NULL, colour = "black"))

animate(sine_anim, fps=8, renderer = gifski_renderer(loop = FALSE), width=300, height=300)
anim_save("sine_anim.gif")

# With planned tour - Doesn't respect the specific frame
#class(sine_path) <- "history_array"

#animate_xy(sine_curve, planned_tour(sine_path))

# Try with plotly
#library(plotly)
#library(tidyverse)

#sine_anim <- ggplot(sine_label) +
#                geom_point(aes(x=X1, y=X2,
#                               frame=frame)) +
#                geom_text(aes(x=labelX, y=labelY,
#                              frame=frame,
#                              label=label_idx)) +
#                xlab("") + ylab("")
#ggplotly(sine_anim) # text not showing



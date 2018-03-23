library(ggplot2)
f <- function(a, b) {
  function(y) {
    a * y * log(y, base = 10) - 1 / b * exp(-(b * y - b / exp(1))^4)
  }
}

y <- seq(0.01, 1, length.out = 100)
cols <- colorspace::rainbow_hcl(5)
d5 <- data.frame(x = f(3, 30)(y), y = y, color = cols[1])
d4 <- data.frame(x = f(2.8, 33)(y), y = y, color = cols[2])
d3 <- data.frame(x = f(2.5, 36)(y), y = y, color = cols[3])
d2 <- data.frame(x = f(2.2, 40)(y), y = y, color = cols[4])
d1 <- data.frame(x = f(2, 50)(y), y = y, color = cols[5])

df <- list(d5, d4, d3, d2, d1)

ggplot(rbind(d1, d2, d3, d4, d5)) +
  geom_path(aes(x, y, color = I(color))) +
  labs(x = "", y = "")

library(tweenr)
df2 <- tween_states(df,
  tweenlength = 2, statelength = 1,
  ease = rep("cubic-in-out", 4), nframes = 100
)
# df3 <- as.data.frame(apply(df2,2,rev))


animation::ani.options(interval = 1 / 24, ani.height = 1000, ani.width = 600)
library(gganimate)

p2 <- ggplot(data = df2, aes(x, y, color = I(color), frame = .frame)) +
  geom_path() +
  xlab(NULL) + ylab(NULL)

gganimate(p2, filename = "breast.gif", title_frame = F)


# animation 这个是基础
# tweenr  使得动画平滑过渡
# gganimate 使得图形有 ggplot2 风格


library(plotrix)
library(plot3D)
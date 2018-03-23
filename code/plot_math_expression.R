pdf(file = "figures/plotmath.pdf", pointsize = 10, width = 8, height = 6)

# 代码来自  http://www.ejwagenmakers.com/misc/Plotting_3d_in_R.pdf
# 3-D plots
mu1 <- 0 # setting the expected value of x1
mu2 <- 0 # setting the expected value of x2
s11 <- 10 # setting the variance of x1
s12 <- 15 # setting the covariance between x1 and x2
s22 <- 10 # setting the variance of x2
rho <- 0.5 # setting the correlation coefficient between x1 and x2
x1 <- seq(-10, 10, length = 41) # generating the vector series x1
x2 <- x1 # copying x1 to x2
f <- function(x1, x2) {
  term1 <- 1 / (2 * pi * sqrt(s11 * s22 * (1 - rho^2)))
  term2 <- -1 / (2 * (1 - rho^2))
  term3 <- (x1 - mu1)^2 / s11
  term4 <- (x2 - mu2)^2 / s22
  term5 <- -2 * rho * ((x1 - mu1) * (x2 - mu2)) / (sqrt(s11) * sqrt(s22))
  term1 * exp(term2 * (term3 + term4 - term5))
} # setting up the function of the multivariate normal density
z <- outer(x1, x2, f) # calculating the density values
nrz <- nrow(z)
ncz <- ncol(z)
# Create a function interpolating colors in the range of specified colors
# jet.colors <- colorRampPalette( c("blue", "green") )
# Generate the desired number of colors from this palette
nbcol <- 100
# color <- jet.colors(nbcol)
color <- viridisLite::viridis(100)
# Compute the z-value at the facet centres
zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)
# persp 坐标轴标签不支持表达式 ?persp
persp(x1, x2, z,
  xlab = expression(x[1]),
  ylab = expression(x[2]),
  zlab = expression(italic(f) ~ (bold(x))),
  main = "Two dimensional Normal Distribution",
  sub = expression(italic(f) ~ (bold(x)) == frac(1, 2 ~ pi ~ sqrt(sigma[11] ~ sigma[22] ~ (1 - rho^2))) ~ phantom(0)^bold(.) ~ exp ~
  bgroup(
    "{", list(
      -frac(1, 2(1 - rho^2)),
      bgroup(
        "[", frac((x[1] ~ -~ mu[1])^2, sigma[11]) ~ -~ 2 ~ rho ~ frac(x[1] ~ -~ mu[1], sqrt(sigma[11])) ~ frac(x[2] ~ -~ mu[2], sqrt(sigma[22])) ~ +~ frac((x[2] ~ -~ mu[2])^2, sigma[22]),
        "]"
      )
    ),
    "}"
  )),
  col = color[facetcol], border = NA, theta = 30, phi = 20,
  r = 50, d = 0.1, expand = 0.5, ltheta = 90, lphi = 180,
  shade = 0.1, ticktype = "detailed", nticks = 5, box = TRUE
) # produces the 3-D plot
# adding a text line to the graph
mtext(expression(list(mu[1] == 0, mu[2] == 0, sigma[11] == 10, sigma[22] == 10, sigma[12] == 15, rho == 0.5)), side = 3)

dev.off()
#-----------------------------
# 注意：persp 函数的坐标轴标签不支持表达式
#----------------------------
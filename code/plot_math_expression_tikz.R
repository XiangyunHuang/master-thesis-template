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

## 使用 tikzDevice 打造出版级的效果图

library(tikzDevice)
tf <- file.path(getwd(), "binormal.tex")

tikz(tf, width = 6, height = 5.5, pointsize = 30, standAlone = TRUE)
op <- par(mar = c(2, 3, 3, 0.5))
persp(x1, x2, z,
  xlab = "$x_{1}$",
  ylab = "$x_{2}$",
  zlab = "$f(\\mathsf{x})$",
  main = "Two dimensional Normal Distribution",
  col = color[facetcol], border = NA, theta = 30, phi = 20,
  r = 50, d = 0.1, expand = 0.5, ltheta = 90, lphi = 180,
  shade = 0.1, ticktype = "detailed", nticks = 5, box = TRUE
) # produces the 3-D plot
# adding a text line to the graph
mtext("$\\mu_1 = 0,\\mu_2 = 0,\\sigma_{11} = 10,\\sigma_{22} = 10,\\sigma_{12} = 15, \\rho = 0.5$", side = 3)
mtext("$f(\\mathsf{x}) = \\frac{1}{2\\pi\\sqrt{\\sigma_{11}\\sigma_{22}(1-\\rho^2)}}\\exp\\big\\{-\\frac{1}{2(1-\\rho^2)}[\\frac{(x_1 - \\mu_1)^2}{\\sigma_{11}} - 2\\rho\\frac{(x_1 - \\mu_1)(x_2 - \\mu_2)}{\\sqrt{\\sigma_{11}}\\sqrt{\\sigma_{22}}} + \\frac{(x_2 - \\mu_2)^2}{\\sigma_{22}}]\\big\\}$", side = 1)
par(op)
dev.off()

tinytex::latexmk(file = "binormal.tex")
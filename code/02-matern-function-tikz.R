
matern <-function (u, phi, kappa) 
{
	uphi <- u/phi
	uphi <- ifelse(u > 0, (((2^(-(kappa - 1))) / ifelse(0, Inf, gamma(kappa))) * (uphi^kappa) * besselK(x = uphi, nu = kappa)), 1)
	# uphi[u > 600 * phi] <- 0
	return(uphi)
}
  
# Matern族
library(tikzDevice)
tf <- file.path(getwd(),'matern.tex')

tikz(tf,width = 6,height = 4,pointsize = 30,standAlone=TRUE)
op <- par(mfrow = c(1,2),mar = c(4, 4, 3, .5))
curve(matern(x, phi= 0.25, kappa = 0.5),from = 0, to = 2,
   xlab = '$u$', ylab = '$\\rho(u)$',lwd = 2, lty = 1,
   main='varying  $\\kappa$ and fixed  $\\phi=0.25$')
curve(matern(x, phi= 0.25, kappa = 1.0),lwd = 2,lty = 2,from = 0, to = 2, add = TRUE)
curve(matern(x, phi= 0.25, kappa = 2.0),from = 0, to = 2, add = TRUE,
   lwd = 2, lty = 3)
curve(matern(x, phi= 0.25, kappa = 3.0),from = 0, to = 2, add = TRUE,
   lwd = 2, lty = 4)
legend("topright", c('$\\kappa=0.5$ ',' $\\kappa=1$' , '$\\kappa=2$', '$\\kappa=3$'),
 lty=c(1,2,3,4), lwd=c(2,2,2,2)) 
 
 
curve(matern(x, phi= 0.2, kappa = 1.5),from = 0, to = 4,
   xlab = '$u$', ylab = '$\\rho(u)$', lty = 1, lwd = 2,
   main='fixed  $\\kappa=1.5$ and varying  $\\phi$')
curve(matern(x, phi= 0.4, kappa = 1.5),lty = 2, lwd = 2,from = 0, to = 4, add = TRUE)
curve(matern(x, phi= 0.6, kappa = 1.5),from = 0, to = 4, add = TRUE,
   lwd = 2, lty = 3)
curve(matern(x, phi= 0.8, kappa = 1.5),from = 0, to = 4, add = TRUE,
   lwd = 2, lty = 4)
legend("topright", c('$\\phi=0.2$ ',' $\\phi=0.4$' , '$\\phi=0.6$', '$\\phi=0.8$'),
 lty=c(1,2,3,4), lwd=c(2,2,2,2))
par(op) 
dev.off()

# View the output
# 在生成的 .tex 文件中加入 \usepackage{times}
tools::texi2dvi(tf,pdf=T)
system(paste(getOption('pdfviewer'),file.path(getwd(),'matern.pdf')))

# 从图中可以看出，kappa 和 phi 对 rho 的影响趋势是一致的，都是随着它们增大而减小，但是 phi 的影响更大些，固定 u = 2，比较两图，可以明显地看出不同 phi 下 rho 之间的差别远大于不同 kappa 下 rho 之间的差别





matern <- function(u, phi, kappa){
	uphi <- u/phi
	uphi <- ifelse(u > 0, (((2^(-(kappa - 1))) / ifelse(0, Inf, gamma(kappa))) * (uphi^kappa) * besselK(x = uphi, nu = kappa)), 1)
	# uphi[u > 600 * phi] <- 0
	return(uphi)
}

n = 100

phi_vec <- seq(from = 0.01,to = .8,length.out = n)
kappa_vec <- seq(from = 0.1, to = 3.0,length.out = n)

# 给定 u 即相当于给定位置，看相关性 matern 值随 phi 和 kappa 的关系
# u <- seq(from = 0.01,to = 5, length.out = 1000)

dat <- data.frame(phi = rep(phi_vec, each = n), kappa = rep(kappa_vec, n) )
		
for(i in seq(n^2)){ 
  dat$value1[i] <- matern(u = 0.1, phi = dat$phi[i], kappa = dat$kappa[i] )
  dat$value2[i] <- matern(u = 0.5, phi = dat$phi[i], kappa = dat$kappa[i] )
  dat$value3[i] <- matern(u = 1.0, phi = dat$phi[i], kappa = dat$kappa[i] )  
  dat$value4[i] <- matern(u = 1.5, phi = dat$phi[i], kappa = dat$kappa[i] )
  dat$value5[i] <- matern(u = 2,   phi = dat$phi[i], kappa = dat$kappa[i] )
  dat$value6[i] <- matern(u = 2.5, phi = dat$phi[i], kappa = dat$kappa[i] )
  dat$value7[i] <- matern(u = 3.0, phi = dat$phi[i], kappa = dat$kappa[i] )
  dat$value8[i] <- matern(u = 3.5, phi = dat$phi[i], kappa = dat$kappa[i] )
  dat$value9[i] <- matern(u = 4,   phi = dat$phi[i], kappa = dat$kappa[i] )  
}

# 三维数组

library(ggplot2)
library(gridExtra)

p1 <- ggplot(dat, aes(phi, kappa)) +
  geom_raster(aes(fill = value1)) +
  scale_fill_distiller(palette = "Spectral", guide = FALSE) +
  labs(x = quote(bold(u) == 0.1),y = '')

p2 <- ggplot(dat, aes(phi, kappa)) +
 geom_raster(aes(fill = value2)) +
  scale_fill_distiller(palette = "Spectral", guide = FALSE) +
  labs(x = quote(bold(u) == 0.5),y = '')
 
p3 <- ggplot(dat, aes(phi, kappa)) +
 geom_raster(aes(fill = value3)) +
  scale_fill_distiller(palette = "Spectral", guide = FALSE) +
  labs(x = quote(bold(u) == 1.0), y = '') 
 
p4 <- ggplot(dat, aes(phi, kappa)) +
 geom_raster(aes(fill = value4)) +
  scale_fill_distiller(palette = "Spectral", guide = FALSE) +
  labs(x = quote(bold(u) == 1.5), y = '')

p5 <- ggplot(dat, aes(phi, kappa)) +
 geom_raster(aes(fill = value5)) +
  scale_fill_distiller(palette = "Spectral", guide = FALSE) +
  labs(x = quote(bold(u) == 2.0), y = '') 
 
p6 <- ggplot(dat, aes(phi, kappa)) +
 geom_raster(aes(fill = value6)) +
  scale_fill_distiller(palette = "Spectral", guide = FALSE) +
  labs(x = quote(bold(u) == 2.5), y = '') 

p7 <- ggplot(dat, aes(phi, kappa)) +
 geom_raster(aes(fill = value7)) +
  scale_fill_distiller(palette = "Spectral", guide = FALSE) +
  labs(x = quote(bold(u) == 3.0), y = '')  
 
p8 <- ggplot(dat, aes(phi, kappa)) +
 geom_raster(aes(fill = value8)) +
  scale_fill_distiller(palette = "Spectral", guide = FALSE) +
  labs(x = quote(bold(u) == 3.5), y = '')

p9 <- ggplot(dat, aes(phi, kappa)) +
 geom_raster(aes(fill = value9)) +
  scale_fill_distiller(palette = "Spectral", guide = FALSE) +
  labs(x = quote(bold(u) == 4.0), y = '') 
# 默认按行排列
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow = 3, ncol = 3)
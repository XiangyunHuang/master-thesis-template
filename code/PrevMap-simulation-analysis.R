set.seed(2018)
library("geoR")
library("RandomFields")
DataNum = 1000;
gData = grf(DataNum, grid = "irreg", DataNum, DataNum,
			xlims = c(0,100), ylims = c(0,100), nsim=1, mean=0,
			cov.mode = "powered.exponential",
			cov.par = c(1,25), nugget = 1, kappa = 1)
# phi = 25 tau = 1 kappa = 1 sigma = 1
# Simulated date matrix with response variable distributed with 'powered exponential' covariance matrix
l = gData$coords
y = gData$data
x = rnorm(DataNum)
y2 = y+.5+x
gdata = cbind(l,y2,x)

## This is an illustration of the function. 
## In practice, Total_Iteration is recommended to be set to 2500 or higher.
## N_subset is recommemded to be set to 300 or higher.
RSA(gdata,N_subset = 400, Stepscale = 40, Total_Iteration = 4000, Warm = 20)

library(RSAgeo)
data(gdata)

@Misc{fields,
    title = {fields: Tools for spatial data},
    author = {{Douglas Nychka} and {Reinhard Furrer} and {John Paige} and {Stephan Sain}},
    note = {R package version 9.0},
    organization = {University Corporation for Atmospheric Research},
    address = {Boulder, CO, USA},
    year = {2015},
    url = {www.image.ucar.edu/fields},
    doi = {10.5065/D6W957CT},
  }

# Observed monthly precipitation, min and max temperatures for the coterminous US 1895-1997
# http://www.image.ucar.edu/GSP/Data/US.monthly.met/ 
# 数据集现已整合进 fields 包

# 广义线性模型在空间数据精算中的应用 书 [@CASR] 相应数据集 [@R-CASdatasets]

# http://www.kriging.com/datasets/
# Practical Geostatistics 2000 Data Sets
########################################################
# 生成 data_sim 数据集
set.seed(2018)
library(geoR)
library(RandomFields)
DataNum <- 10000;
Data_Sim <- grf(DataNum, grid = "irreg", DataNum, DataNum,
			xlims = c(0,100), ylims = c(0,100), nsim = 1, mean=0,
			cov.mode = "powered.exponential",
			cov.par = c(1,25), nugget = 1, kappa = 1)

rbinom(n, size, prob)
# phi = 25 tau = 1 kappa = 1 sigma = 1
# Simulated date matrix with response variable distributed with 'powered exponential' covariance matrix

#########################################################
# 模拟空间广义线性混合效应模型
library(mvtnorm)
library(geoR) 
library(RandomFields)
set.seed(2018)
# 协方差矩阵 模拟二元正态分布
sigma <- matrix(c(2, .2, .2, 3), byrow = TRUE, ncol = 2) 
N <- 400 # N <- 10000
data.x <- cbind(rep(1, N), rmvnorm(n = N, mean = c(1, 2), sigma = sigma))
beta <- c(1.2, 1, 0.2) # beta_0 = 1.2 是截距  
# 空间随机效应 sigma^2 = 1 phi = 25 tau^2 =1 kappa = 1

S <- grf(N, grid = "irreg", nx = N, ny = N,
		xlims = c(0,100), ylims = c(0,100), nsim = 1, mean=0,
		cov.mode = "powered.exponential",
		cov.par = c(1,25), nugget = 1, kappa = 1)
mu <- exp(S$data + data.x %*% beta)/(1 + exp(S$data + data.x %*% beta))
binom.data.y <- rbinom(N, size = 10, prob = mu) / 10

 
library(ggplot2)
ggplot(data = as.data.frame(S$coords), aes(x = x, y = y, colour = binom.data.y)) + 
  geom_point(pch = 16, size = 3, alpha = .8) +	
  scale_colour_distiller(palette = "Spectral") +
  labs(colour = "观察概率",x = "横坐标",y = "纵坐标")


# plot(S$coords, type = 'p', pch = 16, xlab = 'X Coord', ylab = 'Y Coord',
	# col = terrain.colors(11)[binom.data.y*10] )
# title(main = '响应变量的空间分布', family = 'SimSun')


# 泊松分布 空间随机效应和协变量同上
lambda <- exp(S$data + data.x %*% beta)  
pois.data.y <- rpois(length(S$data), lambda = lambda) 

ggplot(data = as.data.frame(S$coords), aes(x = x, y = y, colour = log(pois.data.y+1)) ) + 
  geom_point(pch = 16, size = 3, alpha = .8) +	
  scale_colour_distiller(palette = "Spectral") +
  labs(colour = "对数\n观察数目",x = "横坐标",y = "纵坐标")
  
  
  

#########################################################
library(PrevMap)
library(geoR)
data("data_sim") # 在 PrevMap 包中
knots1 <- expand.grid(seq(-0.2,1.2, length = 8), 
  seq(-0.2,1.2, length = 8))
knots2 <- expand.grid(seq(-0.2,1.2, length = 16), 
  seq(-0.2,1.2, length = 16))
knots3 <- expand.grid(seq(-0.2,1.2, length = 32), 
  seq(-0.2,1.2, length = 32))
# 精确计算  
par0.exact <- c(0,1,0.15)
exact.mcmc <- control.mcmc.MCML(n.sim = 15000, burnin = 5000, thin = 5, 
  h = 1.65/(nrow(data_sim)^(1/6)))                  
system.time(fit.MCML.exact <- binomial.logistic.MCML(y ~ 1, 
  units.m = ~ units.m, coords = ~ x1 + x2,
  data = data_sim, par0 = par0.exact, 
  start.cov.pars = 0.15,
  control.mcmc = exact.mcmc,
  kappa = 2, fixed.rel.nugget = 0, method = "nlminb",
  plot.correlogram = FALSE))  
  
# 近似计算 low-rank  参数估计 分3种粒度
par0.lr <- c(-0.219294,0.97945,0.21393)
lr.mcmc <- control.mcmc.MCML(n.sim = 15000, burnin = 5000, thin = 5, 
                             h = 1.65/(nrow(knots1)^(1/6)))

system.time(fit.MCML.lr1 <- binomial.logistic.MCML(y ~ 1,
  units.m = ~ units.m, coords = ~ x1 + x2,
  data = data_sim, par0 = par0.lr,
  start.cov.pars = par0.lr[3], control.mcmc = lr.mcmc,
  low.rank = TRUE, knots = knots1, kappa = 2,
  method = "nlminb", plot.correlogram  = FALSE))   
  
lr.mcmc$h <- 1.65/(nrow(knots2)^(1/6))
par0.lr <- c(-0.017333,0.16490,0.16971)
system.time(fit.MCML.lr2 <- binomial.logistic.MCML(y ~ 1,
  units.m = ~ units.m, coords = ~ x1 + x2,
  data = data_sim, par0 = par0.lr,
  start.cov.pars = par0.lr[3], control.mcmc = lr.mcmc,
  low.rank = TRUE, knots = knots2, kappa = 2, 
  method = "nlminb", plot.correlogram = FALSE))   
  
lr.mcmc$h <- 1.65/(nrow(knots3)^(1/6))
par0.lr <- c(-0.031759,0.30572, 0.18854)
system.time(fit.MCML.lr3 <- binomial.logistic.MCML(y ~ 1, 
  units.m = ~ units.m, coords = ~ x1 + x2,
  data = data_sim, par0 = par0.lr,
  start.cov.pars = par0.lr[3], control.mcmc = lr.mcmc,
  low.rank = TRUE, knots = knots3, kappa = 2, 
  method = "nlminb", plot.correlogram = FALSE))  
 
# 提取系数 
par.hat <- coef(fit.MCML.exact)
Sigma.hat <- varcov.spatial(coords = data_sim[c("x1","x2")],
  cov.pars = par.hat[2:3], kappa = 2)$varcov
# 重要性采样 近似高维积分  
mu.hat <- rep(par.hat[1], nrow(data_sim))                     
system.time(S.cond.sim <- Laplace.sampling(mu = mu.hat, Sigma = Sigma.hat, y = data_sim$y, 
                                           units.m = data_sim$units.m, control.mcmc = exact.mcmc, 
                                           plot.correlogram = FALSE) )

# 模拟近似的和精确的 prevalence
prevalence.sim <- exp(S.cond.sim$samples)/(1 + exp(S.cond.sim$samples))
prevalence.exact <- apply(prevalence.sim,2, mean)

## 预测 分3种粒度
lr.mcmc$h <- 1.65/(nrow(knots1)^(1/6))
system.time(pred.MCML.lr1 <- spatial.pred.binomial.MCML(fit.MCML.lr1,
  grid.pred = data_sim[c("x1","x2")], control.mcmc = lr.mcmc,
  type = "joint", scale.predictions = "prevalence",
  plot.correlogram = FALSE))

lr.mcmc$h <- 1.65/(nrow(knots2)^(1/6))
system.time(pred.MCML.lr2 <- spatial.pred.binomial.MCML(fit.MCML.lr2,
  grid.pred = data_sim[c("x1","x2")], control.mcmc = lr.mcmc,
  type = "joint", scale.predictions = "prevalence",
  plot.correlogram = FALSE))  

lr.mcmc$h <- 1.65/(nrow(knots3)^(1/6))
system.time(pred.MCML.lr3 <- spatial.pred.binomial.MCML(fit.MCML.lr3,
  grid.pred = data_sim[c("x1","x2")], control.mcmc = lr.mcmc,
  type = "joint", scale.predictions = "prevalence",
  plot.correlogram = FALSE))
 
# 画图展示  images of estimated surfaces of prevalence 
pdf(file = 'figure/simulation.pdf',width = 8,height = 7.2)
# mycols <- viridisLite::viridis(30)
mycols <- colorRampPalette( rev(RColorBrewer::brewer.pal(11, name = 'Spectral')) )(30)
op <- par(mfrow = c(2,2), mar = c(3,4,3,4))
r.exact <- rasterFromXYZ(cbind(data_sim[, c("x1","x2")],
  prevalence.exact))
plot(r.exact, zlim = c(0,1), main = "Exact method", col = mycols )  
contour(r.exact, levels = seq(0.1,0.9,0.1), add = TRUE)
 
plot(pred.MCML.lr1,"prevalence","predictions", zlim = c(0,1),
  main = "Low-rank: 64 knots", col = mycols)
contour(pred.MCML.lr1,"prevalence","predictions", zlim = c(0,1),
  levels = seq(0.1,0.9,0.1), add = TRUE)
             
plot(pred.MCML.lr2,"prevalence","predictions", zlim = c(0,1),
  main = "Low-rank: 256 knots", col = mycols)
contour(pred.MCML.lr2,"prevalence","predictions", zlim = c(0,1),
  levels = seq(0.1,0.9,0.1), add = TRUE)

plot(pred.MCML.lr3,"prevalence","predictions", zlim = c(0,1),
  main = "Low-rank: 1024 knots", col = mycols)
contour(pred.MCML.lr3,"prevalence","predictions", zlim = c(0,1),
  levels = seq(0.1,0.9,0.1), add = TRUE)  
par(op)  
  
dev.off()  
  
  
  
##############################################################################################

# 模拟结果

# 精确计算  
> par0.exact <- c(0,1,0.15)
> exact.mcmc <- control.mcmc.MCML(n.sim = 15000, burnin = 5000, thin = 5, 
+   h = 1.65/(nrow(data_sim)^(1/6)))
> system.time(fit.MCML.exact <- binomial.logistic.MCML(y ~ 1, 
+   units.m = ~ units.m, coords = ~ x1 + x2,
+   data = data_sim, par0 = par0.exact, 
+   start.cov.pars = 0.15,
+   control.mcmc = exact.mcmc,
+   kappa = 2, fixed.rel.nugget = 0, method = "nlminb",
+   plot.correlogram = FALSE))
Fixed relative variance of the nugget effect: 0 
Conditional simulation (burnin=5000, thin=5): 
Iteration 15000 out of 15000 
Estimation: 
  0:    -0.0000000:  0.00000  0.00000 -1.89712
  1:  -0.096588322: 0.00221774 -0.00353693 -1.88858
  2:   -0.22173788: 0.00735497 -0.0104126 -1.87278
  3:   -0.97752221: 0.255598 -0.181303 -1.91676
  4:    -1.2385645: 0.613893 -0.535106 -2.00673
  5:    -1.3523607: 0.517397 -0.434678 -1.98096
  6:    -1.3544228: 0.503688 -0.417004 -1.97647
  7:    -1.3544238: 0.503373 -0.416617 -1.97637
  8:    -1.3544238: 0.503373 -0.416617 -1.97637
   user  system elapsed 
 839.57   18.94  858.19 
> ?control.mcmc.MCML
> 10000/5
[1] 2000
> coef(fit.MCML.exact)
(Intercept)     sigma^2         phi 
  0.5033730   0.6592735   0.1385712 
> # 近似计算 low-rank  参数估计 分3种粒度
> par0.lr <- c(-0.219294,0.97945,0.21393)
> lr.mcmc <- control.mcmc.MCML(n.sim = 15000, burnin = 5000, thin = 5, 
+   h = 1.65/(nrow(knots1)^(1/6)))
> system.time(fit.MCML.lr1 <- binomial.logistic.MCML(y ~ 1,
+   units.m = ~ units.m, coords = ~ x1 + x2,
+   data = data_sim, par0 = par0.lr,
+   start.cov.pars = par0.lr[3], control.mcmc = lr.mcmc,
+   low.rank = TRUE, knots = knots1, kappa = 2,
+   method = "nlminb", plot.correlogram  = FALSE))
Conditional simulation (burnin=5000, thin=5): 
Iteration 15000 out of 15000 
Estimation: 
  0:    -0.0000000: -0.219294 -1.61996 -0.502386
  1:    -1.9700794: -0.204222 -1.64731 -0.415457
  2:    -5.5861874: -0.249859 -2.04459 -0.432375
  3:    -7.0904812: -0.298889 -2.12859 -0.427948
  4:    -8.1851010: -0.316596 -2.13015 -0.436352
  5:    -9.5082331: -0.355437 -2.13856 -0.425871
  6:    -10.067565: -0.372897 -2.17102 -0.407318
  7:    -10.974438: -0.380679 -2.40809 -0.400481
  8:    -10.975223: -0.380693 -2.41513 -0.400440
  9:    -10.975223: -0.380693 -2.41510 -0.400440
 10:    -10.975223: -0.380693 -2.41510 -0.400440
   user  system elapsed 
  74.11    1.47   75.77 
> lr.mcmc$h <- 1.65/(nrow(knots2)^(1/6))
> par0.lr <- c(-0.017333,0.16490,0.16971)
> system.time(fit.MCML.lr2 <- binomial.logistic.MCML(y ~ 1,
+   units.m = ~ units.m, coords = ~ x1 + x2,
+   data = data_sim, par0 = par0.lr,
+   start.cov.pars = par0.lr[3], control.mcmc = lr.mcmc,
+   low.rank = TRUE, knots = knots2, kappa = 2, 
+   method = "nlminb", plot.correlogram = FALSE))
Conditional simulation (burnin=5000, thin=5): 
Iteration 15000 out of 15000 
Estimation: 
  0:    -0.0000000: -0.0173330 -4.16441 -0.733943
  1:    -1.4156772: -0.000845313 -4.19508 -0.645052
  2:    -1.6032590: -0.00360549 -4.27732 -0.647427
  3:    -1.6133414: 0.000503305 -4.29473 -0.654881
  4:    -1.6136570: 0.00136614 -4.29962 -0.656305
  5:    -1.6136571: 0.00137241 -4.29971 -0.656335
  6:    -1.6136571: 0.00137240 -4.29971 -0.656335
   user  system elapsed 
 144.46    9.01  153.56 
> lr.mcmc$h <- 1.65/(nrow(knots3)^(1/6))
> par0.lr <- c(-0.031759,0.30572, 0.18854)
> system.time(fit.MCML.lr3 <- binomial.logistic.MCML(y ~ 1, 
+   units.m = ~ units.m, coords = ~ x1 + x2,
+   data = data_sim, par0 = par0.lr,
+   start.cov.pars = par0.lr[3], control.mcmc = lr.mcmc,
+   low.rank = TRUE, knots = knots3, kappa = 2, 
+   method = "nlminb", plot.correlogram = FALSE))
Conditional simulation (burnin=5000, thin=5): 
Iteration 15000 out of 15000 
Estimation: 
  0:    -0.0000000: -0.0317590 -4.26470 -0.628724
  1:    -4.2142054: -0.0243050 -4.29660 -0.542862
  2:    -5.0398103: -0.00729183 -4.32851 -0.530939
  3:    -5.3459532: -0.0146437 -4.36908 -0.525692
  4:    -5.3511289: -0.0147919 -4.36446 -0.526091
  5:    -5.3511292: -0.0147946 -4.36443 -0.526097
  6:    -5.3511292: -0.0147946 -4.36443 -0.526097
   user  system elapsed 
 901.22  118.55 1019.87 
>    
  
  
  
 # 提取系数 
> par.hat <- coef(fit.MCML.exact)
> Sigma.hat <- varcov.spatial(coords = data_sim[c("x1","x2")],
+   cov.pars = par.hat[2:3], kappa = 2)$varcov
> mu.hat <- rep(par.hat[1], nrow(data_sim))
> system.time(S.cond.sim <- Laplace.sampling(mu = mu.hat, Sigma = Sigma.hat, y = data_sim$y, 
+                                            units.m = data_sim$units.m, control.mcmc = exact.mcmc, 
+                                            plot.correlogram = FALSE) )
Conditional simulation (burnin=5000, thin=5): 
Iteration 15000 out of 15000 
   user  system elapsed 
 278.84   47.99  327.77 
> # 模拟近似的和精确的 prevalence
> prevalence.sim <- exp(S.cond.sim$samples)/(1 + exp(S.cond.sim$samples))
> prevalence.exact <- apply(prevalence.sim,2, mean)
> ## 预测 分3种粒度
> lr.mcmc$h <- 1.65/(nrow(knots1)^(1/6))
> system.time(pred.MCML.lr1 <- spatial.pred.binomial.MCML(fit.MCML.lr1,
+   grid.pred = data_sim[c("x1","x2")], control.mcmc = lr.mcmc,
+   type = "joint", scale.predictions = "prevalence",
+   plot.correlogram = FALSE))
Conditional simulation (burnin=5000, thin=5): 
Iteration 15000 out of 15000 
Spatial predictions: prevalence 
   user  system elapsed 
  37.83    0.93   38.78 
> lr.mcmc$h <- 1.65/(nrow(knots2)^(1/6))
> system.time(pred.MCML.lr2 <- spatial.pred.binomial.MCML(fit.MCML.lr2,
+   grid.pred = data_sim[c("x1","x2")], control.mcmc = lr.mcmc,
+   type = "joint", scale.predictions = "prevalence",
+   plot.correlogram = FALSE))
Conditional simulation (burnin=5000, thin=5): 
Iteration 15000 out of 15000 
Spatial predictions: prevalence 
   user  system elapsed 
  83.76    6.95   90.78 
> lr.mcmc$h <- 1.65/(nrow(knots3)^(1/6))
> system.time(pred.MCML.lr3 <- spatial.pred.binomial.MCML(fit.MCML.lr3,
+   grid.pred = data_sim[c("x1","x2")], control.mcmc = lr.mcmc,
+   type = "joint", scale.predictions = "prevalence",
+   plot.correlogram = FALSE))
Conditional simulation (burnin=5000, thin=5): 
Iteration 15000 out of 15000 
Spatial predictions: prevalence 
   user  system elapsed 
 643.09  130.62  773.80 
 
# PrevMap JSS 文章的 loaloa数据分析

# 3.1 探索性分析

library("PrevMap")
data("loaloa")

loaloa$logit <- log((loaloa$NO_INF + 0.5)/(loaloa$NO_EXAM - loaloa$NO_INF + 0.5))
# 在对数据做经验 logit 变换后，计算 kappa 的 profile 似然  
profile.kappa <- shape.matern(formula = logit ~ 1,coords = ~ LONGITUDE + LATITUDE,
								data = loaloa, set.kappa = seq(0.2,2.2, length = 80),
								start.par = c(0.2,0.05), coverage = 0.95,
								messages=FALSE,plot.profile=FALSE)

c(profile.kappa$lower, profile.kappa$upper)
# 0.2136587	1.104513

profile.kappa$kappa.hat
  # 0.4988445

 # profile似然随kappa的变化图
library(tikzDevice)
tf <- file.path(getwd(),'kappa.tex')
tikz(tf,width = 4,height = 3,pointsize = 30,standAlone=TRUE)
par(mar = c(4, 4, 3, .5))
plot(profile.kappa$set.kappa,profile.kappa$val.kappa,pch=20,
	xlab='$\\kappa$',ylab="Log-likelihood",main='Profile likelihood for $\\kappa$')
dev.off()
# 在 kappa.tex 导言区添加一行 \usepackage{times}
tools::texi2dvi(tf,pdf=T)
system(paste(getOption('pdfviewer'),file.path(getwd(),'kappa.pdf')))


 ## 获取经验变差，再对经验变差做带权最小二乘估计获取理论变差
 # 目的是为了获取高斯空间过程的协方差参数的初始值
 library("geoR")
 coords <- as.matrix(loaloa[, c("LONGITUDE", "LATITUDE")])
 vari <- variog(coords = coords, data = loaloa$logit,
				uvec = seq(from = 0,to = 3,length.out = 25))

 vari.fit <- variofit(vari, ini.cov.pars = c(2, 0.2),
					  cov.model = "matern",
					  fix.nugget = FALSE, nugget = 0 , # 既然只是选初始值就没必要把块金效应加上 
					  fix.kappa = TRUE, kappa = 0.5)  # WLSE  带权最小二乘估计
					  
tf <- file.path(getwd(),'variogram.tex')
tikz(tf, width = 4, height = 4, pointsize = 30, standAlone=TRUE)
par(mar = c(4, 4, 3, .5))
plot(vari, pch = 20, xlab = '$u$', ylab = '$V(u)$', main = "Empirical and theoretical variogram ")
lines(vari.fit, lwd = 1.5, col = "black") # theoretical variogram
dev.off()
# View the output
tools::texi2dvi(tf, pdf = T)
system(paste(getOption('pdfviewer'),file.path(getwd(),'variogram.pdf')))

 vari.fit
 # tausq 代表块金效应，nugget 初始值是 0
 
# 3.2 线性模型 
 
## 对原始数据做 logit 变换后认为是线性模型
fit.MLE <- linear.model.MLE(formula = logit ~ 1,
  coords = ~ LONGITUDE + LATITUDE, data = loaloa,
  start.cov.pars = c(0.2, 0.01), kappa = 0.5) 
# 初值协方差参数 phi = 0.18 nu2 = tau^2/sigma^2   
 

##  用 logit 回归直接拟合原始数据
 
cp1 <- control.profile(rel.nugget = exp(seq(-5, 5, length = 20))) 
cp2 <- control.profile(rel.nugget = exp(seq(-5, 5, length = 20)), 
  phi = exp(seq(-8, 8, length = 40))) 
 
lp1 <- loglik.linear.model(fit.MLE, cp1, plot.profile = FALSE) 
lp2 <- loglik.linear.model(fit.MLE, cp2, plot.profile = FALSE) 
 

plot(lp1, type = "l", log.scale = TRUE, xlab = expression(log(nu^2)),
  ylab = "log-likelihood",
  main = expression("Profile likelihood for" ~ nu^2))
plot(lp2, log.scale = TRUE, xlab = expression(log(phi)),
  ylab = expression(log(nu^2)),
  main = expression("Profile likelihood for" ~ nu^2 ~ "and" ~ phi)) 
 
 
tf <- file.path(getwd(),'logist-linear-model.tex')
tikz(tf,width = 6,height = 4,pointsize = 30,standAlone=TRUE)
op <- par(mfrow = c(1, 2))
plot(lp1, type = "l", log.scale = TRUE, xlab = '$\\log(\\nu^2)$',
  ylab = "log-likelihood",
  main = 'Profile likelihood for~$\\nu^2$')
plot(lp2, log.scale = TRUE, xlab = '$\\log(\\phi)$',
  ylab = '$\\log(\\nu^2)$',
  main = 'Profile likelihood for~$\\nu^2$~and~$\\phi$') 
par(op)  
dev.off()

# View the output
# 在生成的 .tex 文件中加入 \usepackage{times}
tools::texi2dvi(tf,pdf=T)
 
## 3.3 Binomial logistic model

# 基于似然方法的分析

fit.glm <- glm(cbind(NO_INF, NO_EXAM) ~ 1, data = loaloa,
	family = binomial)
par0 <- c(coef(fit.glm), vari.fit$cov.pars, vari.fit$nugget)

c.mcmc <- control.mcmc.MCML(n.sim = 10000, burnin = 2000, 
	thin = 8, h = (1.65)/(nrow(loaloa) ^ (1/6)))
	
fit.MCML1 <- binomial.logistic.MCML(formula = NO_INF ~ 1, 
  units.m = ~ NO_EXAM, par0 = par0,
  coords = ~ LONGITUDE + LATITUDE, data = loaloa,
  control.mcmc = c.mcmc, kappa = 0.5, 
  start.cov.pars = c(par0[3], par0[4]/par0[2]) )
  
fit.MCML1$log.lik


par0 <- coef(fit.MCML1)      
start <- c(par0[3], par0[4]/par0[2])                                        
fit.MCML2 <- binomial.logistic.MCML(formula = NO_INF ~ 1, 
  units.m = ~ NO_EXAM, par0 = par0,
  coords = ~ LONGITUDE + LATITUDE, data = loaloa,
  control.mcmc = c.mcmc, kappa = 0.5,
  start.cov.pars = c(par0[3], par0[4]/par0[2]))
 
fit.MCML2$log.lik

c.mcmc <- control.mcmc.MCML(n.sim = 15000, burnin = 5000, 
  thin = 5, h = (1.65)/(nrow(loaloa)^(1/6)))
par0 <- coef(fit.MCML2)
fit.MCML3 <- binomial.logistic.MCML(formula = NO_INF ~ 1, 
  units.m = ~ NO_EXAM,par0=par0,
  coords = ~LONGITUDE+LATITUDE,data=loaloa,
  control.mcmc = c.mcmc,
  kappa = 0.5, start.cov.pars = c(par0[3],par0[4]/par0[2]))                            
summary(fit.MCML3)


library("splancs")
poly <- coords[chull(coords),]
grid.pred <- gridpts(poly, xs = 0.1, ys = 0.1)
pred.MCML <- spatial.pred.binomial.MCML(fit.MCML3, grid.pred,
  control.mcmc = c.mcmc, type = "marginal",
  scale.predictions = "prevalence",
  standard.errors = TRUE, thresholds = 0.2,
  scale.thresholds = "prevalence") 

par(mfrow = c(1,3))
plot(pred.MCML, type = "prevalence",
  summary = "predictions", zlim = c(0,0.45), 
  main = "Prevalence - predictions \n (classical analysis)")
contour(pred.MCML, type = "prevalence",
  summary = "predictions",
  levels = c(0.05,0.1,0.2,0.3), add = TRUE)
plot(pred.MCML, type = "prevalence",
  summary = "standard.errors", zlim = c(0,0.3), 
  main = "Prevalence - standard errors  \n (classical analysis)")
contour(pred.MCML, type = "prevalence",
  summary = "standard.errors",
  levels = c(0.05,0.1,0.15,0.2), add = TRUE)
plot(pred.MCML, summary = "exceedance.prob",
  zlim = c(0,1), 
  main = "Prevalence - exceedance probabilities \n (classical analysis)")
contour(pred.MCML, summary = "exceedance.prob",
  levels = c(0.1,0.4,0.5,0.7), add = TRUE) 


par(mfrow=c(3,3))
S.mean <- apply(pred.MCML$samples, 2, mean)
acf(S.mean,main = "")
plot(S.mean,type = "l")
plot(ecdf(S.mean[1:5000]), main = "")
lines(ecdf(S.mean[5001:10000]), col = 2, lty = "dashed")

ind.S <- sample(1:nrow(grid.pred), 2)
acf(pred.MCML$samples[ind.S[1],], main = "")
plot(pred.MCML$samples[ind.S[1], ], 
  ylab = paste("Component n.", ind.S[1]), type = "l")
plot(ecdf(pred.MCML$samples[ind.S[1], 1:5000]), main = "")
lines(ecdf(pred.MCML$samples[ind.S[1], 5001:10000]), 
  col = 2, lty = "dashed")

acf(pred.MCML$samples[ind.S[2],], main = "")
plot(pred.MCML$samples[ind.S[2], ], 
  ylab = paste("Component n.", ind.S[2]), type = "l")
plot(ecdf(pred.MCML$samples[ind.S[2], 1:5000]), main = "")
lines(ecdf(pred.MCML$samples[ind.S[2], 5001:10000]), 
  col = 2, lty = "dashed")


cp <-control.prior(beta.mean = 0, beta.covar = 100^2,
  log.normal.sigma2 = c(1,5),
  uniform.phi  = c(0,8),
  log.normal.nugget = c(-3,1))
 
mcmc.Bayes <- control.mcmc.Bayes(n.sim = 6000, burnin = 1000, thin = 1,
  h.theta1 = 1, h.theta2 = 0.7, h.theta3 = 0.05,
  L.S.lim = c(5,50), epsilon.S.lim = c(0.03,0.06),
  start.beta  = -2.3, start.sigma2 = 2.6,
  start.phi = 0.8, start.nugget = 0.05,
  start.S = predict(fit.glm)) 

fit.Bayes <- binomial.logistic.Bayes(formula = NO_INF ~ 1, 
  units.m = ~ NO_EXAM,
  coords = ~ LONGITUDE + LATITUDE, 
  data = loaloa, control.prior = cp,
  control.mcmc = mcmc.Bayes, kappa = 0.5) 

summary(fit.Bayes, hpd.coverage = 0.95)


par(mfrow = c(2,4))
autocor.plot(fit.Bayes, param = "beta", component.beta = 1)
autocor.plot(fit.Bayes, param = "sigma2")
autocor.plot(fit.Bayes, param = "phi")
autocor.plot(fit.Bayes, param = "tau2")
i <- sample(1:nrow(loaloa),4)
autocor.plot(fit.Bayes, param = "S", component.S = i[1])
autocor.plot(fit.Bayes, param = "S", component.S = i[2])
autocor.plot(fit.Bayes, param = "S", component.S = i[3])
autocor.plot(fit.Bayes, param = "S", component.S = i[4])


pred.Bayes <- spatial.pred.binomial.Bayes(fit.Bayes, grid.pred,
  type = "marginal",
  scale.predictions = "prevalence", quantiles = NULL,
  standard.errors = TRUE, thresholds = 0.2,
  scale.thresholds = "prevalence") 

par(mfrow = c(1,3))
 plot(pred.Bayes, type = "prevalence", summary = "predictions", 
  zlim = c(0,0.45), 
  main = "Prevalence - predictions \n (Bayesian analysis)")
 contour(pred.Bayes, type = "prevalence", summary = "predictions",
  levels = c(0.05,0.1,0.2,0.3), add = TRUE)
 
 plot(pred.Bayes, type = "prevalence", summary = "standard.errors", 
  zlim = c(0,0.3), 
  main = "Prevalence - standard errors \n (Bayesian analysis)")
 contour(pred.Bayes, type = "prevalence", 
  summary = "standard.errors",
  levels = c(0.05,0.1,0.15,0.2), add = TRUE)
 
 plot(pred.Bayes, type = "prevalence", zlim = c(0,1), 
  main = "Prevalence - exceedance probabilities \n 
  (Bayesian analysis)")
 contour(pred.Bayes, type = "prevalence",
  levels = c(0.1,0.4,0.5,0.7), add = TRUE)    



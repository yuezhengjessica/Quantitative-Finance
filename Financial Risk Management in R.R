## Risk Management Using R


###  ex01 =======================================

## a) VaR
alpha <- c(0.95, 0.99)
qnorm(alpha, mean=-0.03, sd=0.15) * 20000
## or equivalently
(-0.03 + 0.15 * qnorm(alpha)) * 20000

## b) ES
(-0.03  + 0.15 * dnorm(qnorm(alpha))/(1 - alpha)) * 20000

## c) plot both
x <- seq(0.9,0.999, length=100)
yVaR <- (-0.03 + 0.15 * qnorm(x)) * 20000
yES <- (-0.03 + 0.15 * dnorm(qnorm(x))/(1 - x)) * 20000
plot(x, yVaR, type="l", ylim=range(yVaR, yES),  
     xlab=expression(alpha), ylab="")
lines(x, yES, lty=2, col=2)
legend("topleft", legend=c("VaR","ES"),col=1:2, lty=1:2)



###  ex02 =======================================

library("sn")
## a) VaR 
alpha <- c(0.95,0.99)
mu <- 0.03
lambda <- 0.116
nu <- 5
q <- qst(alpha, location=-mu, scale=lambda, df=nu)
q * 20000

## b) ES
f <- dt(qt(alpha, df=nu), df=nu)
(-mu + lambda * f / (1-alpha) * (nu + qt(alpha, df=nu)^2) / (nu-1)  ) * 20000



###  ex03 =======================================

## a) 
## find out the starting date
sp.end <- as.Date("2013-04-30")
(sp.start <- sp.end - 1000)

## get SP500
library("tseries")
x <- get.hist.quote(start=sp.start, end = sp.end, instrument = "^gspc", 
                    provider = "yahoo", quote="Close")#, retclass="ts")
nrow(x)                                  # not 1000?
x <- get.hist.quote(start="1990-01-02", end = sp.end, instrument = "^gspc", 
                    provider = "yahoo", quote="Close")
x <- tail(x, 1000)
nrow(x)                                  # 1000
plot(x, xlab="S&P 500", ylab="level")
grid()




## b)
## returns
library("zoo")
sp500.ret.zoo <- diff(log(x))               # for this to work you need the library 'zoo'
sp500.ret <- as.numeric(sp500.ret.zoo)      # "zoo" objects can be dangerous
hist(sp500.ret, breaks=50, freq=FALSE)
alpha <- 0.95
q <- quantile(sp500.ret, probs=1-alpha) # (!) mirrored losses
names(q) <- NULL
abline(v=q, col=2)

## c)
## Empirical VaR (Historical Simulation)
q * (-20000)

## Empirical ES (Historical Simulation)
mean(sp500.ret[sp500.ret < q]) * (-20000)

## d) bootstrap CI
set.seed(1234)
B <- 10000
n <- length(sp500.ret)
res <- matrix(0, nrow=B, ncol=2, dimnames=list(NULL, c("VaR","ES")))
for (i in (1:B)){
  x <- sample(sp500.ret, replace=TRUE)
  q <- quantile(x, probs=1-alpha)
  res[i,"VaR"] <- -q * 20000
  res[i,"ES"] <- -mean(x[x < q]) * 20000
}
CI <- apply(res, 2, function(x) quantile(x, probs = c(0.025,0.975)))
t(CI)

## ii) or equivalently using "boot"
library(boot)
VaRES <- function(x,i){
  q <- quantile(x[i], probs=1-alpha)    # if stype="i"
  c(-q * 20000, -mean(x[x < q]) * 20000)
}
res <- boot(sp500.ret, statistic = VaRES, R = B, stype="i")
boot.ci(res, conf=0.95, type="perc", index=1) # index=2 for ES




## e)
plot(sp500.ret.zoo)
abline(h=q, col="red")
points(sp500.ret.zoo[sp500.ret < q], pch=20, col=2)



###  ex04 =======================================

## a)
library(MASS)
library(sn)
## Normal distribution
(fit.norm <- fitdistr(sp500.ret, "normal"))
mu.norm <- fit.norm$estimate[["mean"]]
sd.norm <- fit.norm$estimate[["sd"]]

## b)
## Student's t location-scale distribution
## Warning:
## Estimating the degrees-of-freedom can lead to an infinite likelihood
## see Fernandez & Steel (1999, Biometrika) for further details
(fit.t <- fitdistr(sp500.ret, "t"))
mu.t <- fit.t$estimate[["m"]]
lambda <- fit.t$estimate[["s"]]
nu <- fit.t$estimate[["df"]]

## c)
alpha <- 0.99
## VaR 
qnorm(alpha, mean=-mu.norm, sd=sd.norm) * 20000 # Normal distr.
qst(alpha, location=-mu.t, scale=lambda, df=nu) * 20000 # t-distr.

## ES
(-mu.norm + sd.norm * dnorm(qnorm(alpha))/(1 - alpha)) * 20000 # Normal distr.
f <- dt(qt(alpha, df=nu), df=nu)
(-mu.t + lambda * f / (1-alpha) * (nu + qt(alpha, df=nu)^2) / (nu-1)  ) * 20000 # t-distr.

## d) bootstrap
## i)
set.seed(1234)
B <- 50
res <- matrix(0, nrow=B, ncol=2, dimnames=list(NULL, c("VaR","ES")))
for (i in (1:B)){
  x <- sample(sp500.ret, replace=TRUE)
  fit.t <- fitdistr(x, "t")
  mu.t <- fit.t$estimate[["m"]]
  lambda <- fit.t$estimate[["s"]]
  nu <- fit.t$estimate[["df"]]
  
  ## VaR
  VaR <- qst(alpha, location=-mu.t, scale=lambda, df=nu) * 20000 # t-distr.  
  ## ES
  f <- dt(qt(alpha, df=nu), df=nu)
  ES <- (-mu.t + lambda * f / (1-alpha) * (nu + qt(alpha, df=nu)^2) / (nu-1)  ) * 20000 # t-distr.
  
  res[i,"VaR"] <- VaR
  res[i,"ES"] <- ES
}
CI <- apply(res, 2, function(x) quantile(x, probs = c(0.025,0.975)))
t(CI)

## ii) alternative solution with "boot"
myfun <- function(x, i){
  fit.t <- fitdistr(x[i], "t")
  mu.t <- fit.t$estimate[["m"]]
  lambda <- fit.t$estimate[["s"]]
  nu <- fit.t$estimate[["df"]] 
  ## VaR
  VaR <- qst(alpha, location=-mu.t, scale=lambda, df=nu) * 20000 # t-distr.  
  ## ES
  f <- dt(qt(alpha, df=nu), df=nu)
  ES <- (-mu.t + lambda * f / (1-alpha) * (nu + qt(alpha, df=nu)^2) / (nu-1)  ) * 20000 # t-distr.
  return(c(VaR, ES))  
}
res <- boot(sp500.ret, statistic = myfun, R = 50, stype="i")
boot.ci(res, conf=0.95, type="perc", index=1) # index=2 for ES


## e)
grid <- seq_along(sp500.ret) / (length(sp500.ret) + 1) # empirical cdf
q.n <- qnorm(grid, mean=mu.norm, sd=sd.norm)           # theoretical quantiles (N)
q.t <- qst(grid, location=mu.t, scale=lambda, df=nu)   # theoretical quantiles (t)

layout(t(1:2))
qqplot(q.n, sp500.ret, xlab="Normal distr.", ylab="Empirical quantiles")
abline(a=0, b=1, col=2)
## qqnorm(sp500.ret); qqline(sp500.ret, col=2)
qqplot(q.t, sp500.ret, xlab="t location-scale distr.", ylab="Empirical quantiles")
abline(a=0, b=1, col=2)




###  ex05 =======================================

## a) 
layout(t(1:2))
acf(sp500.ret)
acf(sp500.ret^2)



Box.test(sp500.ret, lag=1, type="Ljung-Box")
Box.test(sp500.ret^2, lag=1, type="Ljung-Box")



library(forecast)
fit.arma <- auto.arima(coredata(sp500.ret)) # ARMA(3,2)
sp500.ret.arma <- resid(fit.arma)
layout(t(1:2))
acf(sp500.ret.arma, na.action=na.pass)
acf(sp500.ret.arma^2, na.action=na.pass)
Box.test(sp500.ret.arma, lag=1, type="Ljung-Box")
Box.test(sp500.ret.arma^2, lag=1, type="Ljung-Box")




## b) GARCH(2,1)
library(fGarch)
fit.garch <- garchFit(~garch(2,1), sp500.ret.arma, cond.dist="std", trace=FALSE)
## fit.armagarch <- garchFit(~arma(3,2) + garch(2,1), sp500.ret, cond.dist="std", trace=FALSE) # ARMA(3,2)-GARCH(2,1)
csd <- fit.garch@sigma.t              # conditional standard deviation
sp500.ret.arma.garch <- sp500.ret.arma/csd # GARCH filter
par(mfrow=c(2,1))
plot(fit.garch, which=3)
plot(sp500.ret.arma.garch, type="l", col="steelblue", main="Standardized series")




par(mfrow=c(1,2))
acf(sp500.ret.arma.garch)
acf(sp500.ret.arma.garch^2)
Box.test(sp500.ret.arma.garch, lag=1, type="Ljung-Box")
Box.test(sp500.ret.arma.garch^2, lag=1, type="Ljung-Box")




## c)
alpha <- 0.95
mu <- forecast.Arima(fit.arma)$mean[1]
nu <- coef(fit.garch)[["shape"]]
sd <- predict(fit.garch, n.ahead=1)[["standardDeviation"]]




lambda <- sd * sqrt((nu-2)/nu)          # Equation (7)


## VaR
qst(alpha, location=-mu, scale=lambda, df=nu) # pkg: sn
(q <- qstd(alpha, mean=-mu, sd=sd, nu=nu))    # pkg: fGarch
(VaR <- 20000 * q)

## ES
f <- dt(qt(alpha, df=nu), df=nu)
(-mu + lambda * f / (1-alpha) * (nu + qt(alpha, df=nu)^2) / (nu-1)  ) * 20000




## d)
##  parametric estimation (marginal SD)
fit.marg <- fitdistr(sp500.ret, "t")
mu.marg <- fit.marg$estimate[["m"]]
lambda <- fit.marg$estimate[["s"]]
nu <- fit.marg$estimate[["df"]]
q.marg <- qst(alpha, location=-mu.marg, scale=lambda, df=nu)
VaR.marg <- q.marg * 20000

##  empirical SD (historical simulation)
q.his <- -quantile(sp500.ret, probs=1-alpha)
names(q.his) <- NULL
VaR.his <- q.his * 20000

## conditional ARMA/GARCH SD
mu.cond <- fitted.Arima(fit.arma)
sd.cond <- fit.garch@sigma.t
lambda.cond <- sd.cond * sqrt((nu-2)/nu) # Equation (5)
q.cond <- qst(alpha, location=-mu.cond, scale = lambda.cond, df=nu)
VaR.cond <- q.cond * 20000

plot(VaR.cond, type="l", col="steelblue", xlab="", ylab="VaR")
abline(h=c(VaR.marg, VaR.his), lty=2:3, col=2:3, lwd=3)
legend("topleft",c("conditional VaR_t","marginal VaR","empirical VaR"),
       lty=1:3, col=c("steelblue","red","green"), lwd=2)



###  ex06 =======================================

## a)
library("fGarch")
#rweek <- read.table("Rweek.csv", header=TRUE, sep=",", dec=".")
rweek <- read.table("http://robinzoni.userweb.mwn.de/riskman/data/Rweek.csv", 
                    header=TRUE, sep=",", dec=".")
rweek <- rweek[,-1]
rweek <- rweek/100          # net returns (not percentage anymore)
x <- rep(1/3, 3)                        # weights
mu_p <- colMeans(rweek) %*% x           # portfolio return
mu_p

## b)
## GARCH(1,1)
if("rweek" %in% search()) detach(rweek) 
attach(rweek)
sp500.garch <- garchFit(~garch(1,1), sp500, cond.dist="norm", trace=FALSE)
ftse.garch <- garchFit(~garch(1,1), ftse, cond.dist="norm", trace=FALSE)
dax.garch <- garchFit(~garch(1,1), dax, cond.dist="norm", trace=FALSE)

## Standardize 
sp500.stand <- sp500 / sp500.garch@sigma.t
ftse.stand <- ftse / ftse.garch@sigma.t
dax.stand <- dax / dax.garch@sigma.t

## c)
Cor <- cor(cbind(sp500.stand, ftse.stand, dax.stand))
(Cov <- cov(cbind(sp500.stand, ftse.stand, dax.stand)))
sigma_p <- x %*% Cov %*% x

sig1 <- (predict(sp500.garch, n.ahead = 1)[["standardDeviation"]])
sig2 <- (predict(ftse.garch, n.ahead = 1)[["standardDeviation"]])
sig3 <- (predict(dax.garch, n.ahead = 1)[["standardDeviation"]])




D <- diag(c(sig1, sig2, sig3))
(Cov_t <- D %*% Cor %*% D)

sd_p <- sqrt(x %*% Cov_t %*% x)         # portfolio sd

## VaR
alpha <- 0.95
qnorm(alpha, mean = -mu_p, sd = sd_p) * 20000



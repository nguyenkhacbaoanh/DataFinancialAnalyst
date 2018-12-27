#library(ggplot)
setwd("~/dev/DataFinancialAnalyst")
dat <- read.csv("datasets/Stock_bond.csv", header=TRUE);
n = dim(dat)[1]
attach(dat)
par(mfrow=c(1,2))
plot(GM_AC)
plot(F_AC)

GMReturn = GM_AC[-1]/GM_AC[-n] - 1
FReturn = F_AC[-1]/F_AC[-n] - 1
par(mfrow=c(1,1))
plot(GMReturn, FReturn)


set.seed(2012)
n = 253
par(mfrow=c(3,3))
for (i in (1:9))
{
  logr = rnorm(n,0.05/n, 0.23/sqrt(n))
  price = c(120,120*exp(cumsum(logr)))
  plot(price,type="b")
}

MCD_stock <- read.csv("datasets/MCD_PriceDaily.csv", header = TRUE)
p = MCD_stock[,7]
n = length(p)
returns = p[-1]/p[-n] - 1
logReturns = diff(log(p))
plot(returns, logReturns, ylab="log return")
abline(a=0,b=1,col='red', lwd=2)

random.seed(2015)
n_iters = 10000
values = matrix(n_iters)
for (i in 1:n_iters)
{
  logr = rnorm(n = 20, mean = mean(logReturns), sd = sd(logReturns))
  price = 93.07*exp(cumsum(logr))
  ind = (min(price) < 85)
  values[i] = 100*ind - (1 - ind)
}
mu = mean(values)
mu

random.seed(2015)
n_iters = 10000
values = matrix(n_iters)
for (i in 1:n_iters)
{
  logr = rnorm(n = 20, mean = mean(logReturns), sd = sd(logReturns))
  price = 93.07*exp(cumsum(logr))
  ind = (min(price) < 84.5)
  values[i] = 125*ind - (1 - ind)
}
mu = mean(values)
mu

logr = rnorm(mean = 0.001, sd = 0.015,n = 1)
pnorm(logr, mean = 0.001, sd = 0.015)


pnorm(log(1.2),mean=3*0.1,sd=sqrt(3)*0.2,lower.tail = FALSE)
help(pnorm)

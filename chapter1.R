#library(ggplot)
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
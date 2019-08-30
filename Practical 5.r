library(qqplotr)
library(car)

#Question 1

xbar = vector()
y1vec = vector()
tvec = vector()

nsim = 1000
n = 1000

for(i in 1:nsim)
{
  x = rexp(n, 1/5)
  xbar[i] = mean(x)
  y1vec[i] = min(x)
  tvec[i] =  n*min(x)
}

#bias 
print("Bias of Xbar:")
print(abs(mean(xbar)) - 5)

print("Bias of T:")
print(abs(mean(tvec)) - 5)

#verify dist of y1
print("Mean of y1")
print(mean(y1vec))

hist(y1vec, 50, probability = TRUE)
lines(seq(0, 0.04, by = 0.001), dexp(seq(0,0.04, by = 0.001), 1/5))

#1.3

#variances
print("variance of xbar")
print(var(xbar))
print("variances of t")
print(var(tvec))
print("THe variace of xba is much lower than the other therefoe")

#1.4
# do on paper = theta^2/n

#1.5
print("crlb is give by ---")
crlb = 25/1000

pint("Variance of xbar is")
print("this is equal to the crlb therefore xbar is the umvue")
#1.6
#Mle

x = rexp(10000, 1/5)
logli = function(x, thetainv)
{
  loglik = sum(log(dexp(x, thetainv)))
  return(-loglik)
  
}

optim(0.01, logli, x = x)

#1.7
#efficianey

rexbat = var(tvec)/var(xbar)
print(rexbat)
eff = 1/rexbat
print(eff)

#Question 2


vartn = vector()
plotvec = vector()

for(i in seq(50, 10000, by = 100))
{
  tnvec = vector()
  for(j in 1:1000)
  {
    x = rexp(i, 1/5)
    tnvec[j] = 1/mean(x)
  }
  vartn[i] = var(tnvec)
  plotvec[i] = i  #sample size
}

plot(plotvec, vartn)
# def 9.4.2

#crlb = 1/(n * theta^2)

crlb = vector()
theta = 5
for(i in seq(50, 10000, by = 100))
{
  crlb[i] = (1)/(i * theta^2)
}

ratiovec = crlb/vartn

plot(plotvec, ratiovec)

#2.6
rnvec = vector()
for(i in 1:50000)
{
  x = rexp(5000, 1/5)
  rnvec[i] =  exp(-1/mean(x))
}

theta = 5
t  = 0.1

empmean = mean(rnvec)
theoretical = exp(-t/theta)

empvar = var(rnvec)
theoreticalvar = (exp(-t/theta)*(t/theta^2)^2)*(theta^2/50000)

hist(rnvec, 100, probability = TRUE)
lines(seq(0.8, 0.83, 0.001), dnorm(seq(0.8, 0.83, 0.001), theoretical, sqrt(theoreticalvar)), lwd = 2)









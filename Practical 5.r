#Question 1

xbar = vector()
y1vec = vector()
tvec - vector()

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
print(abs(mean(xbar)))

print("Bias of T:")
print(abs(mean(tvec)))

#verify dist of y1
print("Mean of y1")
print(mean(y1vec))

hist(y1vec, probability = TRUE, 50)
lines(swq(0, 0.04, by = 0.001), dexp(seq(0,0.04, by = 0.001)))

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









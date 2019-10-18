#Question 1

n1 = 1000
x = rnorm(n1, 5, 1)
sigma = 1
mu0 = 10
alpha = 0.05


#1.1

hyp1sampleequal = function(x, sigma, mu0, alpha)
{
  n = length(x)
  
  ts = (mean(x) - mu0)/(sigma/sqrt(n)) #ts = test statistic
  
  criticalvalue = qnorm(1 - alpha/2)
  
  if(abs(ts) > criticalvalue)
  {
    print("Reject HO: my1 = mu0")
  }
  else
  {
    print("Do not reject Ho: my1 = mu0")
  }
}

hyp1sampleequal(x, sigma, mu0, alpha)

#1.2

hyp1sampleless = function(x, sigma, mu0, alpha)
{
  n = length(x)
  ts = (mean(x) - mu0)/(sigma / sqrt(n))
  criticalvalue = qnorm(1 - alpha)
  
  if(ts > criticalvalue)
  {
    print("Reject Ho: mu1 <= mu0")
  }
  else
  {
    print("Do not reject Ho: mu1 > mu0")
  }
}
hyp1sampleless(x, sigma, mu0, alpha)


#1.3


hyp1samplegreat = function(x, sigma, mu0, alpha)
{
  n = length(x)
  ts = (mean(x) - mu0)/(sigma / sqrt(n))
  criticalvalue = -qnorm(1 - alpha)
  
  if(ts < criticalvalue)
  {
    print("Reject Ho: mu1 >= mu0")
  }
  else
  {
    print("Do not reject Ho: mu1 < mu0")
  }
}
hyp1samplegreat(x, sigma, mu0, alpha)

d = -1
alpha = 0.05

mu1 = 1
sigma1 = 1
n1 = 1000

mu2 = 6
sigma2 = 1
n2 = 1200

x = rnorm(n1, mu1, sigma1)
y = rnorm(n2, mu2, sigma2)

hypfiff = function(x, y, d, alpha)
{
  n1 = length(x)
  n2 = length(y)
  
  xbar = mean(x)
  sx = var(x)
  
  ybar = mean(y)
  sy = var(y)
  
  ts = ((xbar - ybar) - d)/(sqrt(sx/n1 + sy/n2))
  critical = qnorm(1 - alpha/2)
  
  if(abs(ts) > critical)
  {
    print(paste("Reject HO: mu1 - mu2 = ", d, "."))
  }
  else
  {
    print(paste("Do not reject Ho"))
  }
}

hypfiff(x, y, d, alpha)

#Quesi\tion 2.1

x1 = rnorm(1000, 5, 4)
x2 = rnorm(1200, 6, 4)

r = 1
alpha = 0.05

hype2sample = function(x1, x2, r, alpha)
{
  n1 =  length(x1)
  n2 = length(x2)
  ts = (var(x1)/var(x2))
  criticallower = qf(alpha/2, n1 -1, n2 -1)
  criticalupper = qf(1 - alpha/2, n1 -1, n2 - 1)
  
  if(ts < criticallower || ts > criticalupper)
  {
    print(paste("Reject HoL var1/var2", r, "."))
  }
  else
  {
    print(paste("Do not reject HO"))
  }
}
hype2sample(x1, x2, r, alpha)
#Question 2

#1. y 6th order statistic

#2. 100 hours

#2.4


survivaltime = function(m)
{
  sum = 0
  for(i in 1:m)
  {
    sum = sum + 1/i
  }
  return(sum * 100)
}

mvec = 1:100
mvec = seq(1, 100, 1) # alternative

survivalvec = sapply(mvec, survivaltime)
plot(survivalvec ~ mvec, xlab = "Number of components", ylab = "Mean survival time", main = "Survival time influenced by components")

reliability = function(m)
{
  return(1 - (1 - exp(-200/100))^m)
}


mvec = 1:100
mvec = seq(1, 100, 1) # alternative

reliabilityvec = sapply(mvec, reliability)

plot(reliabilityvec ~ mvec, xlab = "Number of compenents", ylab = "relaibilty after 200 hours", main = "Reliabilty oafter 200 hours influenced by number of compenets
     ")

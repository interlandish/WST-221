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

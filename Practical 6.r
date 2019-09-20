#Question 1

n = 10000
nsim = 10000

theta = 5

chi25 = qchisq(0.025, 2*n)
chi975 = qchisq(0.975, 2*n)

contain = 0

for(i in 1:nsim)
{
  x = rexp(n, 1/theta)
  
  lowerbound = (2*n*mean(x))/chi975
  upperbound = (2*n*mean(x))/chi25
  
  if(theta <= upperbound & theta >= lowerbound)
  {
    contain = contain + 1
  }
}

print(paste("The true value of theta is contained in the interval", contain/nsim*100, " % of the time"))

print(contain/nsim)


#Question 2

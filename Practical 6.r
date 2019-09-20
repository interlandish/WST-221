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
n = 1000
nsim = 10000

contain = 0

mu = 3
sigma = 2

z = qnorm(0.975, 0, 1)

for(i in 1:nsim)
{
  x = rnorm(n, mu, sigma )
  
  lowerbound = mean(x) - z*(sigma/sqrt(n))
  upperbound = mean(x) + z*(sigma/sqrt(n))
  
  if(mu <= upperbound & mu >= lowerbound){contain = contain + 1}

}

print(paste("The true value of mu is contained in the interval ", contain/nsim, " of the time"))

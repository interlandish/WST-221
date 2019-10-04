# if pdf is a constant then it is considered as uninformative

# Beta is approptiate because the value of x is always between 0 and 1

# conjugate prior happens when the distibution of the posterior is in the same family as the distribution of the prior

#Question 1

n = 10000
alpha = 5
beta = 5
x = rbinom(n, 1, 0.7)
#uniform prior
parameter1 = sum(x) + 1
parameter2 = n - sum(x) + 1

uniprior = rbeta(n, parameter1, parameter2)

#Beta prior
parameter1 = sum(x) + 1
parameter2 = n - sum(x) + 1

betaprior1 = rbeta(n, parameter1, parameter2)

#Any values
parameter1 = sum(x) + alpha
parameter2 = n - sum(x) + beta

betaprior2 = rbeta(n, parameter1, parameter2)

#Plot histogram

#10. 

bayesuni = mean(uniprior)
bayesbeta1 = mean(betaprior1)
bayesbeta2 = mean(betaprior2)

bayesuni
bayesbeta1
bayesbeta2

#Question 2

n = 10000

beta = 0.500
kappa = 0.500

x = rpois(n, 0.7)

mle = mean(x)
mle

parameter1 = 1/(n + 1/beta)
parameter2 = sum(x) + kappa

Tee = parameter1 * parameter2
Tee

# works best for small kappa and large beta, small kappa and beta are also quite close
# work out and equation median for a gamma distribution

theta = rgamma(n, parameter2, 1/parameter1)
median(theta)


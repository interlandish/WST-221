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

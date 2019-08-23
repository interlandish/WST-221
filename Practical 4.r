#quesiton 1

n = 10000
x = runif(n, 2,5)
xbar = mean(x)


a = 1
b = 2*xbar - 4*xbar
c = 4* xbar*xbar - (3/n)*sum(x^2)

mme = quadratic(a, b, c)
mmetheta1 = min(mme)
mmetheta2 = max(mme)

mmetheta1
mmetheta2

#Question 2

n = 10000
x = rexp(n, 1)

liklihood = function(x, theta = 1)
{
  likli = sum(log(dexp(x, theta)))
  return(-likli)
}

idx = 1
for(i in seq(0.1,2, 0.01))
{
  thetaestimates[idx] = -liklihood(x, i)
  idx = idx + 1
}
thetas = seq(from = 0.1, to = 2, by = 0.01)

(mle_theta = thetas[which.max(thetaestimates)])

plot(x = thetas, y = thetaestimates)

optim(0.1, liklihood, x = x)

#Question 3

sample = rnorm(10000, 5, 5)

hood = function(e,parameters)
{
  mu = parameters[1]
  sigma = parameters[2]
  lik = sum(log(dnorm(e, mu, sigma)))
  return(-lik)
}

optim(c(2,2), hood,e = sample)

#Question 4

weisample = rweibull(10000, shape = 2, scale = 5)

hood2 = function(r, parameters)
{
  beta = parameters[1]
  theta = parameters[2]
  lik = sum(log(dweibull(r, shape = beta, scale = theta)))
  return(-lik)
}

optim(c(1, 1), hood2, r = weisample)

# Question 5
eta = 5
y = runif(10000, 0, 1)

x = eta - log(1 - y)

ddexp = function(c, eta)
{
  return(exp(-x + eta))
}

hood3 = function(x, eta){
  if(eta > min(x))
  {
    return(10 ^ 6)
  }
  lik = sum(log(ddexp(x, eta)))
  return(-lik)
}

results = vector()

idx = 1
for(i in seq(4.1, 6, 0.001))
{
  results[idx] = -hood3(x, i)
  idx = idx + 1
}
ivec = seq(4.1, 6, 0.001)
mlaeta = ivec[which.max(results)]
plot(ivec, results)
print(mlaeta)

mleeta = optim(3, hood3, x = x)
mleetamin = min(x)

mlaeta
mleeta
mleetamin









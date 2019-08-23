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

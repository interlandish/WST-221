library(ggplot2)

normalSample <- rnorm(10000, mean = 5, sd = 5)
binomialSample <- rbinom(10000, 5, 0.7)
betaSample <- rbeta(10000, 0.5, 0.5)

hist(normalSample, main = "Normal Sample", bins = 100)
hist(binomialSample, main = "Binomial Sample", bins = 5)
hist(betaSample, main = "Beta Sample", bins = 100)

normalSamples1 = matrix(ncol = 1000, nrow = 10)
for(i in 1:1000) { normalSamples1[, i] <-  rnorm(10, 5, 5)}
normalSamples2 = matrix(ncol = 1000, nrow = 1000)
for(i in 1:1000) { normalSamples2[, i] <-  rnorm(1000, 5, 5)}

binomialSamples1 = matrix(ncol = 1000, nrow = 10)
for(i in 1:1000) { binomialSamples1[, i] <-  rbinom(10, 5, 0.7)}
binomialSamples2 = matrix(ncol = 1000, nrow = 1000)
for(i in 1:1000) { binomialSamples2[, i] <-  rbinom(1000, 5, 0.7)}

betaSample1 = matrix(ncol = 1000, nrow = 10)
for(i in 1:1000) { betaSample1[, i] <-  rbeta(10, 0.5, 0.5)}
betaSample2 = matrix(ncol = 1000, nrow = 1000)
for(i in 1:1000) { betaSample2[, i] <-  rbeta(1000, 0.5, 0.5)}

generateZn <- function(matrix, size, mean, variance)
{
  znVector <- numeric(1000)
  for(i in 1:1000)
  {
    sumXi <- sum(matrix[, i])
    mu <- size * mean
    sd <- sqrt(size) * sqrt(variance)
    zn <- (sumXi - mu)/sd
    znVector <- append(znVector, zn, i)
  }
  print(sumXi)
  print(mean)
  print(size)
  print(mu)
  print(sd)
  print(zn)
  return(znVector)
  
}

normalZn1 <- generateZn(normalSamples1, 10, 5, 25)
normalZn2 <- generateZn(normalSamples2, 1000, 5, 25)

binomialZn1 <- generateZn(binomialSamples1, 10, 0.7 * 5, 0.7 * 5 * 0.3)
binomialZn2 <- generateZn(binomialSamples2, 1000, 0.7 * 5, 0.7 * 5 * 0.3)

betaZn1 <- generateZn(betaSample1, 10, 0.5, 0.25 / 2)
betaZn2 <- generateZn(betaSample2, 1000, 0.5, 0.25 / 2)

hist(normalZn2)
hist(binomialZn2)
hist(betaZn2)

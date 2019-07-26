library(ggplot2)
library(qqplotr)
library(car)

normalSample <- rnorm(10000, mean = 5, sd = 5)
binomialSample <- rbinom(10000, 5, 0.7)
betaSample <- rbeta(10000, 0.5, 0.5)

hist(normalSample, main = "Normal Sample")
hist(binomialSample, main = "Binomial Sample")
hist(betaSample, main = "Beta Sample")

# Generating the random samples
normalSamples1 = matrix(rnorm(10*1000, 5, 5), ncol = 1000, nrow = 10)
normalSamples2 = matrix(rnorm(1000*1000, 5, 5),ncol = 1000, nrow = 1000)

binomialSamples1 = matrix(rbinom(10*1000, 5, 0.7), ncol = 1000, nrow = 10)
binomialSamples2 = matrix(rbinom(1000*1000, 5, 0.7), ncol = 1000, nrow = 1000)

betaSample1 = matrix(rbeta(10*1000, 0.5, 0.5), ncol = 1000, nrow = 10)
betaSample2 = matrix(rbeta(1000*1000, 0.5, 0.5), ncol = 1000, nrow = 1000)

generateZn <- function(matrix, size, mean, variance)
{
  znVector <- numeric()
  for(i in 1:1000)
  {
    sumXi <- sum(matrix[, i])
    mu <- size * mean
    sd <- sqrt(size) * sqrt(variance)
    zn <- (sumXi - mu)/sd
    znVector <- append(znVector, zn, i)
  }
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

qqPlot(normalZn1, main = "Normal Zn n = 10")
qqPlot(normalZn2, main = "Normal Zn n = 1000")

qqPlot(binomialZn1, main = "Binomial Zn with n = 10")
qqPlot(binomialZn2, main = "Binomial Zn with n = 1000")

qqPlot(betaZn1, main = "Beta Zn with n = 10")
qqPlot(betaZn2, main = "Beta Zn with n = 1000")


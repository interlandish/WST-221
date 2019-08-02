library(qqplotr)
library(car)

#Question 1

verifyBeta <- function()
{
  sampleMeans = numeric(0)
  sampleVariances <- numeric(0)
  theoreticalMean <- 0.8 / (0.8 + 0.3)
  theoreticalSampleVariance <- ((0.8 * 0.3) / ((0.8 + 0.3 + 1) * (0.8 + 0.3)^2)) / 1000
  theoreticalVariance <- ((0.8 * 0.3) / ((0.8 + 0.3 + 1) * (0.8 + 0.3)^2))
  for(i in 1:100)
  {
    betaSample = rbeta(1000, 0.8, 0.3)
    sampleMeans <- append(sampleMeans, mean(betaSample), i)
    sampleVariances <- append(sampleVariances, var(betaSample), i)
  }
  print(paste("Theoretical Mean: ", theoreticalMean , " vs emperical mean: ", mean(sampleMeans)))
  print(paste("Theoretical Variance: ", theoreticalSampleVariance , " vs emperical var: ", var(sampleMeans)))
  print(paste("Theoretical Sample Variance: ", theoreticalVariance, " vs emperical sample variance", mean(sampleVariances)))
}
verifyBeta()

#Question 2

verifyNormal <- function()
{
  XMeans <- numeric()
  YMeans <- numeric()
  XVariances <- numeric()
  YVariances <- numeric()
  
  XTheoreticalMean = 0
  YTheoreticalMean = 5
  XTheoreticalVariance = 1/1040
  YTheoreticalVariance = 16/1050
  
  for(i in 1:1000)
  {
    standardNormal <- rnorm(1040, mean = 0, sd = 1)
    normal <- rnorm(1050, mean = 5, sd = 4)
    XMeans <- append(XMeans, mean(standardNormal), i)
    YMeans <- append(YMeans, mean(normal), i)
    XVariances <- append(XVariances, var(standardNormal), i)
    YVariances <- append(YVariances, var(normal), i)
  }
  
  X_YMeans <- XMeans - YMeans
  TheoreticalX_YMeans <- XTheoreticalMean - YTheoreticalMean
  THeoreticalX_YVariance  <- XTheoreticalVariance + YTheoreticalVariance
  
  print(paste("X Emperical Mean: ", mean(XMeans), "vs X Theoretical Mean: ", XTheoreticalMean))
  print(paste("X Emperical Variance", var(XMeans), "vs X Theoretical Variance:", XTheoreticalVariance))
  print(paste("X - Y Empertical Mean", mean(X_YMeans), "vs X - Y Theoretical Mean", TheoreticalX_YMeans))
  print(paste("X - Y Empertical Variance", var(X_YMeans), "vs X - Y Theoretical Variance", THeoreticalX_YVariance))
  qqPlot(XMeans, main = "X bar  QQ Plot")
  qqPlot(X_YMeans, main = "X - Y QQ Plot")
  
  chi <- (1050/sqrt(XTheoreticalVariance)) * (XMeans - XTheoreticalMean)^2
  hist(chi, 100, probability = TRUE)
  lines(seq(0,5,by=0.1),dchisq(seq(0,5,by=0.1),1),lwd=2)
  
  chin <- (1040 - 1)/XTheoreticalVariance * XVariances
  hist(chin, 100, probability = TRUE)
  lines(seq(0,5,by=0.1),dchisq(seq(0,5,by=0.1),1),lwd=2)
}
verifyNormal()

verifySquaredNormal <- function()
{
  X <- rnorm(1000, 0, 1)^2
  hist(X,100,probability=TRUE)
  lines(seq(0,5,by=0.1),dchisq(seq(0,5,by=0.1),1),lwd=2)
}
verifySquaredNormal()

#Question 6

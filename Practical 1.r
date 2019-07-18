# Question 1

halve <- function(number) { return(number/2)}
name <- "James Bond" 
codename <- 007
charVector <- vector("character", length = 5)
logicMatrix <- matrix(c(TRUE, TRUE, FALSE, FALSE), nrow = 2, ncol = 2, byrow = TRUE)
logicMatrix

oddList <- list(halve, name, codename, charVector, logicMatrix)

# Question 2

nums <-  c(10, 20, 30)
summarize <- function(vector)
{
  print(paste("Sum: ", sum(vector)))
  print(paste("Mean: ", mean(vector)))
  print(paste("Element Product: ", prod(vector)))
}
summarize(nums)

#Quesiton 3

data("mtcars")

paste("The mean of the MPG is", mean(mtcars$mpg))

subCars <- mtcars[mtcars$cyl < 6, ]

library(datasets)
data <- mtcars
list <- 1:32
data <- data.frame(data, list)

lines(data$list, data$hp)


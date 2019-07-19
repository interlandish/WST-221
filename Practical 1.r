# Question 1

halve <- function(number) { return(number/2)}
name <- "James Bond" 
codename <- 007
charVector <- vector("character", length = 5)
charVector <- c("James", "Bond", " KGotso", "Modise", "File", "Debug")
charVector
logicMatrix <- matrix(c(TRUE, TRUE, FALSE, FALSE), nrow = 2, ncol = 2, byrow = TRUE)
logicMatrix

oddList <- list(halve, name, codename, charVector, logicMatrix)
oddList

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

subCars <- rownames(mtcars[mtcars$cyl < 6, ])
list <- 1:nrow(mtcars)
mtcars <- data.frame(mtcars, list)

plot(mtcars$list, mtcars$hp, type = "l")
plot(mtcars$hp, type = "l", ylab = "Horsepower", main = "jqavjwvjch")

# Question 4

quad <- function(a, b, c)
{
  if(a == 0)
  {
    print("The equation is not a quadratic equation")
  }
  else
  {
    desc <- (b^2 - 4 * a * c)
    if(desc < 0)
    {
      print("The equation has no real roots")
    }
    else if(desc == 0)
    {
      root <- (-b)*(2*a)
      print(paste("You equation has a double root: ", root))
    }
    else if(desc > 0)
    {
      root1 <- (-b + sqrt(desc))/(2 * a)
      root2 <- (-b - sqrt(desc))/(2 * a)
      print(paste("Your equation has two real roots: ", root1, " and ", root2))
    }
  }
}

quad(1, 3, 1)

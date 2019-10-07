#Question 1.2

thetavec = seq(0.3, 3, 0.1)

powerA = pchisq(37.57/thetavec, 20, lower.tail = FALSE)
powerB = pchisq(32.42/thetavec, 20, lower.tail = FALSE)
powerC = pchisq(55.76/thetavec, 40, lower.tail = FALSE)

plot(powerA ~ thetavec, type = "b", 
     bty = "l", 
     xlab = "Theta", 
     ylab = "Power", 
     col = "blue", 
     lwd = 3, 
     pch = 5, 
     xlim = c(0.3, 3), 
     main = "Power of the test")

lines(powerB ~ thetavec, col = "red", lwd = 3, pch = 18, type = "b")
lines(powerC ~ thetavec, col = "green", lwd = 3, pch = 14, type = "b")
legend("bottomright", legend = c("Test 1", "Test B", "Test c"), 
                                col = c("blue", "red", "green"),
                                pch = c(5, 18, 14), 
                                pt.cex = 2, 
                                cex = 1.2, 
                                bty = "n",
                                text.col = "black", 
                                horiz = F, 
                                inset = c(0.1, 0.1))



#Question 1.4.3
nvec = seq(20, 30, 1)
dfvector = 2* nvec

chivec = qchisq(0.95, dfvector)

powern = pchisq(chivec/2, dfvector, lower.tail = FALSE)

plot(powern ~ nvec, xlab = "x", ylab = "Power", col = "blue", lwd = 8, pch = 1, ylim = c(min(powern), max(powern)), main = "Power as a function of n")


#Question 2

mu0 = 6
alpha = 0.05
sigma = 10
mu2 = 5.8
beta = 0.1
z1mina = qnorm(1-alpha)
z1minb = qnorm(1 - beta)

muvec = seq(5.9, 7.1, 0.1)
nvec1 =(sigma*(z1minb - z1mina)^2)/((muvec - mu2)^2) 

plot(nvec1 ~ muvec, 
     type = "b", 
     ylab = "n", 
     xlab = "Mu0", 
     col = "blue", 
     lwd = 3, 
     pch = 5, 
     ylim = c(min(nvec1), max(nvec1)), 
     main = "Sample size influenced by n")
#2
sigmavec = seq(5, 20, 1)
nvec2 = (sigmavec*(z1minb - z1mina)^2)/((mu0 - mu2)^2) 

plot(nvec1 ~ sigmavec, 
     type = "b", 
     ylab = "n", 
     xlab = "Mu0", 
     col = "blue", 
     lwd = 3, 
     pch = 5, 
     ylim = c(min(nvec1), max(nvec1)), 
     main = "Sample size influenced by n")
#3
alphavec = seq(0.01, 0.1, 0.01)
zvec1 = qnorm(1 - alphavec)
nvec3 = (sigmavec*(z1minb - z1mina)^2)/((mu0 - mu2)^2) 



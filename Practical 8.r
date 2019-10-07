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

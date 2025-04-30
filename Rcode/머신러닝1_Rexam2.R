getwd()
load("p1.RData")
plot(p1,pch=16,col="red",cex=2)
reg1 <- lm(y~x1,data=p1)
# 선형회귀
abline(reg1, col="blue",lwd = 2)
summary(reg1)

# 비선형
p1$x2 <- p1$x1^2
nlreg1 <- lm(y~ x1+x2 ,data=p1)
w0_hat <- nlreg1$coefficients[1]
w1_hat <- nlreg1$coefficients[2]
w2_hat <- nlreg1$coefficients[3]
summary(nlreg1)
curve(w0_hat+w1_hat*x+w2_hat*x^2,
      col= "darkgreen", lwd=2, add=T)

# SSE SST SSR, R^2
y <- p1$y
y_bar <- mean(y)
y_hat <- nlreg1$fitted.values

SST <- sum((y - y_bar)^2)
SSR <- sum((y_hat - y_bar)^2)
SSE <- sum((y-y_hat)^2)
summary(nlreg1)
R2 <- SSR/SST

# F 통계량
MSR <- SSR/2
MSE <- SSE/97
F <- MSR/MSE 

# p-value
1-pf(MSR,MSE,2,97)

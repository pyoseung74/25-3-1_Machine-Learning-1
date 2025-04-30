X <- 10*c(138,312,352,113,103,172,392,149,186,
       343,200,366,250,122,139)   # 집의 면적
Y <- c(76,216,238,69,50,119,282,81,132,
       228,145,251,170,71,29) # 집 가격
house <- data.frame(X,Y)
reg <- lm(Y ~ X, data = house)
plot(house,pch=16,cex=2)
abline(reg,lwd=2,col="red")
abline(v = 2227, lty=2)
predict(reg, newdata = data.frame(X=c(2227,3000)))

x <- c(36.5,28.0,42.9,52.0,51.5,53.8,25.4,37.2,50.9,29.2)
y <- c(14,9,15,20,21,25,9,13,20,10)
tech <- data.frame(x,y)
plot(x,y,xlab = "광고비",ylab = "신규고객수(100명)",
     pch=16, cex=1.5,col="blue")

reg2 <- lm(x ~ y, data = tech)
predict(reg2, newdata = data.frame(y=17))
 
x <- 10*c(138,312,352,113,103,172,392,149,186,343,200,366,250,122,139)
y <- c(76,216,238,69,50,119,282,81,132,228,145,251,170,71,29)

house <- data.frame(x,y)
plot(house, pch = 16, cex=2)
reg1 <- lm(y~x, data =house)
abline(reg1,col="red",lwd=3)
house$잔차 <- reg1$residuals
house$예측값 <- reg1$fitted.values
house
sum(house$잔차)



# ------------ 4/15

x1 <- 1:10
x2 <- 3:12
x2[c(5,6)] <- 11
y <- c(4,5,8,10,11,14,16,18,20,20)
df <- data.frame(x1,x2,y)

X <- cbind(1,x1,x2)
w_hat <- solve(t(X) %*% X) %*% t(X) %*% y

lm(y ~ .,data=df)

#----------------------------숙제

st <- as.data.frame(state.x77)[,-c(6,8)]
mur <- setdiff(names(st), "Murder")

X0 <- scale(as.matrix(st[mur]))
y  <- st$Murder
x  <- cbind(1, X0)

X <- rep(0, ncol(x))
alpha <- 0.01
for(i in 1:5000) {
  X <- X - alpha * t(x) %*% ( x %*% X - y) / length(y)
}

mu <- attr(X0, "scaled:center")
sig <- attr(X0, "scaled:scale")
beta <- X[-1] / sig
intercept <- X[1] - sum(beta * mu)

cat("경사하강법 결과 : ", round(c(intercept, beta),5), "\n")

# ---------------------

state <- as.data.frame(state.x77)
View(state)
state <- state[,-c(6,8)]

reg2 <- lm(Murder~ .,data=state)
round(reg2$coefficients,5)


# ------------ 숙제 1
install.packages("Deriv")
library(Deriv)

f <- function(x1,x2,x3) (x1 - 4)^2 + x3^2*x1 + (x2 + 1)^2 + 6
x <- c(0,0,0)
alpha <- 0.1

f1_prime <- Deriv(f,"x1")
f2_prime <- Deriv(f,"x2")
f3_prime <- Deriv(f,"x3")
for(i in 1:10000){
  grad <- c(f1_prime(x[1],x[2],x[3]),
            f2_prime(x[1],x[2],x[3]),
            f3_prime(x[1],x[2],x[3]))
  x <- x - alpha*grad
}
print(x)











# -----------------------------------


f2 <- function(x1, x2) {
  (x1 + 1)^4 + x2^2 + 2 * (x1 * x2 - x1 + x2)
}
XX <- C(0,0)
alpha <- 0.01

f_prime_x1 <- Deriv(f,"x1")
f_prime_x2 <- Deriv(f,"x2")
for(i in 1:1000){
  grad <- c(f_prime_x1(x[1],x[2]),
            f_prime_x2(x[1],x[2]))
  x <- XX -alpha*grad
}


# ----------- SSR,SST,SSE

x1 <- c(4,6,3,0,8,7,6,5,1,0)+10
x2 <- c(37,43,38,42,36,33,40,35,34,29)
y <- 10*c(85,97,73,94,92,83,94,90,76,71)

yoplet <- data.frame(x1,x2,y)
reg1 <- lm(y ~ ., data = yoplet)
summary(reg1)

yoplet1 <- data.frame(x1,x2,x3=10,y)
reg2 <- lm(y~., data = yoplet1)
summary(reg2)

# SST
y <- yoplet$y
y_bar <- mean(y)
y_hat <- reg1$fitted.values   # 예측값

SST <- sum((y - y_bar)^2)
SSR <- sum((y_hat - y_bar)^2)
SSE <- SST- SSR
SSR/SST



# -------------- 4/23 자유도
curve(dchisq(x,1),0,50)
curve(dchisq(x,10),0,50,col="red",add=T)
curve(dchisq(x,20),0,50,col="blue",add=T)
curve(dchisq(x,35),0,50,col="green",add=T)

#-------------------- 4/29 
state <- as.data.frame(state.x77)
state <- state[,-c(6,8)]
reg <- lm(Murder ~ ., data = state)
summary(reg)

#
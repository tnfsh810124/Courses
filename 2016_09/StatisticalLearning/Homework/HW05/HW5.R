### 10 ###
install.packages("ISLR")
library(ISLR)
attach(Carseats)
# 10.a. #
lm10a <- lm(Sales ~ Price + Urban + US)
# 10.b. #
summary(lm10a)
# 10.e. #
lm10e <- lm(Sales ~ Price + US)
summary(lm10e)
# 10.g #
confint(lm10e, level = 0.95)
# 10.h #
par(mfrow = c(1,1))
plot(predict(lm10e), rstudent(lm10e))
plot(lm10e)

### 12 ###

# b #
set.seed(1)
x <- rnorm(100, 0, 5)
y <- 5*x+ rnorm(100, 0, 0.1)
lm(y~x+0)
lm(x~y+0)

# c #
set.seed(1)
x <- rnorm(100, 0, 1)
y <- abs(x+rnorm(100, 0, 0.01))
lm(y~x+0)
lm(x~y+0)


### 14 ###
# a #
set.seed (1)
x1=runif (100)
x2 =0.5* x1+rnorm (100) /10
y=2+2* x1 +0.3* x2+rnorm (100)

# b #
cor(x1, x2)
plot(x1,x2)

# c #
lm14c <- lm(y~x1+x2)
summary(lm14c)

# d #
lm14d <- lm(y~x1)
summary(lm14d)

# e #
lm14e <- lm(y~x2)
summary(lm14e)

# g #
x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)

lm14g <- lm(y~ x1+x2)
summary(lm14g)
plot(predict(lm14g), rstudent(lm14g))
plot(lm14g)

lm14g1 <- lm(y~ x1)
summary(lm14g1)
plot(predict(lm14g1), rstudent(lm14g1))
plot(lm14g1)

lm14g2 <- lm(y~ x2)
summary(lm14g2)
plot(predict(lm14g2), rstudent(lm14g2))
plot(lm14g2)


### 15 ###
install.packages("MASS")
library(MASS)
lm <- summary(lm(Boston[, 1]~Boston[, 4]))
lm$coefficients

# a #
Intercept <- 
sapply(2:14, FUN=function(x) {
  lm <- summary(lm(Boston[,1] ~ Boston[,x]));
  lm$coefficients[1,1]
})

Intercept.p <- 
  sapply(2:14, FUN=function(x) {
    lm <- summary(lm(Boston[,1] ~ Boston[,x]));
    lm$coefficients[1,4]
  })

Slope <- 
  Intcpt <- 
  sapply(2:14, FUN=function(x) {
    lm <- summary(lm(Boston[,1] ~ Boston[,x]));
    lm$coefficients[2,1]
  })
cbind(Intercept, Slope)

Slope.p <- 
  Intcpt <- 
  sapply(2:14, FUN=function(x) {
    lm <- summary(lm(Boston[,1] ~ Boston[,x]));
    lm$coefficients[2,4]
  })

Boston.Coef <- data.frame(cbind(Intercept, Intercept.p, Slope, Slope.p))
row.names(Boston.Coef) <- names(Boston)[2:14]
Boston.Coef

par(mfrow = c(2,3))
for (k in 2:14) {
  plot(Boston[,1] ~ Boston[,k], 
       main = names(Boston)[k],
       xlab = names(Boston)[k],
       ylab = names(Boston)[1])
}
par(mfrow = c(1,1))
# b #
attach(Boston)
lm15b <- lm(crim ~ zn+indus+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+medv)
summary(lm15b)


# c #
plot(lm15b$coefficients[2:14] ~ Boston.Coef[,3], xlab="x", ylab = "y")

# d #
lm <- summary(lm(Boston[, 1] ~ poly(Boston[,2],3)))

tmp <- 
cbind(
sapply(c(2,3,5:14), function(x){
  lm <- summary(lm(Boston[, 1] ~ poly(Boston[,x],3)));
  lm$coefficients[1,1]
})
,
sapply(c(2,3,5:14), function(x){
  lm <- summary(lm(Boston[, 1] ~ poly(Boston[,x],3)));
  lm$coefficients[1,4]
})
,
sapply(c(2,3,5:14), function(x){
  lm <- summary(lm(Boston[, 1] ~ poly(Boston[,x],3)));
  lm$coefficients[2,1]
})
,
sapply(c(2,3,5:14), function(x){
  lm <- summary(lm(Boston[, 1] ~ poly(Boston[,x],3)));
  lm$coefficients[2,4]
})
,
sapply(c(2,3,5:14), function(x){
  lm <- summary(lm(Boston[, 1] ~ poly(Boston[,x],3)));
  lm$coefficients[3,1]
})
,
sapply(c(2,3,5:14), function(x){
  lm <- summary(lm(Boston[, 1] ~ poly(Boston[,x],3)));
  lm$coefficients[3,4]
})
,
sapply(c(2,3,5:14), function(x){
  lm <- summary(lm(Boston[, 1] ~ poly(Boston[,x],3)));
  lm$coefficients[4,1]
})
,
sapply(c(2,3,5:14), function(x){
  lm <- summary(lm(Boston[, 1] ~ poly(Boston[,x],3)));
  lm$coefficients[4,4]
})
)

Boston.Coef.d <- data.frame(tmp)
row.names(Boston.Coef.d) <- names(Boston)[c(2,3,5:14)]
names(Boston.Coef.d) <- c("Intercept", "Intercept.p",
                          "x^1","x^1.p",
                          "x^2","x^2.p",
                          "x^3","x^3.p")


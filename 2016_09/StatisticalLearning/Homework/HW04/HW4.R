### 9 ###
Auto <- read.csv("Auto.csv", header = T, sep = ",",na.strings="?")
Auto <- na.omit(Auto)
attach(Auto)
# 9.a. #
pairs(Auto)

# 9.b. #
Auto.cor <- cor(matrix(as.numeric(as.matrix(Auto[,-9])), ncol = 8))
Auto.cor <- data.frame(Auto.cor)
colnames(Auto.cor) <- colnames(Auto)[1:8]
rownames(Auto.cor) <- colnames(Auto)[1:8]

Auto.cor

# 9.c. #
lm.Auto <- lm(mpg ~ cylinders + displacement + horsepower + 
     weight + acceleration + year + origin, data = Auto)

summary(lm.Auto)

# 9.d. #
par(mfrow = c(2,2))
plot(lm.Auto)

par(mfrow = c(1,1))
plot(predict(lm.Auto), rstandard(lm.Auto))


# 9.e. #
lm.Auto2 <- lm(mpg ~ displacement + weight + year + origin +
   displacement:weight + displacement:year + displacement:origin + 
     weight:year + weight:origin + year:origin)

summary(lm.Auto2)


# 9.f. #
lm.Auto3 <- lm(log(mpg) ~ sqrt(displacement) + (weight)^2)
summary(lm.Auto3)
par(mfrow = c(2,2))
plot(lm.Auto3)

par(mfrow = c(1,1))
plot(predict(lm.Auto3), rstandard(lm.Auto3))




### 11 ###
set.seed(1)
x=rnorm (100)
y=2*x+rnorm (100)


# 11.a. #
summary(lm(y~x+0))

# 11.b. #
summary(lm(x~y+0))

# 11.e. #
(sqrt(length(x)-1) * sum(x*y)) / (sqrt(sum(x*x) * sum(y*y) - (sum(x*y))^2))

# 11.f. #
lme1 <- summary(lm(y~x))
lme2 <- summary(lm(x~y))
lme1$coefficients
lme2$coefficients


### 13 ###
set.seed(1)
# 13.a. #
x <- rnorm(100,0,1)

# 13.b. #
eps <- rnorm(100,0,0.25)

# 13.c. #
y <- -1 + 0.5*x +eps
length(y)

# 13.d. #
plot(y ~ x)

# 13.e. #
lm13e <- lm(y~x)
confint(lm13e)
# 13.f. #
plot(y~x)
abline(lm13e, col= "red")

# 13.g. #
lm13g <- lm(y ~ x + I(x^2))
summary(lm13g)

# 13.h. #
eps <- rnorm(100, 0, 0.1)
y <- -1 + 0.5*x +eps
lm13h <- lm(y~x)
plot(y~x)
abline(lm13h, col= "red")
summary(lm(y ~ x + I(x^2)))
confint(lm13h)

# 13.i. #
eps <- rnorm(100, 0, 0.5)
y <- -1 + 0.5*x +eps
lm13i <- lm(y~x)
plot(y~x)
abline(lm13i, col= "red")
summary(lm(y ~ x + I(x^2)))
confint(lm13i)

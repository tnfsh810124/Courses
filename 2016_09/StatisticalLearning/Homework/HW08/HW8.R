library(ISLR)
library(boot)
library(MASS)
### Chapter 5 ###
# 6 #
data("Default")
# a
set.seed(12)
fit6a <- glm(default ~ income + balance, 
             data = Default, family = binomial)
summary(fit6a)
# b
boot.fn <- function(data, index){
  fit.fn <- glm(default ~ income + balance, data = data, 
                family = binomial, subset = index);
  fit.fn$coefficients
}
# c
bt.6c <- boot(Default, boot.fn, 87)
bt.6c
# d

# 7 #
# a
fit7a <- glm(Direction ~ Lag1 + Lag2, data = Weekly, family = binomial)
summary(fit7a)
# b
fit7b <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-1,], family = binomial)
summary(fit7b)
# c
predict7c <- ifelse(predict(fit7b, Weekly[1,2:3], type = "response") > .5,
                    "Up", "Down")
predict7c == Weekly$Direction[1]
# d 
error7d <- 
  sapply(1:dim(Weekly)[1], function(n){
    fit7d <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-n,], family = binomial);
    predict7d <- ifelse(predict(fit7d, Weekly[n,2:3], type = "response") >= .5,
                        "Up", "Down")
    predict7d != Weekly$Direction[n]
  })
sum(error7d)
# e
mean(error7d)

# 8 #
# a
set.seed(1)
y <- rnorm(100)
x <- rnorm(100)
y <- x-2*x^2+rnorm(100)
df8 <- data.frame(x, y)
# b 
plot(y ~ x, asp =1)
# c 
set.seed(3)
fit8ci   <- glm(y ~ x)
fit8cii  <- glm(y ~ poly(x, 2))
fit8ciii <- glm(y ~ poly(x, 3))
fit8civ  <- glm(y ~ poly(x, 4))
cv.glm(df8, fit8ci  )$delta
cv.glm(df8, fit8cii )$delta
cv.glm(df8, fit8ciii)$delta
cv.glm(df8, fit8civ )$delta
# d
set.seed(4)
fit8ci   <- glm(y ~ x)
fit8cii  <- glm(y ~ poly(x, 2))
fit8ciii <- glm(y ~ poly(x, 3))
fit8civ  <- glm(y ~ poly(x, 4))
cv.glm(df8, fit8ci  )$delta
cv.glm(df8, fit8cii )$delta
cv.glm(df8, fit8ciii)$delta
cv.glm(df8, fit8civ )$delta
# e 
# f
summary(fit8civ)

# 9 #
# a
mu <- mean(Boston$medv)
# b
se <- sd(Boston$medv)/sqrt(length(Boston$medv))
# c
boot.fn <- function(data, index) return(mean(data[index]))
set.seed(87)
bstrp <- boot(Boston$medv, boot.fn, 5487)
# d
t.test(Boston$medv)$conf[1:2]
c(bstrp$t0 - 2*0.4101611, bstrp$t0 + 2*0.4101611)
# e
med <- median(Boston$medv)
# f
boot.fn <- function(data, index) return(median(data[index]))
set.seed(87)
boot(Boston$medv, boot.fn, 5487)
# g
quant10 <- quantile(Boston$medv, .1)
# h
boot.fn <- function(data, index) return(quantile(data[index], .1))
set.seed(87)
boot(Boston$medv, boot.fn, 5487)
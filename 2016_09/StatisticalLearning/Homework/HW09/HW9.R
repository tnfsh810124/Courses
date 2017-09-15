# 8 # 

# a
set.seed(1)
X   <- rnorm(100)
eps <- rnorm(100)

# b
b0 <- 2.99
b1 <- 2.2
b2 <- -3.5
b3 <- 0.2
Y  <- b0 + b1*X + b2*X^2 + b3*X^3 + eps

# c
# install.packages("leaps")
library(leaps)
df  <- data.frame(y = Y, x = X)
mod.c <- regsubsets(y ~ poly(x, 10, raw = T), data = df, nvmax = 10)
ms.c  <- summary(mod.c)
c(which.min(ms.c$cp), which.min(ms.c$bic), which.max(ms.c$adjr2))
par(mfrow = c(1,3))
for(i in c(5, 6)){
  plot(ms.c[[i]], type = "l", main = "Default", xlab = "subset size", ylab = names(ms.c)[i], 
       col = gray(.5))
  points(which.min(ms.c[[i]]), min(ms.c[[i]]), pch = 16, col = "orange2", cex = 1.5)
}
plot(ms.c[[4]], type = "l", main = "Default", xlab = "subset size", ylab = names(ms.c)[4], 
     col = gray(.5))
points(which.max(ms.c[[4]]), max(ms.c[[4]]), pch = 16, col = "orange2", cex = 1.5)
coefficients(mod.c, id = 3)

# d
# forward
df  <- data.frame(y = Y, x = X)
mod.fwd <- regsubsets(y ~ poly(x, 10, raw = T), data = df, nvmax = 10)
ms.fwd  <- summary(mod.fwd)
c(which.min(ms.fwd$cp), which.min(ms.fwd$bic), which.max(ms.fwd$adjr2))
par(mfrow = c(1,3))
for(i in c(5, 6)){
  plot(ms.fwd[[i]], type = "l", main = "Forward", xlab = "subset size", ylab = names(ms.fwd)[i], 
       col = gray(.5))
  points(which.min(ms.fwd[[i]]), min(ms.fwd[[i]]), pch = 16, col = "orange2", cex = 1.5)
}
plot(ms.fwd[[4]], type = "l", main = "Forward", xlab = "subset size", ylab = names(ms.fwd)[4], 
     col = gray(.5))
points(which.max(ms.fwd[[4]]), max(ms.fwd[[4]]), pch = 16, col = "orange2", cex = 1.5)
coefficients(mod.fwd, id = 3)

# backward
df  <- data.frame(y = Y, x = X)
mod.bwd <- regsubsets(y ~ poly(x, 10, raw = T), data = df, nvmax = 10)
ms.bwd  <- summary(mod.bwd)
c(which.min(ms.bwd$cp), which.min(ms.bwd$bic), which.max(ms.bwd$adjr2))
par(mfrow = c(1,3))
for(i in c(5, 6)){
  plot(ms.bwd[[i]], type = "l", main = "Backward", xlab = "subset size", ylab = names(ms.bwd)[i], 
       col = gray(.5))
  points(which.min(ms.bwd[[i]]), min(ms.bwd[[i]]), pch = 16, col = "orange2", cex = 1.5)
}
plot(ms.bwd[[4]], type = "l", main = "Backward", xlab = "subset size", ylab = names(ms.bwd)[4], 
     col = gray(.5))
points(which.max(ms.bwd[[4]]), max(ms.bwd[[4]]), pch = 16, col = "orange2", cex = 1.5)
coefficients(mod.bwd, id = 3)

# e
# install.packages("glmnet")
library(glmnet)
xmat <- model.matrix(y ~ poly(x, 10, raw = T), data = df)[, -1]
mod.lasso   <-  cv.glmnet(xmat, Y, alpha = 1)
best.lambda <-  mod.lasso$lambda.min
best.lambda
par(mfrow = c(1,1))
plot(mod.lasso)
best.model <- glmnet(xmat, Y, alpha = 1)
predict(best.model, s = best.lambda, type = "coefficients")

# f
b7 <-  6.9
Y  <-  b0 + b7 * X^7 + eps
df <- data.frame(y = Y, x = X)
mod.f <- regsubsets(y ~ poly(x, 10, raw = T), data = df, nvmax = 10)
ms.f  <- summary(mod.f)
c(which.min(ms.f$cp), which.min(ms.f$bic), which.max(ms.f$adjr2))
sapply(c(2, 1, 4), function(k) coefficients(mod.f, id = k))

xmat <- model.matrix(y ~ poly(x, 10, raw = T), data = df)[, -1]
mod.lasso   <-  cv.glmnet(xmat, Y, alpha = 1)
best.lambda <-  mod.lasso$lambda.min
best.lambda
par(mfrow = c(1,1))
plot(mod.lasso)
best.model <- glmnet(xmat, Y, alpha = 1)
predict(best.model, s = best.lambda, type = "coefficients")

# 9 #
library(ISLR)
set.seed(11)
# a
sample.num <- sample(1:dim(College)[1], round(0.5*dim(College)[1]))
train <- College[ sample.num , ]
test  <- College[-sample.num , ]

# b
fit <- lm(Apps ~. , data = train)
lm.prd <- predict(fit, test)
mean((test$Apps - lm.prd)^2)

# c
library(glmnet)
train.mat <- model.matrix(Apps ~ . , data = train)
test.mat  <- model.matrix(Apps ~ . , data = test )
grid      <- 10^seq(4, -2, length = 100)
mod.ridge <- cv.glmnet(train.mat, train$Apps, alpha = 0, lambda = grid, thresh = 1e-12)
lambda.best <- mod.ridge$lambda.min
lambda.best
ridge.prd <- predict(mod.ridge, newx = test.mat, s = lambda.best)
mean((test$Apps - ridge.prd)^2)

# d
mod.lasso <-  cv.glmnet(train.mat, train$Apps, alpha = 1, lambda = grid, thresh = 1e-12)
lambda.best <- mod.lasso$lambda.min
lambda.best
lasso.prd <- predict(mod.lasso, newx = test.mat, s = lambda.best)
mean((test$Apps - lasso.prd)^2)
mod.lasso <- glmnet(model.matrix(Apps~., data=College), College$Apps, alpha = 1)
predict(mod.lasso, s = lambda.best, type = "coefficients")

# e 
#install.packages("pls")
library(pls)
pcr.fit <- pcr(Apps ~ ., data = train, scale = T, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
pcr.prd <- predict(pcr.fit, test, ncomp = 10)
mean((test$Apps - data.frame(pcr.prd))^2)

# f
pls.fit <-  plsr(Apps~., data = train, scale = T, validation = "CV")
validationplot(pls.fit, val.type = "MSEP")
pls.prd <- predict(pls.fit, test, ncomp = 10)
mean((test$Apps - data.frame(pls.prd))^2)

# g
test.avg <- mean(test[, "Apps"])
lm.test.r2 <- 1 - mean((test[, "Apps"] - lm.prd)^2) /mean((test[, "Apps"] - test.avg)^2)
ridge.test.r2 <- 1 - mean((test[, "Apps"] - ridge.prd)^2) /mean((test[, "Apps"] - test.avg)^2)
lasso.test.r2 <- 1 - mean((test[, "Apps"] - lasso.prd)^2) /mean((test[, "Apps"] - test.avg)^2)
pcr.test.r2 <- 1 - mean((test[, "Apps"] - data.frame(pcr.prd))^2) /mean((test[, "Apps"] - test.avg)^2)
pls.test.r2 <- 1 - mean((test[, "Apps"] - data.frame(pls.prd))^2) /mean((test[, "Apps"] - test.avg)^2)
barplot(c(lm.test.r2, ridge.test.r2, lasso.test.r2, pcr.test.r2, pls.test.r2), col = gray(.9), 
        names.arg=c("OLS", "Ridge", "Lasso", "PCR", "PLS"), main="Test R-squared")

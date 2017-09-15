#install.packages("car")
#install.packages("ISLR")
library(ISLR)
library(MASS)
library(class)
library(car)
credit.train  <- read.csv("~/Desktop/M052040003/credit/training_data.csv")
credit.test   <- read.csv("~/Desktop/M052040003/credit/test_data.csv")
data2.train   <- read.csv("~/Desktop/M052040003/data2/training_data.csv")
data2.test    <- read.csv("~/Desktop/M052040003/data2/test_data.csv")
disease.train <- read.csv("~/Desktop/M052040003/disease/training_data.csv")
disease.test  <- read.csv("~/Desktop/M052040003/disease/test_data.csv")

### 1 ###
# a #
# i
xnam <- paste0("x", 1:350)
fmla <- as.formula(paste("y ~", paste(xnam, collapse= "+")))
fit.1.a.i <- lda(fmla, data = disease.train)
tr.err.i <- length(disease.train[predict(fit.1.a.i, disease.train)$class != disease.train$y, 1])/2700
te.err.i <- length(disease.test[predict(fit.1.a.i, disease.test)$class != disease.test$y, 1])/300
# ii
choose <- 1:350
Bh <- function(h) choose[sapply(1:350, function(x) sum(abs(disease.train[,x ]))/sum(abs(disease.train[, -351])) >= 0.001*h)]
B1 <- Bh(1)
length(B1)
# iii
xnam <- paste0("x", B1)
fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+")))
fit.1.a.iii <- lda(fmla, data = disease.train)
tr.err.iii <- length(disease.train[predict(fit.1.a.iii, disease.train)$class != disease.train$y, 1])/2700
te.err.iii <- length(disease.test[predict(fit.1.a.iii, disease.test)$class != disease.test$y, 1])/300
# iv & v
err.compare <- data.frame(matrix(c(tr.err.i, te.err.i, tr.err.iii, te.err.iii), nrow = 2))
names(err.compare) <- c("original", "B1")
row.names(err.compare) <- c("training error", "test error")
err.compare

# b #
xnam <- xnam <- paste0("x", B.0.8[[4]])
qda(fmla, data = disease.train[B.0.8[[4]]])
#  Error in qda.default(x, grouping, ...) : 
#  some group is too small for 'qda'
B.0.8 <- sapply(0:8, function(h) Bh(h))
det.cov.m <- data.frame(matrix(NA, nrow = 10, ncol = 9))
for(h in 1:9){
  det.cov.m[, h] <- sapply(1:10, function(k) det(cov(disease.train[disease.train$y == k, B.0.8[[h]]] )))
}
names(det.cov.m) <- paste0("B", 0:8)

err.1.b <- 
  sapply(2:8, function(x) {
    xnam <- paste0("x", Bh(x))
    fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+")))
    fit.1.b <- qda(fmla, data = disease.train)
    c(length(disease.train[predict(fit.1.b, disease.train)$class != disease.train$y, 1])/2700, 
      length( disease.test[predict(fit.1.b, disease.test)$class != disease.test$y, 1])/300)
  })
err.1.b <- data.frame(t(err.1.b))
err.1.b <- cbind(2:8, err.1.b)
names(err.1.b) <- c("Bh", "train", "test")
plot(err.1.b$train ~ err.1.b$Bh, type = "b", ylim = range(err.1.b[, 2:3]),
     xlab = "Bh", ylab = "error rate", main = "Comparison of errors for QDA", 
     col = "gray47", pch = 16)
points(err.1.b$test  ~ err.1.b$Bh, type = "b", col = "blue3", pch = 16)
abline(v = err.1.b$Bh[which.min(err.1.b$test)], col = "red", lty = 2)
legend(x = 1.89, y = 0.06, legend = c("training error rate", "test error rate", "minimum test error rate"), 
       pch = c(16,16,95), col = c("gray47", "blue3", "red"))


# c # #################################################
xnam <- paste0("x", choose);
fmla <- as.formula(paste("yi ~ ", paste(xnam, collapse= "+")))
predict.1.c.tr <- matrix(NA, nrow = 2700, ncol = 10)
predict.1.c.te <- matrix(NA, nrow =  300, ncol = 10)
for(i in 1:10){
    yi <- as.factor(ifelse(disease.train$y == i, 1, 0));
    fit.b <- glm(fmla, data = disease.train, family = "binomial");
    predict.1.c.tr[, i] <- predict(fit.b, disease.train[, -351], type = "response")
    predict.1.c.te[, i] <- predict(fit.b, disease.test[ , -351], type = "response")
}
predict.1.c.tr <- as.data.frame(predict.1.c.tr)
class.1.c.tr <- apply(predict.1.c.tr, MARGIN = 1, function(x) which.max(x))
table(disease.train$y, class.1.c.tr)
tr.err.c <- 1 - sum(diag(table(disease.train$y, class.1.c.tr)))/2700

predict.1.c.te <- as.data.frame(predict.1.c.te)
class.1.c.te <- apply(predict.1.c.te, MARGIN = 1, function(x) which.max(x))
table(disease.test$y, class.1.c.te)
te.err.c <- 1 - sum(diag(table(disease.test$y, class.1.c.te)))/300

# d #
err.d <- 
sapply(1:10, function(h) {
  predict.1.d.tr <- knn(disease.train, disease.train, disease.train$y, k=10*h);
  predict.1.d.te <- knn(disease.train, disease.test , disease.train$y, k=10*h);
  c(1 - sum(diag(table(disease.train$y, predict.1.d.tr)))/2700, 
    1 - sum(diag(table(disease.test$y , predict.1.d.te)))/300)
})
err.d <- data.frame(cbind(1/((1:10)*10), t(err.d)))
names(err.d) <- c("1/K", "train", "test")
row.names(err.d) <- paste("k =", (1:10)*10)
plot(err.d$train ~ err.d$`1/K`, type = "b", ylim = range(err.d[, 2:3]),
     xlab = "1/K", ylab = "error rate", main = "Comparison of errors for KNN", 
     col = "gray47", pch = 16)
points(err.d$test  ~ err.d$`1/K`, type = "b", col = "blue3", pch = 16)
abline(v = err.d$`1/K`[which.min(err.d$test)], col = "red", lty = 2)
legend(x = 0.035, y = 0.085, legend = c("training error rate", "test error rate", "minimum test error rate"), 
       pch = c(16,16,95), col = c("gray47", "blue3", "red"))
# e #

### 2 ###

# a #
err.compare.2 <- 
sapply(1:20, function(M){
  xnam <- paste0("x", 1:M);
  fmla <- as.formula(paste("y ~ 0 + ", paste(xnam, collapse= "+")));
  fit.2.a <- lm(fmla, data = data2.train);
  tr.err.2a <- (1/(150-M))*sum(data2.train$y - predict(fit.2.a ,data2.train))^2;
  te.err.2a <- (1/150    )*sum((data2.test$y - predict(fit.2.a ,data2.test))^2);
  c(tr.err.2a, te.err.2a)
})
err.compare.2 <- data.frame(t(err.compare.2))
names(err.compare.2) <- c("train", "test")
plot(err.compare.2$train , type = "b", ylim = range(err.compare.2),
     xlab = "The M-th model", ylab = "error rate", main = "Comparison errors for model M", 
     col = "gray47", pch = 16)
points(err.compare.2$test, type = "b", col = "blue3", pch = 16)
abline(v = which.min(err.compare.2$test), col = "red", lty = 2)
abline(v = which.min(err.compare.2$train), col = "green", lty = 2)
legend(x = 6, y = 7, 
       legend = c("training error", "test error", 
                  "minimum training error", "minimum test error"), 
       pch = c(16,16,95,95), col = c("gray47", "blue3", "green", "red"))


# b #
# i
choose2 <- 1:20
xnam <- paste0("x", choose2);
fmla <- as.formula(paste("y ~ 0 + ", paste(xnam, collapse= "+")));
fit.2.b <- lm(fmla, data = data2.train)
mse2.all <- mean((data2.test$y - predict(fit.2.b, data2.test))^2)
which(vif(fit.2.b)> 10)

xnam <- paste0("x", choose2[-1]);
fmla <- as.formula(paste("y ~ 0 + ", paste(xnam, collapse= "+")));
fit.2.b.v1 <- lm(fmla, data = data2.train)
mse2.1 <- mean((data2.test$y - predict(fit.2.b.v1, data2.test))^2)

xnam <- paste0("x", choose2[-8]);
fmla <- as.formula(paste("y ~ 0 + ", paste(xnam, collapse= "+")));
fit.2.b.v8 <- lm(fmla, data = data2.train)
mse2.8 <- mean((data2.test$y - predict(fit.2.b.v8, data2.test))^2)

xnam <- paste0("x", choose2[-c(1, 8)]);
fmla <- as.formula(paste("y ~ 0 + ", paste(xnam, collapse= "+")));
fit.2.b.v18 <- lm(fmla, data = data2.train)
mse2.18 <- mean((data2.test$y - predict(fit.2.b.v18, data2.test))^2)

te.err.2.b <- data.frame(c(mse2.all, mse2.1, mse2.8, mse2.18))
names(te.err.2.b) <- "test error"
row.names(te.err.2.b) <- c("model 20", "x1 removed", "x8 removed", "x1 and x8 removed")
row.names(te.err.2.b)[which.min(te.err.2.b$`test error`)]

# ii 
fit.2.M1 <- lm(y ~ 0 + x1, data = data2.train)
hi <- (1/150) + (((data2.train$x1 - mean(data2.train$x1))^2)/(149*var(data2.train$x1)))
which.max(hi)
x1n <- data2.train$x1
yn  <- data2.train$y
yn[which.max(hi)] <- yn[which.max(hi)] + sd(yn)
fit.2.M1.n <- lm(yn ~ 0 + x1n)
slope.change.rate <- (fit.2.M1.n$coefficients[1] - fit.2.M1$coefficients[1])/fit.2.M1$coefficients[1]
names(slope.change.rate) <- "change rate of slope estimator"


### 3 ###
# a #
credit.train <- as.data.frame(credit.train)
sapply(6:9, function(x) {
  credit.train[, x] <- as.factor(credit.train[,x]); 
  credit.test[, x]  <- as.factor(credit.test[,x] );
})

xnam <- paste0("x", 1:10)
names.3 <- names(credit.test)
names(credit.train) <- c("y", xnam)
names(credit.test)  <- c("y", xnam)
fmla <- as.formula(paste("y ~", paste(c(xnam[1:5], paste0("as.factor(", xnam[6:9], ")"), xnam[10]), collapse = "+")))
fit.3.a <- lm(fmla, data = credit.train)
summary(fit.3.a)

err.3.a <- 
  c((1/(length(credit.train$y) - 12))*sum((credit.train$y - predict(fit.3.a, newdata = credit.train, type = "response"))^2),
    (1/(length(credit.test$y )     ))*sum((credit.test$y  - predict(fit.3.a, newdata = credit.test , type = "response"))^2))
names(err.3.a) <- c("training error", "test error")
err.3.a


# b #
fit.3.b <- lm(y ~ x1 + as.factor(x7) + x10, data = credit.train)
summary(fit.3.b)
confint(fit.3.b)
err.3.b <- 
  c((1/(length(credit.train$y) - 4))*sum((credit.train$y - predict(fit.3.b, newdata = credit.train, type = "response"))^2),
    (1/(length(credit.test$y )     ))*sum((credit.test$y  - predict(fit.3.b, newdata = credit.test , type = "response"))^2))
names(err.3.b) <- c("train", "test")
err.compare.3 <- rbind(err.3.a, err.3.b)
row.names(err.compare.3) <- c("model a", "model b")

# c #
plot(fit.3.b)

# d #
xnam <- paste0("x", 1:10)
x7xi <- paste0("as.factor(x7):", c(xnam[1:5], paste0("as.factor(", xnam[c(6,8,9)], ")"), xnam[10]))
fmla <- unclass(sapply(1:9, function(x) as.formula(paste0("y ~ x1 + as.factor(x7) + x10 +", x7xi[x]))))

err.3.c <- 
  sapply(1:9, function(x){
    fit.3.c <- lm(fmla[[x]], data = credit.train)
    c((1/(length(credit.train$y) - length(fit.3.c$coefficients) ))*sum((credit.train$y - predict(fit.3.c, newdata = credit.train, type = "response"))^2),
      (1/(length(credit.test$y )                                ))*sum((credit.test$y  - predict(fit.3.c, newdata = credit.test , type = "response"))^2))
  })
err.3.c <- data.frame(t(err.3.c))
names(err.3.c) <- c("train", "test")
row.names(err.3.c) <- paste0("x7:", xnam[c(1:6, 8:10)])
which.min(err.3.c$test)
err.compare.3 <- rbind(err.3.a, err.3.b, err.3.c)
row.names(err.compare.3)[1:2] <- c("model a", "model b")

plot(err.compare.3$train[-1] , type = "b", ylim = range(err.compare.3),
     xlab = "The M-th model", ylab = "error rate", main = "Comparison errors for models in problem 3(d)", 
     col = "gray47", pch = 16, xaxt = "n")
points(err.compare.3$test[-1], type = "b", col = "blue3", pch = 16)
abline(v = which.min(err.compare.3$test[-1]), col = "red", lty = 2)
legend(x = 5, y =113, 
       legend = c("training error", "test error", "minimum test error"), 
       pch = c(16,16,95), col = c("gray47", "blue3", "red"))
axis(1, 1:10, c("model b", paste0("x7:", xnam[c(1:6, 8:10)])))

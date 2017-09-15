##################################
#####Statistical Learning HW3#####
##################################

### Chapter 2 ###

## 7 ##
X1 <- c(0, 2, 0, 0,-1, 1)
X2 <- c(3, 0, 1, 1, 0, 1)
X3 <- c(0, 0, 3, 2, 1, 1)
Y  <- c("red","red","red","green","green","red") 

data.2.7 <- cbind(X1, X2, X3, Y)
data.2.7 <- data.frame(data.2.7)
#mode(X1)

V <- as.vector(data.2.7[, 1:3])

# 7.a #
VV <- function(x) array(c(as.numeric(as.character(V[x, 1])), as.numeric(as.character(V[x, 2])), as.numeric(as.character(V[x, 3]))))
sqrt(sum((VV(x)-array(c(0,0,0)))^2))
eucl.d <- function(x) sqrt(sum((VV(x)-array(c(0,0,0)))^2))
eu.dist <- apply(matrix(1:6, 1, 6), MARGIN = 2, FUN = eucl.d)

data.2.7 <- cbind(data.2.7, eu.dist)



### Chapter 3 ###

## 8 ##
Auto <- read.csv("Auto.csv", header = T, sep = ",")

# 8.a. #
attach(Auto)
names(Auto)
lm8 <- lm(mpg ~ horsepower)
summary(lm8)

CI <- predict(lm8, data.frame(horsepower=98),
              interval = "confidence", level = 0.95)
PI <- predict(lm8, data.frame(horsepower=98),
              interval = "prediction", level = 0.95)

Int.8a <- rbind(CI,PI)
Int.8a <- data.frame(t(cbind(Int.8a[,2], Int.8a[,1] , Int.8a[,3])))
names(Int.8a) <- c("confidence", "prediction")
Int.8a <- t(Int.8a)
colnames(Int.8a) <- c("lower", "fit", "upper")

Int.8a


# 8.b. #
par(mfrow = c(1,1))
plot(mpg ~ horsepower)
abline(lm8, col = "red")


# 8.c. #
par(mfrow = c(2,2))
plot(lm8)
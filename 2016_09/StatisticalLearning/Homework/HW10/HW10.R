# 6 #
# a
y <- 3
lambda <- 5
betas <- seq(-10,10,0.1)
func <- (y-betas)^2+lambda*betas^2
plot(betas,func,type="l",xlab="beta",ylab="Ridge optimization")
est.beta <- y/(1+lambda)
est.func <- (y-est.beta)^2+lambda*est.beta^2
points(est.beta,est.func,col="red",pch=16,lwd=5,cex=1)
# b
betas <- seq(-3,3,0.01)
func<- (y-betas)^2+lambda*abs(betas)
plot(betas,func,type="l",xlab="beta",ylab="Lasso optimization")
est.beta <- y-lambda/2
est.func <- (y-est.beta)^2+lambda*abs(est.beta)
points(est.beta,est.func,col="red",pch=16,cex=1)

# 10 #
# a
set.seed(1)
p <- 20
n <- 1000
x <- matrix(rnorm(n*p),n,p)
B <- rnorm(p)
B[3]  <- 0
B[4]  <- 0
B[9]  <- 0
B[10] <- 0
B[19] <- 0
eps <- rnorm(n)
y <- x %*% B + eps

# b
train <- sample(seq(1000),100,replace=FALSE)
y.train <- y[ train,]
y.test  <- y[-train,]
x.train <- x[ train,]
x.test  <- x[-train,]

# c
library(leaps)
regfit.full <- regsubsets(y~.,data=data.frame(x=x.train,y=y.train),nvmax=p)
val.errors <- rep(NA, p)
x_cols <- colnames(x, do.NULL=FALSE, prefix="x.")
for(i in 1:p)
{
  coefi <- coef(regfit.full,id=i)
  pred  <- as.matrix(x.train[,x_cols%in%names(coefi)])%*%coefi[names(coefi)%in%x_cols]
  val.errors[i] <- mean((y.train-pred)^2)
}
plot(val.errors,ylab="Training MSE",pch=19,type="b")

# d
val.errors <- rep(NA,p)
for(i in 1:p)
{
  coefi <- coef(regfit.full,id=i)
  pred <- as.matrix(x.test[,x_cols%in%names(coefi)])%*%coefi[names(coefi)%in%x_cols]
  val.errors[i] <- mean((y.test-pred)^2)
}
plot(val.errors, ylab = "Test MSE", pch=19, type="b")

# e
which.min(val.errors)

# f
coef(regfit.full, id=14)

# g
val.errors <- rep(NA,p)
a <- rep(NA,p)
b <- rep(NA, p)
for(i in 1:p)
{
  coefi <- coef(regfit.full,id=i)
  a[i]  <- length(coefi)-1
  b[i]  <- sqrt(sum((B[x_cols%in%names(coefi)]-coefi[names(coefi)%in%x_cols])^2)+sum(B[!(x_cols%in%names(coefi))])^2)
}
plot(x=a, y=b, xlab="number of coefficients", ylab="error between estimated and true coefficients")

which.min(b)

# 11 #
# a
set.seed(1)
library(MASS)
library(leaps)
library(glmnet)

# Best subset selection #
predict.regsubsets <- function(object,newdata,id,...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object,id=id)
  mat[,names(coefi)] %*% coefi
}

k <- 10
p <- ncol(Boston)-1
folds <- sample(rep(1:k,length=nrow(Boston)))
cv.errors <- matrix(NA,k,p)
for(i in 1:k)
{
  best.fit <- regsubsets(crim ~ ., data = Boston[folds!=i, ], nvmax = p)
  for(j in 1:p)
  {
    pred <- predict(best.fit,Boston[folds==i,],id=j)
    cv.errors[i,j] <- mean((Boston$crim[folds==i]-pred)^2)
  }
}
rmse.cv <- sqrt(apply(cv.errors,2,mean))
plot(rmse.cv, pch=19, type="b")

which.min(rmse.cv)
rmse.cv[which.min(rmse.cv)]

#Lasso#
x <- model.matrix(crim~.-1,data=Boston)
y <- Boston$crim
cv.lasso <- cv.glmnet(x,y,type.measure="mse")
plot(cv.lasso)

coef(cv.lasso)

sqrt(cv.lasso$cvm[cv.lasso$lambda == cv.lasso$lambda.1se])

#Ridge regression#
x <- model.matrix(crim~.-1,data=Boston)
y <- Boston$crim
cv.ridge <- cv.glmnet(x, y, type.measure="mse", alpha=0)
plot(cv.ridge)

coef(cv.ridge)

sqrt(cv.ridge$cvm[cv.ridge$lambda == cv.ridge$lambda.1se])

#PCR#
library(pls)
pcr.fit <- pcr(crim~., data = Boston, scale = TRUE, validation = "CV")
summary(pcr.fit)


attach(iris)

### 1.a ###
par(mfrow = c(4, 4))

for (i in 1:4) {
  for (j in 1:4){
    if(i==j){
      plot(iris[, i]~iris$Species, 
           xlab="Species", ylab = " ",
           main = names(iris)[i], 
           col = c(0,2,3))
    }
    else{
      plot(iris[,i]~iris[,j], 
           xlab = " ", ylab = " "
           , col = Species)
    }
  }
}


### 1.b ###

# histogram and density plot #
par(mfrow = c(2,2))

for (k in 1:4) {
  x <- iris[ ,k]
  h<-hist(x, breaks=10, 
          main = names(iris)[k], xlab = " ",
          ylim = c(0,40)) 
  xfit<-seq(min(x),max(x),length=40) 
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
  yfit <- yfit*diff(h$mids[1:2])*length(x) 
  lines(xfit, yfit, col="red", lwd=2)
}

# Q-Q plot #
par(mfrow = c(2,2))
for (k in 1:4) {
  qqnorm(iris[,k], main = names(iris)[k])
  qqline(iris[,k], col="red")
}


### 1.c ###
iris.summary <- summary(iris)
iris.mean <- matrix(data = c(5.843, 3.057, 3.758, 1.199), 4, 1)

iris.cov <- cov(iris[, 1:4])
iris.cor <- cor(iris[, 1:4])

V <- diag(NA, 4)
for (k in 1:4) {
  V[k, k] <- cov(iris[, 1:4])[k, k]
}

solve(V^(1/2))%*%as.matrix(iris.cov)%*%solve(V^(1/2))

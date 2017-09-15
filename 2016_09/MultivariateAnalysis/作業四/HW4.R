########################### Multivariate Analysis ###########################
#install.packages("pwr")
library(pwr)
library(nortest)

### 6.12 ###
x1=c(6.4,6.8,7.3,7.0)
x2=c(4.3,4.9,5.3,5.1)
sum=x1+x2
c1=c(1,-2,1,0)
c2=c(0,1,-2,1)
c=cbind(c1,c2)
s1=c(0.61,0.26,0.07,0.16)
s2=c(0.26,0.64,0.17,0.14)
s3=c(0.07,0.17,0.81,0.03)
s4=c(0.16,0.14,0.03,0.31)
s=cbind(s1,s2,s3,s4)
w=(1/30+1/30)*t(c)%*%s%*%c
inv=solve(w)
t.sq=t(sum)%*%c%*%inv%*%t(c)%*%sum
t.sq
f=qf(0.95,2,30+30-4+1)
c.sq=(30+30-2)*2/(30+30-4+1)*f
c.sq
t.sq>c.sq

### 6.23. ###
attach(iris)
MANOVA.23 <- manova(cbind(Sepal.Width, Petal.Width) ~ Species)
summary(MANOVA.23, test = "Wilks")
x <- as.matrix(cbind(Sepal.Width, Petal.Width))
xb <- t(as.matrix(c(mean(Sepal.Width), mean(Petal.Width))))
x1 <- x[1:50   , ]
x2 <- x[51:100 , ]
x3 <- x[101:150, ]
xb1 <- c(mean(x1[,1]), mean(x1[,2]))
xb2 <- c(mean(x2[,1]), mean(x2[,2]))
xb3 <- c(mean(x3[,1]), mean(x3[,2]))
B <- 50*(t(xb1 - xb) %*% (xb1 - xb)) + 
  50*(t(xb2 - xb) %*% (xb2 - xb)) + 
  50*(t(xb3 - xb) %*% (xb3 - xb))
W1 <- sapply(1:50, function(i) x1[i, ]-xb1)
W2 <- sapply(1:50, function(i) x2[i, ]-xb2)
W3 <- sapply(1:50, function(i) x3[i, ]-xb3)
W <- W1%*%t(W1) + W2%*%t(W2) + W3%*%t(W3)
W <- as.matrix.data.frame(W)
B
W
B+W
d <- qt(p=(0.05/(2*3*2)), 147, lower.tail = F)*sqrt(diag(W)/(25*147))
d12 <- xb1 - xb2
d13 <- xb1 - xb3
d23 <- xb2 - xb3
Simu.CI <- data.frame(t(cbind(rbind(d12-d, d12+d), rbind(d13-d, d13+d), rbind(d23-d, d23+d))))
names(Simu.CI) <- c("lower", "upper")
row.names(Simu.CI) <- c("d12 resp1", "d12 resp2", "d13 resp1", "d13 resp2", "d23 resp1", "d23 resp2")
S1 <- as.matrix.data.frame(cov(x1))
S2 <- as.matrix.data.frame(cov(x2))
S3 <- as.matrix.data.frame(cov(x3))
Sp <- W/147
M <- 147*log(det(Sp)) - 49*(log(det(S1))+log(det(S2))+log(det(S3)))
u <- ((3/49)-(1/147))*(2*2^2+3*2-1)/(6*3*2)
C <- (1-u)*M
chi.2.3.05 <- qchisq(0.05, 12, lower.tail = F)
C > chi.2.3.05
### 6.32. ###
data32 <- read.csv("data.csv", header = T, sep = ",")
attach(data32)
manova.32 <- manova(cbind(X560CM, X720CM) ~ Species + Nutrient)
summary(manova.32, test = "Wilks")
anova.32.1 <- aov(X560CM ~ Species + Nutrient)
summary(anova.32.1)
anova.32.2 <- aov(X720CM ~ Species + Nutrient)
summary(anova.32.2)
detach(data32)
### 6.33. ###
data33 <- read.csv("6.33.csv")
data33$Time <- as.factor(data33$Time)
attach(data33)
#a
manova.33 <- manova(cbind(X560nm, X720nm) ~ Species + Time + Species:Time)
summary(manova.33, test = "Wilks")
#b
res.33 <- data.frame(manova.33$residuals)
hist(res.33$X560nm, main = "Histogram Residuals of 560 nm", xlab = "", ylab = "")
lillie.test(res.33$X560nm)
hist(res.33$X720nm, main = "Histogram Residuals of 720 nm", xlab = "", ylab = "")
lillie.test(res.33$X720nm)
#c
summary(aov(X560nm ~ Species*Time))
summary(aov(X720nm ~ Species*Time))
detach(data33)
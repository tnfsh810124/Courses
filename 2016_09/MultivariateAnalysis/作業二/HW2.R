################################################
## Multivariate analysis HW 2 ##################
################################################

### 4.26 ###

# (a) #
x1 <- c(1, 2, 3, 3, 4, 5, 6, 8, 9, 11)
x2 <- c(18.95, 19.00, 17.95, 15.54, 14.00, 
        12.95, 8.94, 7.49, 6.00, 3.99)

df <- data.frame(cbind(x1, x2))
S <- cov(df)
x1.bar <- mean(x1)
x2.bar <- mean(x2)
A <- t(cbind((x1 - x1.bar), (x2 - x2.bar)))
D.sq <- t(A)%*%solve(S)%*%A
dii <- diag(D.sq)


# (b) #
chisq.2.0.5 <- dii[dii <= qchisq(0.5, 2)]
length(chisq.2.0.5)/length(dii)


# (c) #
order.d <- dii[order(dii)]
chi.10 <- sapply(1:10, 
                 function(j) 
                   qchisq((10-j+0.5)/10 ,2, lower.tail = F))
plot(order.d ~ chi.10, xlab = "Chi-square Quantile",
     ylab = "Ordered Square Distance", main = "Chi-square Plot")

# (d) #
# OK SOSO YES


### 4.30. ###
install.packages("MASS")
library(MASS)
library(car)

# (a) #
lambda1 <- powerTransform(x1)
l1 <- lambda1$lambda
x1.n <- (x1^l1-1)/l1
qqnorm(x1.n)
qqline(x1.n, col="red")
# (b) #
lambda2 <- powerTransform(x2)
l2 <- lambda2$lambda
x2.n <- (x2^l2-1)/l2
qqnorm(x2.n)
qqline(x2.n, col="red")


# (c) #
lambda12 <- powerTransform(cbind(x1,x2))
l1.c <- lambda12$lambda[1]
l2.c <- lambda12$lambda[2]
x1.c <- (x1^l1.c-1)/l1.c
x2.c <- (x1^l2.c-1)/l2.c


### 4.39 ###
# (a) #
#install.packages("nortest")
library(nortest)
df <- read.csv("4.39.csv", header = F, sep = ",")
names(df) <- c("Indep", "Supp", "Benev", "Conform", 
               "Leader", "Gender", "Socio")
df.ks.p <- sapply(1:5, FUN = function(x) lillie.test(df[,x])$p.value)
df.sw.p <- sapply(1:5, FUN = function(x) shapiro.test(df[,x])$p.value)



# (b) #
df <- df[, 1:5]
S <- cov(df)
A <- t(apply(df, MARGIN = 2, FUN = function(x) x-mean(x)))
D.sq <- t(A)%*%solve(S)%*%A
dii <- diag(D.sq)
order.d <- dii[order(dii)]
chi.130 <- sapply(1:130, 
                 FUN = function(j) 
                   qchisq((length(dii)-j+0.5)/length(dii) ,2, lower.tail = F))
plot(order.d~chi.130, xlab = "Chi-square Quantile",
     ylab = "Ordered Square Distance", main = "Chi-square Plot")

# (c) #
library(car)
sapply(1:5, function(x) powerTransform(df[,x])$lambda)


df.lambda <- sapply(1:5, function(x) powerTransform(df[, x])$lambda)
df.n <- 
apply(df, MARGIN = 2, 
      FUN = function(x) {
        lam <- powerTransform(x)$lambda;
        (x^lam-1)/lam})


df.ks.p.n <- sapply(1:5, function(x) lillie.test(df.n[, x])$p.value)

df.norm <- t(cbind(df.ks.p, df.lambda, df.ks.p.n))
colnames(df.norm) <- names(df)

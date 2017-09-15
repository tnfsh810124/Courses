install.packages("car")
install.packages("Hotelling")
install.packages("DescTools")
install.packages("outliers")
install.packages("gridExtra")
install.packages("IQCC")
install.packages("qcc")
install.packages("ggplot2")
install.packages("ellipse")
install.packages("MSQC")
library(MSQC)
library(qcc)
library(IQCC)
library(outliers)
library(DescTools)
library(Hotelling)
library(car)
library(base)
library(ggplot2)
library(foreign)
library(gridExtra)
library(ellipse)

### 5.20. ###
Bird <- read.table("BirdData.txt")
attach(Bird)

# (a) #
S <- cov(Bird)
mean.b <- sapply(1:2, function(x) mean(Bird[,x]))
conf.elps1 <- ellipse(S/45, centre = mean.b, level = 0.95)
plot(conf.elps1, type = "l", xlab = "tail", ylab = "wing", main = "ellipse 1")

ch.sq=2*(45-1)/(45-2)*qf(.95, 2, 45-2)
conf.elps2=ellipse(S/45, centre=mean.b, t=sqrt(ch.sq))
plot(conf.elps2, type = "l", xlab = "tail", ylab = "wing", main = "ellipse 2")

HotellingsT2Test(t(t(Bird)- c(190, 275)))

# (b) #

# simutanious
d1 <- sqrt(((45-1)*2/(45-2))*qf(0.95,2,45-2))*sqrt(S[1,1]/45)
d2 <- sqrt(((45-1)*2/(45-2))*qf(0.95,2,45-2))*sqrt(S[2,2]/45)
mu1L <- mean.b[1] - d1
mu1U <- mean.b[1] + d1
mu2L <- mean.b[2] - d2
mu2U <- mean.b[2] + d2
mu1.CI <- c(mu1L, mu1U)
mu2.CI <- c(mu2L, mu2U)

# Bonferroni
b1 <- qt(0.05/(2*2),45-1,lower.tail=F)*sqrt(S[1,1]/45)
b2 <- qt(0.05/(2*2),45-1,lower.tail=F)*sqrt(S[2,2]/45)
mu1LB=mean.b[1] - b1
mu1UB=mean.b[1] + b1
mu2LB=mean.b[2] - b2
mu2UB=mean.b[2] + b2
mu1.CI.B <- c(mu1LB, mu1UB)
mu2.CI.B <- c(mu2LB, mu2UB)

CI.20 <- data.frame( mu1.CI.B, mu2.CI.B, mu1.CI, mu2.CI)
names(CI.20) <- c("Bonf of Tail", "Bonf of Wing", "T-sq of Tail", "T-sq of Wing")
row.names(CI.20) <- c("lower", "upper")
CI.20 <- t(CI.20)
CI.20
# (c) #
A <- (t(Bird) - mean.b)
D.sq <- t(A) %*% solve(S) %*% A
dii <- diag(D.sq)
order.d <- dii[order(dii)]
chi.45 <- sapply(1:45, 
                 function(j) 
                   qchisq((45-j+0.5)/45 ,2, lower.tail = F))

plot(order.d ~ chi.45, xlab = "Chi-square Quantile",
     ylab = "Ordered Square Distance", main = "Chi-square Plot")



### 5.22. ###
Milk <- read.table("Milk.txt")
names(Milk) <- c("Fuel type", "fuel", "repair", "capital")
df <- Milk[1:25, 2:4]
attach(df)
# (a) #
g.f <- ggplot(df, aes(sample=fuel)) + stat_qq()
g.r <- ggplot(df, aes(sample=repair)) + stat_qq()
g.c <- ggplot(df, aes(sample=capital)) + stat_qq()

g.fr <- ggplot(df, aes(fuel, repair)) + geom_point()
g.fc <- ggplot(df, aes(fuel, capital)) + geom_point()
g.rc <- ggplot(df, aes(repair, capital)) + geom_point()

grid.arrange(g.f, g.r, g.c, g.fr, g.fc, g.rc, nrow=2, ncol=3)

rdf <-  df[-c(9,21), ]

rg.f <- ggplot(rdf, aes(sample=fuel)) + stat_qq()
rg.r <- ggplot(rdf, aes(sample=repair)) + stat_qq()
rg.c <- ggplot(rdf, aes(sample=capital)) + stat_qq()

rg.fr <- ggplot(rdf, aes(fuel, repair)) + geom_point()
rg.fc <- ggplot(rdf, aes(fuel, capital)) + geom_point()
rg.rc <- ggplot(rdf, aes(repair, capital)) + geom_point()

grid.arrange(rg.f, rg.r, rg.c, rg.fr, rg.fc, rg.rc, nrow=2, ncol=3)

# (b)
S <- cov(df) 
mean.m <- sapply(1:3, function(x) mean(df[,x]))
# simutanious
d1 <- sqrt(((25-1)*3/(25-3))*qf(0.95,3,25-3))*sqrt(S[1,1]/25)
d2 <- sqrt(((25-1)*3/(25-3))*qf(0.95,3,25-3))*sqrt(S[2,2]/25)
d3 <- sqrt(((25-1)*3/(25-3))*qf(0.95,3,25-3))*sqrt(S[3,3]/25)
mu1L <- mean.m[1] - d1
mu1U <- mean.m[1] + d1
mu2L <- mean.m[2] - d2
mu2U <- mean.m[2] + d2
mu3L <- mean.m[3] - d3
mu3U <- mean.m[3] + d3

mu1.CI <- c(mu1L, mu1U)
mu2.CI <- c(mu2L, mu2U)
mu3.CI <- c(mu3L, mu3U)

# Bonferroni
b1 <- qt(0.05/(2*3),25-1,lower.tail=F)*sqrt(S[1,1]/25)
b2 <- qt(0.05/(2*3),25-1,lower.tail=F)*sqrt(S[2,2]/25)
b3 <- qt(0.05/(2*3),25-1,lower.tail=F)*sqrt(S[3,3]/25)
mu1LB=mean.m[1] - b1
mu1UB=mean.m[1] + b1
mu2LB=mean.m[2] - b2
mu2UB=mean.m[2] + b2
mu3LB=mean.m[3] - b3
mu3UB=mean.m[3] + b3
mu1.CI.B <- c(mu1LB, mu1UB)
mu2.CI.B <- c(mu2LB, mu2UB)
mu3.CI.B <- c(mu3LB, mu3UB)

CI.22.1 <- data.frame(mu1.CI.B, mu2.CI.B, mu3.CI.B, mu1.CI, mu2.CI, mu3.CI)
names(CI.22.1) <- c("Bonf of Fuel", "Bonf of Repair", "Bonf of Capital", 
                    "T-sq of Fuel", "T-sq of Repair", "T-sq of Capital")
row.names(CI.22.1) <- c("lower", "upper")

CI.22.1 <- t(CI.22.1)


S <- cov(rdf) 
mean.m <- sapply(1:3, function(x) mean(rdf[,x]))
# simutanious
d1 <- sqrt(((25-1)*3/(25-3))*qf(0.95,3,25-3))*sqrt(S[1,1]/25)
d2 <- sqrt(((25-1)*3/(25-3))*qf(0.95,3,25-3))*sqrt(S[2,2]/25)
d3 <- sqrt(((25-1)*3/(25-3))*qf(0.95,3,25-3))*sqrt(S[3,3]/25)
mu1L <- mean.m[1] - d1
mu1U <- mean.m[1] + d1
mu2L <- mean.m[2] - d2
mu2U <- mean.m[2] + d2
mu3L <- mean.m[3] - d3
mu3U <- mean.m[3] + d3

mu1.CI <- c(mu1L, mu1U)
mu2.CI <- c(mu2L, mu2U)
mu3.CI <- c(mu3L, mu3U)

# Bonferroni
b1 <- qt(0.05/(2*3),25-1,lower.tail=F)*sqrt(S[1,1]/25)
b2 <- qt(0.05/(2*3),25-1,lower.tail=F)*sqrt(S[2,2]/25)
b3 <- qt(0.05/(2*3),25-1,lower.tail=F)*sqrt(S[3,3]/25)
mu1LB=mean.m[1] - b1
mu1UB=mean.m[1] + b1
mu2LB=mean.m[2] - b2
mu2UB=mean.m[2] + b2
mu3LB=mean.m[3] - b3
mu3UB=mean.m[3] + b3
mu1.CI.B <- c(mu1LB, mu1UB)
mu2.CI.B <- c(mu2LB, mu2UB)
mu3.CI.B <- c(mu3LB, mu3UB)


CI.22.2 <- data.frame(mu1.CI.B, mu2.CI.B, mu3.CI.B, mu1.CI, mu2.CI, mu3.CI)
names(CI.22.2) <- c("Bonf of Fuel", "Bonf of Repair", "Bonf of Capital", 
                    "T-sq of Fuel", "T-sq of Repair", "T-sq of Capital")
row.names(CI.22.2) <- c("lower", "upper")
CI.22.2 <- t(CI.22.2)

CI.22.2
CI.22.1

### 5.28. ###

df <- read.table("T5-14.dat")

A <- matrix(colMeans(df[1:30,]),nrow=1)
S <- cov(df[1:30,])

mult.chart(type="t2",Xmv=t(A),s=S,alpha=0.05,car,phase =2)


### 5.29. ###

30*A%*%solve(S)%*%t(A)
(((30-1)*6)/(30-6))*qf(0.05,6,24,lower.tail = F)

30*A%*%solve(S)%*%t(A)>(((30-1)*6)/(30-6))*qf(0.05,6,24,lower.tail = F)

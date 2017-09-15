#install.packages("nortest")
library(MASS)
library(nortest)
attach(Boston)
df <- Boston

#1
summary(df)
table(chas, rad)

#2
pairs(df)

par(mfrow = c(3,5))
for(k in 1:14){
  boxplot(df[,k], main = names(df)[k])
}

#3
conti.variable.p <- data.frame(sapply(c(1,2,3,5,6,7,8,10:14), function(x) lillie.test(df[,x])$p.value))
row.names(conti.variable.p) <- names(df)[c(1,2,3,5,6,7,8,10:14)]
names(conti.variable.p) <- "p-value of the Lillie test"

#4
corr.medv <- data.frame(sapply(c(1,2,3,5,6,7,8,10:13), function(x) cor(df[,x], df$medv)))
row.names(corr.medv) <- names(df)[c(1,2,3,5,6,7,8,10:13)]
names(corr.medv) <- "correlation coefficient"

#5
cont.p <- 
sapply(c(1,2,3,5,6,7,8,10:13), FUN = function(x) {
  fit <- summary(lm(medv~df[,x]));
  fit$coefficient[2,4]
})

cont.p <- data.frame(cont.p)
names(cont.p) <- "p.value of the continuous predictor"
row.names(cont.p) <- names(df)[c(1,2,3,5,6,7,8,10:13)]

sapply(c(4,9), function(x) {
  fit <- summary(lm(medv ~ factor(df[,x])));
  fit$coefficients
})


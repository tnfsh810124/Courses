pr <- function(n) 1-(1-(1/n))^n
x <- 1:100000
pr(x)
plot(log(x), pr(x), 
     xlab = "log(n)", ylab = "Pr(in)", main = "trend of the probability in n samples", 
     type = "l", col = "darkviolet")
abline(h = pr(100000), col = "red")

store <- rep(NA, 10000)
for(i in 1:10000){
  store[i]=sum(sample(1:100, rep=TRUE)==4)>0 }
mean(store)

# 5.a #
library(ISLR)
fit5a <- glm(default ~ income + balance, data = Default, family = binomial)
summary(fit5a)

# 5.b #
# i
set.seed(3)
tr5b <- sample(dim(Default)[1], round(0.5*dim(Default)[1]))
# ii
fit5b <- glm(default ~ income + balance, data = Default, family = "binomial", subset = tr5b)
summary(fit5b)
# iii
prob5b <- predict(fit5b, newdata = Default[-tr5b, ], type = "response")
pred5b <- as.factor(ifelse(prob5b>0.5, "Yes", "No"))
# iv 
te.err.5b <- mean(pred5b != Default[-tr5b, ]$default)
te.err.5b

# 5.c #
te.err.5c <- 
  sapply(1:3, function(k) {
    set.seed(k+3);
    tr5c <- sample(dim(Default)[1], round(0.5*dim(Default)[1]));
    fit5c <- glm(default ~ income + balance, data = Default, family = "binomial", subset = tr5c);
    prob5c <- predict(fit5c, newdata = Default[-tr5c, ], type = "response");
    pred5c <- as.factor(ifelse(prob5c>0.5, "Yes", "No"));
    mean(pred5c != Default[-tr5c, ]$default)
  })

# 5.d #
set.seed(11)
tr5d <- sample(dim(Default)[1], round(0.5*dim(Default)[1]))
fit5d <- glm(default ~ income + balance + student, data = Default, family = "binomial", subset = tr5d)
prob5d <- predict(fit5d, newdata = Default[-tr5d, ], type = "response")
pred5d <- as.factor(ifelse(prob5d > 0.5, "Yes", "No"))
te.err.5d <- mean(pred5d != Default[-tr5d, ]$default)
te.err.5d
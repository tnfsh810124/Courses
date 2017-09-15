# 7.3. #
x <- -2:2
y <- 1 + x + -2 * (x-1)^2 * I(x>1)
plot(x, y, main = "figure of exercise 7.3", pch = 16, col = "darkviolet")

# 7.6.a #
set.seed(1)
library(ISLR)
library(boot)
all.deltas <- rep(NA, 10)
for (i in 1:10) {
  glm.fit <- glm(wage~poly(age, i), data=Wage)
  all.deltas[i] <- cv.glm(Wage, glm.fit, K=10)$delta[2]
}
plot(1:10, all.deltas, xlab="Degree", ylab="CV error", type="l", pch=20, lwd=2, ylim=c(1590, 1700))
min.point <- min(all.deltas)
sd.points <- sd(all.deltas)
abline(h=min.point + 0.2 * sd.points, col="red", lty="dashed")
abline(h=min.point - 0.2 * sd.points, col="red", lty="dashed")
legend("topright", "0.2-standard deviation lines", lty="dashed", col="red")

fit.1  <- lm(wage~poly(age, 1),  data = Wage)
fit.2  <- lm(wage~poly(age, 2),  data = Wage)
fit.3  <- lm(wage~poly(age, 3),  data = Wage)
fit.4  <- lm(wage~poly(age, 4),  data = Wage)
fit.5  <- lm(wage~poly(age, 5),  data = Wage)
fit.6  <- lm(wage~poly(age, 6),  data = Wage)
fit.7  <- lm(wage~poly(age, 7),  data = Wage)
fit.8  <- lm(wage~poly(age, 8),  data = Wage)
fit.9  <- lm(wage~poly(age, 9),  data = Wage)
fit.10 <- lm(wage~poly(age, 10), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7, fit.8, fit.9, fit.10)

plot(wage ~ age, data = Wage, col="darkgrey")
agelims <- range(Wage$age)
deg.col <- c(1, "greenyellow", "red", "black",5, 6, 7, 8, "blue")
for(d in c(4, 9, 3, 2)){
  age.grid <- seq(from=agelims[1], to=agelims[2])
  lm.fit <- lm(wage ~ poly(age, d), data = Wage)
  lm.pred <-  predict(lm.fit, data.frame(age = age.grid))
  lines(age.grid, lm.pred, col = deg.col[d], lwd = 2)
}
legend("topright", rev(paste0("deg = ", c("4", "9", "3", "2"))), lty = 1, lwd = 2, 
       col = rev(deg.col[c(4, 9, 3, 2)]))

# 7.6.b #
all.cvs <- rep(NA, 10)
for (i in 2:10) {
  Wage$age.cut <- cut(Wage$age, i)
  lm.fit <- glm(wage ~ age.cut, data = Wage)
  all.cvs[i] <- cv.glm(Wage, lm.fit, K = 10)$delta[2]
}
plot(2:10, all.cvs[-1], xlab = "Number of cuts", ylab = "CV error", 
     type = "l", pch = 20, lwd = 2)
points(which.min(all.cvs[-1])+1, min(all.cvs[-1]), pch = 16, col = "red", cex = 1.5)

lm.fit <- glm(wage ~ cut(age, 8), data = Wage)
agelims <- range(Wage$age)
age.grid <- seq(from = agelims[1], to = agelims[2])
lm.pred <- predict(lm.fit, data.frame(age = age.grid))
plot(wage~age, data = Wage, col = "darkgrey")
lines(age.grid, lm.pred, col = "red", lwd = 2)

# 7.7 #
library(ISLR)
library(gam)
set.seed(1)
summary(Wage$maritl)
summary(Wage$jobclass)
par(mfrow = c(1, 2))
plot(Wage$maritl, Wage$wage)
plot(Wage$jobclass, Wage$wage)

fit <- lm(wage ~ maritl, data = Wage)
deviance(fit)
fit <- lm(wage ~ jobclass, data = Wage)
deviance(fit)
fit <- lm(wage ~ maritl + jobclass, data = Wage)
deviance(fit)
fit <- gam(wage ~ maritl + jobclass + s(age, 4), data = Wage)
deviance(fit)

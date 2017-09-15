#install.packages("plot3D")
#install.packages("plot3Drgl")
#install.packages("MVN")
library(nortest)
library(car)
library(MASS)
library(plot3D)
library(plot3Drgl)
library(MVN)

### 7.21 ###
df15 <- read.table("T1-5.dat")
names(df15) <- c("Wind", "radiation", "CO", "NO", "NO2", "O3", "HC")
df15 <- df15[, c(1,2,5,6)]
pairs(df15)
z1 <- df15$Wind
z2 <- df15$radiation
y1 <- df15$NO2
y2 <- df15$O3
# a #
# (i)
shapiro.test(y1)
lambda <- powerTransform(y1)$lambda
y1t <- ((y1)^lambda-1)/lambda
shapiro.test(y1t)
summary(lm(y1t ~ z1 + z2))
par(mfrow = c(1,1))
scatter3D(y1t, z1, z2, pch = 18, cex = 1, 
          theta = 20, phi =20, ticktype = "detailed",
          xlab = "z1", ylab = "z2", zlab = "y1t", clab = "yt1", 
          colkey = list(length = 1, width = .5),
          surf = , 
          main = "scatter plot")
summary(lm(y1t ~  0 + z1 + z2))
lm.21.a <- lm(y1t ~  0 + z1 + z2)
# (ii)
par(mfrow = c(1,1))
plot(lm.21.a)
# (iii)
pre.21.a <- data.frame(cbind(10, 80))
names(pre.21.a) <- c("z1", "z2")
pre.NO2.t.CI <- predict(lm.21.a, pre.21.a, interval = "prediction", level = .95)
pre.NO2.o.CI <- (1+lambda*pre.NO2.t.CI)^(1/lambda)
pre.NO2.CI   <- rbind(pre.NO2.o.CI, pre.NO2.t.CI)
row.names(pre.NO2.CI) <- c( "original y1", "transformed y1")
pre.NO2.CI

# b #
# (i)
mardiaTest(cbind(y1, y2))
lambda1 <- powerTransform(cbind(y1, y2))$lambda[1]
lambda2 <- powerTransform(cbind(y1, y2))$lambda[2]
y1t <- ((y1)^lambda1-1)/lambda1
y2t <- ((y2)^lambda2-1)/lambda2
summary(lm(cbind(y1t, y2t) ~ 0 + z1 + z2))
# (ii)
par(mfrow = c(1,1), oma = rep(0, 4), mar = rep(3, 4))
mardiaTest(lm(cbind(y1t, y2t) ~ 0 + z1 + z2)$residual, qqplot = T)
# (iii)
Z <- cbind(rep(1, 42), z1, z2)
z0 <-  c(1,10,80)
rad <- (1+t(z0)%*%solve(t(Z)%*%Z)%*%z0)*(2*(42-2-1)/(42-2-2))*qf(0.05,2,42-2-2,lower.tail=F)
rad <- as.numeric(rad)
par(mfrow = c(1,1), oma = rep(0, 4), mar = c(5,5,5,1.5))
plot(NA, xlim = c(-3, 3), ylim = c(-7, 7), asp = 1, 
     xlab = "y1 with lambda = -0.1641231 ", ylab = "y2 with lambda = 0.2384723 ", 
     main = "95% prediction interval for NO2")
elps21 <- ellipse(center = colMeans(lm(cbind(y1t, y2t) ~ 0 + z1 + z2)$residuals),
                  shape  = cov(lm(cbind(y1t, y2t) ~ 0 + z1 + z2)$residuals),
                  radius = rad, draw = T, col = gray(0.7))
abline(v = pre.NO2.t.CI[2:3], col = "red")

### 7.25 ###
df76 <- read.table("T7-6.DAT")
names(df76) <- c("y1", "y2", paste0("z", 1:5))
y1 <- df76$y1
y2 <- df76$y2
z1 <- df76$z1
z2 <- df76$z2
z3 <- df76$z3
z4 <- df76$z4
z5 <- df76$z5
# a #
# (i)
shapiro.test(y1)
lambda <- powerTransform(y1)$lambda
y1t <- (y1^lambda-1)/lambda
summary(lm(y1t ~ z1 + z2 + z3 + z4 + z5))
summary(lm(y1t ~ z1 + z2 + z3 + z4 + z5 + 0))
summary(lm(y1t ~ z3 + z4 + 0))
lm.25.a <- lm(y1t ~ z3 + z4 + 0)
# (ii)
par(mfrow = c(2,2))
plot(lm.25.a)
# (iii)
pre.25.a <- as.data.frame(cbind(1, 1200, 140, 70, 85))
names(pre.25.a) <- paste0("z", 1:5)
pre.25a.t.CI <- predict(lm.25.a, pre.25.a, interval = "prediction")
pre.25a.o.CI <- (1 + lambda*pre.25a.t.CI)^(1/lambda)
pre.25a.CI <- rbind(pre.25a.o.CI, pre.25a.t.CI)
row.names(pre.25a.CI) <- c( "original y1", "transformed y1")
pre.25a.CI

# b #
# (i)
shapiro.test(y2)
lambda <- powerTransform(y2)$lambda
y2t <- (y2^lambda-1)/lambda
summary(lm(y2t ~ z1 + z2 + z3 + z4 + z5))
summary(lm(y2t ~ z1 + z2 + z3 + z4 + z5 + 0))
summary(lm(y2t ~ z3 + z4 + 0))
lm.25.b <- lm(y2t ~ z3 + z4 + 0)
# (ii)
par(mfrow = c(2,2))
plot(lm.25.b)
# (iii)
pre.25.b <- as.data.frame(cbind(1, 1200, 140, 70, 85))
names(pre.25.b) <- paste0("z", 1:5)
pre.25b.t.CI <- predict(lm.25.b, pre.25.b, interval = "prediction")
pre.25b.o.CI <- (1 + lambda*pre.25b.t.CI)^(1/lambda)
pre.25b.CI <- rbind(pre.25b.o.CI, pre.25b.t.CI)
row.names(pre.25b.CI) <- c( "original y1", "transformed y1")
pre.25b.CI

# c #
# (i)
mardiaTest(cbind(y1, y2))
lambda1 <- powerTransform(cbind(y1, y2))$lambda[1]
lambda2 <- powerTransform(cbind(y1, y2))$lambda[2]
y1t <- ((y1)^lambda1-1)/lambda1
y2t <- ((y2)^lambda2-1)/lambda2
summary(lm(cbind(y1t, y2t) ~ z1 + z2 + z3 + z4 + z5))
summary(lm(cbind(y1t, y2t) ~ z1 + z2 + z3 + z4 + z5 + 0))
summary(lm(cbind(y1t, y2t) ~ z1 + z3 + z4 + 0))
lm.25.c <- lm(cbind(y1t, y2t) ~ z1 + z3 + z4 + 0)
# (ii)
par(mfrow = c(1,1), oma = rep(0, 4), mar = rep(3, 4))
mardiaTest(lm.25.c$residual, qqplot = T)

# (iii)
Z <- cbind(rep(1, 17), z1, z3, z4)
z0 <-  c(1, 1, 140, 70)
rad <- (1+t(z0)%*%solve(t(Z)%*%Z)%*%z0)*(2*(17-3-1)/(17-3-2))*qf(0.05,2,17-3-2,lower.tail=F)
rad <- as.numeric(rad)
par(mfrow = c(1,1), oma = rep(0, 4), mar = c(5,5,5,1.5))
plot(NA, xlim = c(-3, 3), ylim = c(-15, 15), asp = 1, 
     xlab = "y1 with lambda = -0.1772955  ", ylab = "y2 with lambda = -0.1852451 ", 
     main = "95% prediction interval for NO2")
elps21 <- ellipse(center = colMeans(lm(cbind(y1t, y2t) ~ 0 + z1 + z2)$residuals),
                  shape  = cov(lm(cbind(y1t, y2t) ~ 0 + z1 + z2)$residuals),
                  radius = rad, draw = T, col = gray(0.7))
abline(v = pre.25a.t.CI[2:3], col = "orange")
abline(h = pre.25b.t.CI[2:3], col = "red")

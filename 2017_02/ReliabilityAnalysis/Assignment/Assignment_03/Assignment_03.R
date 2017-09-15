library(boot)  # for logit transformation and its inverse

########## 3.4 

## (a)
t_i  <- c(0, 33, 46, 50, 59, 62, 71, 74, 75, 78)
d_i  <- c(0,  1,  1,  1,  1,  1,  1,  1,  1,  2)
n_i  <- c(130, sapply(1:10, function(j) 130 - sum(d_i[1:j]))[-10])
F_i  <- cumsum(d_i)/130
S_i  <- 1 - F_i
pi_i <- d_i/130
p_i  <- c(0, sapply(1:9, FUN = function(j) (pi_i[j+1])/S_i[j]))
# VF_i <- sapply(1:10, function(i, x_i = p_i/(n_i * (1 - p_i))) (S_i[i])^2*sum(x_i[1:i]) )
VF_i <-  F_i*(1 - F_i)/130
SE_F <- sqrt(VF_i)
# as.data.frame(round(cbind(t_i, n_i, d_i, pi_i, p_i, F_i, S_i, SE_F), 4)[-1, ])
as.data.frame(round(cbind(t_i, F_i, SE_F), 4)[-1, ])

## (b)
Flg <-  logit(F_i)
SE_Flg <- SE_F/(F_i*(1 - F_i))
z_val <- qnorm(.95)

F_CI_L <- F_i - z_val * SE_F
F_CI_U <- F_i + z_val * SE_F
Flg_CI_L <- inv.logit(Flg - z_val * SE_Flg)
Flg_CI_U <- inv.logit(Flg + z_val * SE_Flg)
FF <- as.data.frame(round(cbind(t_i, F_i, F_CI_L, F_CI_U, Flg_CI_L, Flg_CI_U), 4)[-1,])

pdf("3_4_b.pdf", width = 9, height = 5)
par(mfrow = c(1, 2))
plot(F_i[-1], ylim = c(min(FF[, 3:6], na.rm = T), max(FF[, 3:6], na.rm = T)), 
     pch = 16, xlab = "time", xaxt = "n", ylab = "F(t)", 
     main = "Confidence Band of F")
axis(1, at = 1:9, labels = t_i[-1])
points(F_CI_U[2:10],  pch = 25)
points(F_CI_L[2:10],  pch = 24)

plot(F_i[-1], ylim = c(min(FF[, 3:6], na.rm = T), max(FF[, 3:6], na.rm = T)), 
     pch = 16, xlab = "time", xaxt = "n", ylab = "F(t)", 
     main = "Confidence Band of Logit F")
axis(1, at = 1:9, labels = t_i[-1])
points(Flg_CI_U[2:10],  pch = 25)
points(Flg_CI_L[2:10],  pch = 24)
par(mfrow = c(1, 1))
dev.off()


## (c)
S_i_6 <- cumprod(1 - p_i)
F_i_7 <- 1 - S_i_6
cbind(t_i, F_i, F_i_7)


as.data.frame(round(cbind(t_i, F_i, F_i_7, SE_F, F_CI_L, F_CI_U, Flg_CI_L, Flg_CI_U), 4)[-1,])


########## 3.10
set.seed(6666666)
Ui <- sapply(1:50, function(j) runif(200, 0, 1))
Ti <- -log(1 - Ui)

## (a)
sapply(1:50, function(j) ecdf(Ti[, j]))

## (b)
Fhat1 <- sapply(1:50, function(j) sum(Ti[ ,j] < 1)/200 )


pdf("3_10_ab.pdf", width = 10, height = 5)
par(mfrow = c(1, 2))
plot(1:200 ~ sort(Ti[, 1]), type = "l", col = gray(.8), 
     yaxt = "n", xlim = c(-1, max(Ti)+1), 
     xlab = "time", ylab = "F(t)", main = "The ECDFs' Curves and Their theoretical Curve")
sapply(2:50, function(j) points(1:200 ~ sort(Ti[, j]), type = "l", col = gray(.8)))
points(seq(0, 10, 0.05), 200*(1-exp(-seq(0, 10, 0.05))), type = "l", 
       col = "red", lwd = 1.5)
axis(2, at = seq(0, 200, 50), labels = seq(0, 1, .25))
legend("bottomright", legend = c("ecdfs' curve", "theoretical curve")
       , lty = 1, lwd = 1.5, col = c(grey(.8), "red"))

hist(Fhat1, breaks = 10, main = "The Estimates of F(1)", xlab = "F(1)")
par(mfrow = c(1, 1))
dev.off()

## (c)
F1 <- 1 - exp(-1)
SEhat <- sqrt(Fhat1*(1-Fhat1)/200)
ZF <- (Fhat1 - F1)/SEhat

## (d)
Fhlog1 <- logit(Fhat1)
Flog1  <- logit(F1)
SElog <- SEhat/(Fhat1*(1-Fhat1))
Zlog <- (Fhlog1 - Flog1)/SElog

pdf("3_10_cd.pdf", width = 10, height = 5)
par(mfrow = c(1, 2))
hist(ZF, breaks = 10, main = "The Estimates of Z-scores of F(1)", xlab = "")
hist(Zlog, breaks = 10, main = "The Estimates of Logit Z-scores of F(1)", xlab = "")
par(mfrow = c(1, 1))
dev.off()


# group excercise ###################################################################

########## 3.11
Ui <- sapply(1:50, function(j) runif(20, 0, 1))
Ti <- -log(1 - Ui)
Fhat1 <- sapply(1:50, function(j) sum(Ti[ ,j] < 1)/20 )

pdf("3_11_1.pdf", width = 10, height = 5)
par(mfrow = c(1, 2))
plot(1:20 ~ sort(Ti[, 1]), type = "l", col = gray(.8), 
     yaxt = "n", xlim = c(-1, max(Ti)+1), 
     xlab = "time", ylab = "F(t)", main = "The ECDFs' Curves and Their theoretical Curve")
sapply(2:50, function(j) points(1:20 ~ sort(Ti[, j]), type = "l", col = gray(.8)))
points(seq(0, 10, 0.05), 20*(1-exp(-seq(0, 10, 0.05))), type = "l", 
       col = "red", lwd = 1.5)
axis(2, at = seq(0, 20, 5), labels = seq(0, 1, .25))
legend("bottomright", legend = c("ecdfs' curve", "theoretical curve")
       , lty = 1, lwd = 1.5, col = c(grey(.8), "red"))

hist(Fhat1, breaks = 10, main = "The Estimates of F(1)", xlab = "F(1)")
par(mfrow = c(1, 1))
dev.off()

F1 <- 1 - exp(-1)
SEhat <- sqrt(Fhat1*(1-Fhat1)/20)
ZF <- (Fhat1 - F1)/SEhat

pdf("3_11_2.pdf", width = 10, height = 5)
par(mfrow = c(1, 2))
hist(ZF, breaks = 10, main = "The Estimates of Z-scores of F(1)", xlab = "")
hist(Zlog, breaks = 10, main = "The Estimates of Logit Z-scores of F(1)", xlab = "")
par(mfrow = c(1, 1))
dev.off()


########## 3.12
ti <- c(2500, 3000, 3500, 3600, 3700, 3800)
di <- c(   1,    1,    2,    1,    1,    1)
ni <- c(28, 28 - cumsum(di))[-7]
## (b)
Fi <- cumsum(di)/28
## (c)
SE <-  sqrt(Fi*(1 - Fi)/28)


## (d)
Fl <-  logit(Fi)
SE_Fl <- SE/(Fi*(1 - Fi))
zval <- qnorm(.975)
Fl_CI_L <- inv.logit(Fl - zval * SE_Fl)
Fl_CI_U <- inv.logit(Fl + zval * SE_Fl)

## (e)
sg <- 28 * cumsum(di/(ni*(ni-di)))
Ki <- sg/(1+sg)
ez <- 3.21
wi <- exp(ez*SE/(Fi*(1 - Fi)))

Fs_CI_L <- Fi/(Fi + (1 - Fi)*wi)
Fs_CI_U <- Fi/(Fi + (1 - Fi)/wi)

as.data.frame(round(cbind(ti, Fi, SE, Fl_CI_L, Fl_CI_U, Fs_CI_L, Fs_CI_U), 4))


pdf("3_12_de.pdf", width = 9, height = 5)
par(mfrow = c(1, 2))
plot(Fi, ylim = c(0, 0.6), 
     pch = 16, xlab = "time", xaxt = "n", ylab = "F(t)", 
     main = "Pointwise Confidence Band of F")
axis(1, at = 1:6, labels = ti)
points(Fl_CI_U,  pch = 25)
points(Fl_CI_L,  pch = 24)

plot(Fi, ylim = c(0, 0.6), 
     pch = 16, xlab = "time", xaxt = "n", ylab = "F(t)", 
     main = "Simultaneous Confidence Band of F")
axis(1, at = 1:6, labels = ti)
points(Fs_CI_U,  pch = 25)
points(Fs_CI_L,  pch = 24)
par(mfrow = c(1, 1))
dev.off()


########## 3.17
ti <- 1:3
di <- c(4, 5, 2)
ni <- c(300, 197, 97)
pi <- di/ni
Si <- cumprod(1-pi)
Fi <- 1-Si
Vr <- Si^2*cumsum(pi/(ni*(1-pi)))
se <- sqrt(Vr)
zval <- qnorm(.975)
wi <- exp(zval*se/(Fi*(1 - Fi)))
CI_L <- Fi/(Fi + Si*wi)
CI_U <- Fi/(Fi + Si/wi)
as.data.frame(round(cbind(ti, Fi, se, CI_L, CI_U), 4))


########## 3.19
ti <- seq(25, 100, 25)
di <- c(109, 42, 17, 7)
ni <- c(sum(di)+13, sum(di)+13 - cumsum(di)[-4])
pi <- di/ni
Si <- cumprod(1-pi)
Fi <- 1-Si
Vr <- Si^2*cumsum(pi/(ni*(1-pi)))
se <- sqrt(Vr)
zval <- qnorm(.975)
wi <- exp(zval*se/(Fi*(1 - Fi)))
CI_L <- Fi/(Fi + Si*wi)
CI_U <- Fi/(Fi + Si/wi)
Pi <- c(di, 13)/188
as.data.frame(round(cbind(ti, pi, Fi, CI_L, CI_U), 4))

Pi <- c(di, 13)/188

pdf("3_19.pdf", width = 9, height = 5)
par(mfrow = c(1, 2))
plot(Fi, ylim = c(0, 1), 
     pch = 16, xlab = "time", xaxt = "n", ylab = "F(t)", 
     main = "Pointwise Confidence Band of F")
axis(1, at = 1:4, labels = ti)
points(CI_U,  pch = 25)
points(CI_L,  pch = 24)
points(Fi)

plot(Pi, pch = 1, ylim = c(0, 1), xaxt = "n", 
     ylab = "Pi(t)", xlab = "time", main = "Probabilities of Failures in Time Intervals")
sapply(1:4, function(i) segments(x0 = i, y0 = Pi[i], x1 = i+1, y1 = Pi[i]))
points(2:5, di/188, pch = 16)
points(Pi, pch = 16, col = "white")
points(Pi)
axis(1, at = 1:5, labels = c(0, ti))
text(x = c(1:4)+.15, y = Pi[1:4], labels = round(Pi[1:4], 2), pos = 1)
text(x = 5-.15, y = Pi[5], labels = round(Pi[5], 2), pos = 3)
par(mfrow = c(1, 1))
dev.off()

########## 3.21
ti <- c( 50, 100, 150, 200, 250, 300, 350, 400, 
        500, 550, 600, 650, 700, 750, 850, 900, 
       1000,1050,1100,1150,1200,1350,1550,1700)
di <- c(  1,   0,   1,   4,   1,   1,   1,   4, 
          4,   2,   2,   1,   2,   1,   3,   0,
          0,   1,   0,   0,   1,   1,   1,   1)
ri <- c(  5,   6,   1,   6,   2,   1,   2,   2,
          3,   1,   0,   0,   1,   0,   0,   1,
          1,   0,   1,   2,   0,   0,   0,   0)
ni <- c(68, 68 - cumsum(di + ri)[-24])
ar <- ni - 0.5*ri
pi <- di/ar
Si <- cumprod(1 - pi)
Fi <- 1 - Si
Vr <- Si^2*cumsum(pi/(ar*(1-pi)))
se <- sqrt(Vr)
zval <- qnorm(.95)
wi <- exp(zval*se/(Fi*(1 - Fi)))
CI_L <- Fi/(Fi + Si*wi)
CI_U <- Fi/(Fi + Si/wi)
as.data.frame(round(cbind(ti, Fi, se, CI_L, CI_U), 4))

pdf("3_21.pdf", width = 9, height = 5)

plot(Fi, ylim = c(0, 1), 
     pch = 16, xlab = "time", xaxt = "n", ylab = "F(t)", 
     main = "Pointwise Confidence Band of Logit F")
axis(1, at = 1:24, labels = ti)
points(CI_U,  pch = 25)
points(CI_L,  pch = 24)
dev.off()

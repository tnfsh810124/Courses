
# 2.3 (c) ############################################################
Fcn_3 <- function(t) t/2
fcn_3 <- function(t) (1*t)/(2*t)
hzd_3 <- function(t) 1/(2-t)

## plot
pdf("2_3_c.pdf",  width = 6.66, height = 4)
par(mfrow = c(1, 2))
# CDF
curve(Fcn_3(x), lty = 1, from = 0, to =2, lwd = 1.3, 
      main = "CDF", xlab = "t", ylab = "", yaxt = "n")
axis(side = 2, at = c(seq(0, 1, .1)))

curve(fcn_3(x), lty = 1, from = 0, to =2, lwd = 1.3, 
      main = "PDF", xlab = "t", ylab = "", yaxt = "n")
axis(side = 2, at = c(seq(0, 1, .1)))
dev.off()


# 2.3 (d) ############################################################
pdf("2_3_d.pdf",  width = 4, height = 4)
curve(hzd_3(x), lty = 1, from = 0, to =2, lwd = 1.3, 
      main = "Harzard Function", xlab = "t", ylab = "", yaxt = "n")
axis(side = 2, at = c(seq(0, 50, 5)))
abline(v = 2, lty = 2, col = gray(.5))
dev.off()

# 2.4 (c) ############################################################


# CDF
Fcn_4 <- function(t, beta) 1 - exp(-(t/1)^(beta))
pdf("2_4_c.pdf",  width = 10, height = 4)
# pdf("2_4_c_1.pdf",  width = 6, height = 4)
par(mfrow = c(1,3))

curve(Fcn_4(x, .5), lty = 1, from = 0, to =10, lwd = 1.3, 
      main = "CDF", xlab = "t", ylab = "", yaxt = "n")
par(new = T)
curve(Fcn_4(x, 1) , lty = 2, from = 0, to =10, lwd = 1.3, 
      xlab = "t", ylab = "", yaxt = "n")
par(new = T)
curve(Fcn_4(x, 2) , lty = 3, from = 0, to =10, lwd = 1.3, 
      xlab = "t", ylab = "", yaxt = "n")
axis(side = 2, at = c(seq(0, 1, .1)))
legend("bottomright", bty = "n", 
       legend = c("beta = 0.5", "beta = 1", "beta = 2"), 
       lty = 1:3)
#dev.off()

# PDF
fcn_4 <- function(t, beta) (beta*(t/1)^(beta - 1))*exp(-(t/1)^beta)
#pdf("2_4_c_2.pdf",  width = 6, height = 4)

curve(fcn_4(x, .5), lty = 1, from = 0, to =10, lwd = 1.3, 
      main = "PDF", xlab = "t", ylab = "", yaxt = "n")
par(new = T)
curve(fcn_4(x, 1) , lty = 2, from = 0, to =10, lwd = 1.3, 
      xlab = "t", ylab = "", yaxt = "n")
par(new = T)
curve(fcn_4(x, 2) , lty = 3, from = 0, to =10, lwd = 1.3, 
      xlab = "t", ylab = "", yaxt = "n")
axis(side = 2, at = c(seq(0, 1, .1)))
legend("topright", bty = "n", 
       legend = c("beta = 0.5", "beta = 1", "beta = 2"), 
       lty = 1:3)
#dev.off()

# hazard function
hzd_4 <- function(t, beta) beta*t^(beta - 1)
#pdf("2_4_c_3.pdf",  width = 6, height = 4)

curve(hzd_4(x, .5), lty = 1, from = 0, to =10, lwd = 1.3, 
      main = "Harzard Function", xlab = "t", ylab = "", yaxt = "n")
par(new = T)
curve(hzd_4(x, 1) , lty = 2, from = 0, to =10, lwd = 1.3, 
      xlab = "t", ylab = "", yaxt = "n")
par(new = T)
curve(hzd_4(x, 2) , lty = 3, from = 0, to =10, lwd = 1.3, 
      xlab = "t", ylab = "", yaxt = "n")
axis(side = 2, at = c(seq(0, 40, 10)))
legend("bottomright", bty = "n", 
       legend = c("beta = 0.5", "beta = 1", "beta = 2"), 
       lty = 1:3)
#dev.off()
par(mfrow = c(1,1))

dev.off()

# 2.5 (b) ############################################################

pdf("2_5_b.pdf",  width = 10, height = 4)
par(mfrow = c(1,3))
# CDF
Fcn_5 <- function(t) 1 - exp(-t)
#pdf("2_5_b_1.pdf",  width = 6, height = 4)
curve(Fcn_5(x), from = 0, to = 10, lwd = 1.3, 
      main = "CDF", xlab = "t", ylab = "", yaxt = "n")
axis(side = 2, at = c(seq(0, 1, .1)))
#dev.off()

# PDF
fcn_5 <- function(t) exp(-t)
#pdf("2_5_b_2.pdf",  width = 6, height = 4)
curve(fcn_5(x), from = 0, to = 10, lwd = 1.3, 
      main = "PDF", xlab = "t", ylab = "", yaxt = "n")
axis(side = 2, at = c(seq(0, 1, .1)))
#dev.off()

# hazard function
hzd_5 <- function(t) exp(-t)/exp(-t)
#pdf("2_5_b_3.pdf",  width = 6, height = 4)
curve(hzd_5(x), from = 0, to = 10, lwd = 1.3, 
      main = "Hazard Function", xlab = "t", ylab = "", yaxt = "n")
axis(side = 2, at = c(seq(0, 2, .1)))
#dev.off()
par(mfrow = c(1,1))
dev.off()


# 2.5 (c) ############################################################
pdf("2_5_c.pdf",  width = 6.66, height = 4)
par(mfrow = c(1,2))

# CDF
# pdf("2_5_c_1.pdf",  width = 6, height = 4)
curve(Fcn_5(x), from = 0, to = 1, lwd = 1.3, 
      main = "CDF", xlab = "t", ylab = "", yaxt = "n")
axis(side = 2, at = c(seq(0, 1, .1)))
segments(       0, 0.1, x1 = -log(.9), y1 = .1, lty = 2)
segments(-log(.9), 0  , x1 = -log(.9), y1 = .1, lty = 2)
points(-log(.9), 0.1, pch = 16, cex = .8)
text(-log(.9), 0.1, labels = "(-log(0.9), 0.1)", pos = 4)
#dev.off()

# PDF
#auc <- curve(fcn_5(x), xlim = c(0, -log(.9)))
#pdf("2_5_c_2.pdf",  width = 6, height = 4)
curve(fcn_5(x), from = 0, to = 1, lwd = 1.3, 
      main = "PDF", xlab = "t", ylab = "", yaxt = "n")
par(new = T)
polygon(c(0, auc$x, -log(.9)), c(0, auc$y, 0), col = gray(.9))
#dev.off()

par(mfrow = c(1,1))
dev.off()

# 2.5 (d) ############################################################
auc <- curve(fcn_5(x), xlim = c(0.1, 0.2))
pdf("2_5_d.pdf",  width = 4, height = 4)
curve(fcn_5(x), from = 0, to = 1, lwd = 1.3, 
      main = "PDF", xlab = "t", ylab = "", yaxt = "n")
polygon(c(0.1, auc$x, 0.2), c(0, auc$y, 0), col = gray(.9))
dev.off()

# 2.6 (a) ############################################################
Fcn_6 <- function(t) 1 - exp(-t)


pdf("2_6_a.pdf",  width = 4, height = 4)
curve(Fcn_6(x), from = 0, to = 20, lwd = 1.3, 
      main = "CDF", xlab = "t", ylab = "", yaxt = "n")
axis(side = 2, at = c(seq(0, 1, .1)))
segments( 0, Fcn_6(10), x1 = 10, y1 = Fcn_6(10), lty = 2)
segments(10,         0, x1 = 10, y1 = Fcn_6(10), lty = 2)
points(10, Fcn_6(10), pch = 16, cex = .8)
text(10, Fcn_6(10), labels = "(10, 0.9999546)", pos = 1)
dev.off()

# 2.6 (b) ############################################################
Fcn_6 <- function(t) 1 - exp(-t)
fcn_6 <- function(t) exp(-t)
Suv_6 <- function(t) exp(-t)
ts <- c(0, .1, .2, .5, 1, 2, Inf)

Fcn_6(ts[2:7])
Suv_6(ts[2:7])
Pi_6 <- Fcn_6(ts[2:7]) - Fcn_6(ts[1:6])
pi_6 <- Pi_6/Suv_6(ts[1:6])

table_6 <- as.data.frame(cbind(Fcn_6(ts[2:7]), Suv_6(ts[2:7]), Pi_6, pi_6))
names(table_6)[1:2] <- c("Fcn_6", "Suv_6")
round(table_6, 4)


# 2.7
75e-9*20*1500*8760*2


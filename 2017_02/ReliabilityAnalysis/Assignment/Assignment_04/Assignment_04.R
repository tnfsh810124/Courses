##### 4.15
## (a) proof
## (b)
bet <- c(.5, 1, 3, 5)
eta <- c(50, 100)
sapply(bet, function(bet) {
  sapply(eta, function(eta) round(1/(bet*log(eta)), 3))
})

f_weib <- 
  function(t, bet, eta) {
    (bet/eta)*(t/eta)^(bet-1)*exp(-(t/eta)^bet)
  }

pdf("f_4_15_b.pdf")
curve(f_weib(t = x, bet = bet[1], eta = eta[1]), type = "n",  
      ylim = c(0, .04), xlim = c(0,200), 
      main = paste("The PDF of Weibull Distributions"), ylab = "f(t)", xlab = "t") 
sapply(1:4, function(b) {
  sapply(1:2, function(e) {
    curve(f_weib(t = x, bet = bet[b], eta = eta[e]), lty = e%%2 +1, lwd = 1.5, add = T, 
          col = b%%4 +1)
  })
})
legend("topright", legend = c(expression(eta==50), expression(eta==100),
                              "", 
                              expression(beta==0.3), expression(beta==1), 
                              expression(beta==3), expression(beta==5)), 
       lty = c(1:2%%2 +1, 0, rep(1, 4)), col = c(1, 1, 0, 1:4%%4 +1))
dev.off()

## (c) explain


##### 5.1
## (a)
## (b)
## (c)

## (d)
ht <- function(t) (exp(-t)+.2*exp(-.2*t))*(exp(-t)+exp(.2*-t))^(-1)
ht_1 <- function(t) exp(-t)/exp(-t)
ht_5 <- function(t) .2*exp(.2*-t)/exp(.2*-t)

pdf("f_5_1_d.pdf")
curve(ht(x), xlim = c(0, 10), ylim = c(0, 1), type = "n", 
      xlab = "t", ylab = "h(t)", main = "Mixture of Exponential Hazard Function")
curve(ht_1(x), lty = 3, add = T, col = 1, lwd = 2)
curve(ht_5(x), lty = 2, add = T, col = 1, lwd = 2)
curve(ht(x)  , lty = 1, add = T, col = 2, lwd = 2)
legend("bottomright", legend = c(expression(theta[1]==1), 
                              expression(theta[2]==5), 
                              "mixture"),
       lty = c(3, 2, 1), col = c(1, 1, 2), lwd = 2)
dev.off()


##### 5.8
the <- c(.1, 1, 10)
kap <- c(1, 5, 8)
h_par <- function(t, the, kap) kap*the/(1+the*t)

pdf("f_5_8_c.pdf")
curve(h_par(x, 1, 1), xlim = c(0, 100), type = "n", xlab = "t", ylab = "h(t)", main = "Pareto Hazard Functions")
sapply(1:3, function(c) {
  sapply(1:3, function(k) {
    curve(h_par(t = x, the = the[c], kap = kap[k]), lty = c%%3 +1, lwd = 1.5, add = T, 
          col = k%%3 +1)
  })
})
legend("topright", legend = c(expression(theta==0.1), expression(theta==1), expression(theta==10),
                              "", 
                              expression(kappa==1), expression(kappa==5), expression(kappa==8)), 
       lty = c(1:3%%3 +1, 0, rep(1, 4)), col = c(1, 1, 1, 0, 1:3%%3 +1))
dev.off()



##### 6.1
## (b)
pdf("f_6_1_b.pdf")
par(mar=c(5,7,6,6)+0.1)
p <- seq(0,0.99,by=0.01)
plot(log(p/(1-p)),1+log(p/(1-p)),type='l',xaxt='n',yaxt='n',ann=F,col=2)
mtext(side=1,line=3,"log(p/(1-p))")
mtext(side=2,line=4,expression(paste(log,"(",t[p],")")),las=2)
mtext(side=3,line=3,expression(paste(F,"(",t[p],")",'=','p')))
mtext(side=4,line=4.5,expression(t[p]),las=2)
points(log(p/(1-p)),1+2*log(p/(1-p)),type='l',col=4)
axis(side=1,at=seq(-5,5,1))
axis(side=2,at=seq(-9,100,0.8),las=2)
axis(side=3,at=seq(-5,5,1)
     ,labels=round(exp(seq(-5,5,1))/(1+exp(seq(-5,5,1))), 2), las = 2)
axis(side=4,at=seq(-9,100,0.8),labels=round(exp(seq(-9,100,0.8)),2),las = 2)
legend('bottomright',c("LOGLOGIS(1,1)","LOGLOGIS(1,2)"),lty=c(1,1)
       ,col=c(2,4),cex=0.7)
dev.off()


############################################################################################
############################################################################################
############################################################################################

##### 5.2
## (b)
F_mix <- function(t, xi) xi*(1-exp(-t)) + (1-xi)*(1-exp(-t/10))
xis <- c(0, .1, .5, .9, 1)
clr <- colorRampPalette(colors = c("red", "orange2", "gold2", "forestgreen"))(5)

pdf("f_5_2_b.pdf")
curve(F_mix(x, 0), xlim = c(0, 30), ylim = c(0, 1), xlab = "time", ylab = "F(t)", type = "n")
sapply(1:5, function(k) curve(F_mix(t = x, xi = xis[k]), add = T, col = clr[k], lwd = 1.5))
legend("bottomright", legend = c(expression(xi == 0), expression(xi == 0.1), 
                                 expression(xi == 0.5), expression(xi == 0.9), 
                                 expression(xi == 1)), 
       lty = 1, lwd = 1.5, col = clr)
dev.off()

## (c)
t <- seq(1, 30, .2)
pdf("f_5_2_c.pdf")
plot(1, 1, xlim = c(0, log(30)), ylim = c(-3, 3), xlab = "log(t)", ylab = "log{-log[1-F(t)]}", type = "n")
sapply(1:5, function(k) {
  Ft <- xis[k]*(1-exp(-t)) + (1-xis[k])*(1-exp(-t/10))
  par(new = T)
  plot(log(t), log(-log(1 - Ft)), type = "l", xlim = c(0, log(30)), ylim = c(-3, 3), 
       ylab = "", xlab = "", col = clr[k], lwd = 1.5)
})
legend("bottomright", legend = c(expression(xi == 0), expression(xi == 0.1), 
                                 expression(xi == 0.5), expression(xi == 0.9), 
                                 expression(xi == 1)), 
       lty = 1, lwd = 1.5, col = clr)
dev.off()

## (d)
ht <- function(t, xi) (xi*exp(-t) + (1-xi)*.1*exp(-t/10))/(xi*exp(-t) + (1-xi)*exp(-t/10))
pdf("f_5_2_d.pdf")
curve(ht(x, 1), xlim = c(1, 30), ylim = c(0, 1), type = "n", xlab = "t", ylab = "h(t)")
sapply(1:5,  function(k) curve(ht(x, xis[k]), add = T, col = clr[k], lwd = 1.5))
legend("bottomright", legend = c(expression(xi == 0), expression(xi == 0.1), 
                                 expression(xi == 0.5), expression(xi == 0.9), 
                                 expression(xi == 1)), 
       lty = 1, lwd = 1.5, col = clr)
dev.off()

##### 6.5
## (a)
t_i <- c( 17.88,  28.92,  33.00,  41.52,  42.12, 45.60, 
          48.40,  51.84,  51.96,  54.12,  55.56, 67.80, 
          68.64,  68.64,  68.88,  84.12,  93.12, 98.64, 
         105.12, 105.84, 127.92, 128.04, 173.40) 
F_t <- 1:23/23
pdf("f_6_5_a.pdf")
plot(t_i, F_t, xlab = "", ylab = "", pch = 16)
dev.off()

## (b)
mu  <- mean(t_i)
sig <- sd(t_i)
p   <- (1:23-.5)/23
log_tp <- mu+sig*qnorm(p)
pdf("f_6_5_b.pdf")
plot(log_tp, qnorm(F_t), xlab = "", ylab = "", pch = 16)
dev.off()

## (c)
tp_weibull <- exp(mu + sig*qnorm(p))
log_tp <- mu+sig*log((-1)*log(1-p))
pdf("f_6_5_c.pdf")
plot(log_tp, log((-1)*log(1-p)), ylab="", xlab="", pch = 16)
dev.off()
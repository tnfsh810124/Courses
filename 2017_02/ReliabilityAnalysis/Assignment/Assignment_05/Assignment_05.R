### 7.6 ##########################
Fi <- 1:7/11
ti <- c(7.74,17.05,20.46,21.02,22.66,43.40,47.30)

### a
pdf('fig_7_6_a.pdf', height = 4, width = 7)
par(mfrow = c(1, 2))
# weibull
plot(ti, log(-log(1-Fi)), xlim=c(0,100), ann=F, pch = 16)
mtext(side=1,line=3,expression(t[p]))
mtext(side=2,line=3,'log(-log(1-p))')
mtext(side=3,line=3,'Weibull')
# exponential
plot(ti, -log(1-Fi), xlim=c(0,100), ann=F, pch = 16)
mtext(side=1,line=3,expression(t[p]))
mtext(side=2,line=3,'-log(1-p)')
mtext(side=3,line=3,'Exponential')
par(mfrow = c(1, 1))
dev.off()

### b 
# comment

### c, d, e, f
# inference




### 8.2 ##########################
library(survival)

### a
# exponential
t1 <- c(rep(1,4),rep(1,5),rep(2,2),rep(3,95),rep(1,99),rep(2,95))
t2 <- c(rep(1,4),rep(2,5),rep(3,2),rep(3,95),rep(1,99),rep(2,95))
ev <- c(rep(2,4),rep(3,7),rep(0,289))

A <- Surv(t1, t2, ev, type = "interval")
survreg(A~1,dist = "exponential")   # intercept:mu   scale:sigma

# Fit an exponential to the data
pdf("fig_8_2_a.pdf")
par(mar=c(6,6,6,6)+0.1)
theta.hat <- 53.49847  # MLE of theta
p <- seq(0.001, 0.08, by=0.01)
plot(-theta.hat*log(1-p), -log(1-p), type='l', lwd=2, xaxt='n', yaxt='n', ann=F)
mtext(side=4,line=3,"-log(1-p)")
mtext(side=2,line=4,'Proportion Failing')
mtext(side=3,line=2.5,'Exponential')
mtext(side=1,line=3,'time(minute)')
axis(side=4,at=seq(0,0.08,length.out=6)
     ,labels=round(seq(0,0.08,length.out=6),2),las=2)
axis(side=2,at=-log(1-seq(0.001,0.08,length.out=8))         #transform fcn
     ,labels=round(seq(0.001,0.08,length.out=8),3),las=2)
axis(side=1,at=seq(0,4,0.5))

sp=c(0.0133,0.0384,0.0582)
t=c(1,2,3)
points(t,-log(1-sp),pch=16)
dev.off()

### b
survreg(A~1,dist = "weibull")
pdf('fig_8_2_b.pdf')
par(mar=c(6,6,6,6)+0.1)
eta.hat=exp(3.162091)  # MLE of eta
beta.hat=1/0.7432098 # MLE of beta
p=seq(0.001,0.08,by=0.01)
plot(log(eta.hat)+log(-log(1-p))*(1/beta.hat),log(-log(1-p)),type='l',lwd=2
     ,xaxt='n',yaxt='n',ann=F)
mtext(side=4,line=4,"log(-log(1-p))")
mtext(side=3,line=2,expression(paste(log,"(",t[p],")")))
mtext(side=3,line=4,'Weibull')
mtext(side=2,line=4,'Proportion Failing')
mtext(side=1,line=3,'time(minute)')

axis(side=4,at=seq(-7,-2,length.out=7)
     ,labels=round(seq(-7,-2,length.out=7),1),las=2)
axis(side=2,at=round(log(-log(1-seq(0.001,0.08,length.out=10))), 1)   
     ,labels=round(seq(0.001,0.08,length.out=10),3),las=2)
axis(side=3,at=seq(-2,2,length.out=8)
     ,labels=round(seq(-2,2,length.out=8),1))
axis(side=1,at=log(seq(0,3,0.5))
     ,labels=round(seq(0,3,0.5),1))

sp=c(0.0133,0.0384,0.0582)
t=c(1,2,3)
points(log(t),log(-log(1-sp)),pch=16)
dev.off()
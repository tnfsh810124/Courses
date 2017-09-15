###### Reliability Midterm Exam
library(openair)
# hist(rlnorm(500, meanlog = 5, sdlog = .8))

U <- runif(500)
LNU <- (exp(5 + qnorm(U)*.8))
hist(LNU)

LNf <- function(t) (1/(.8*t))*(1/(2*pi))*exp(-(((log(t)-5)/.8)^2)/2)
LNF <- function(t) pnorm((log(t)-5)/.8)
LNh <- function(t) (1/(.8*t))*(1/(2*pi))*exp(-(((log(t)-5)/.8)^2)/2)/(1-pnorm((log(t)-5)/.8))

pdf("fig_4.pdf", width = 4, height = 6)
par(mfrow = c(3, 1))
hist(LNU, main = "Histogram of Pseudorandom Observations", xlab = "t")
curve(LNf, from = 0, to = 1500, main = "f(t) of log-normal", ylab = "f(t)", xlab = "t")
curve(LNh, from = 0, to = 1500, main = "f(h) of log-normal", ylab = "H(t)", xlab = "t")
par(mfrow = c(1, 1))
dev.off()


median(LNU)

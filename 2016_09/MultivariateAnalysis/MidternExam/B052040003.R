##### B052040003 #####
library(reshape)
# 1 #
df1 <- read.table("lizard.txt", header = T)
y1  <- log(df1$Mass)
y2  <- log(df1$SVL)
# (a)
boxplot(log(cbind(g0$Mass, g1$Mass, g0$SVL, g1$SVL)), main = "1 (a)", 
        col = c("gold", "darkorange", "violet", "darkviolet"), border = gray(.7), 
        xaxt = "n")
axis(1, 1:4, labels = c("Mass 0", "Mass 1", "SVL 0", "SVL 1"))
# (b)
plot(y1 ~ y2, col = df1$genera +2, main = "1 (b)", pch = 16, cex = .8, 
     xlab = "log(SVL)", ylab = "log(Mass)", asp = 1)
legend(3.5, 3.5, legend = c(0, 1), col = c(0, 1)+2, pch = 16, cex = .8)
g0 <- df1[df1$genera == 0, 1:2]
g1 <- df1[df1$genera == 1, 1:2]
abline(lm(log(g0$Mass) ~log(g0$SVL)), col = "brown")
abline(lm(log(g1$Mass) ~log(g1$SVL)), col = "darkgreen")
# (c)
df1.means <- data.frame(rbind(colMeans(g0), colMeans(g1)))
row.names(df1.means) <- c("0", "1")
df1.means
cov(g0)
cov(g1)
Spool <- ((20-1)*cov(g0) + (40-1)*cov(g1))/(20+40-2)
Spool
# (d)
shapiro.test(y1)
shapiro.test(y2)
t.test(y1, y2, var.equal = T, paired = F)
# (e)
var.test(y1, y2)
# (f)
t.test(y1, y2, var.equal = F, paired = F)

# 2 #
df2 <- read.table("men.txt", header = T)
df2n <- df2$Country
df2 <- df2[, -1]
row.names(df2) <- df2n
# (a)
colMeans(df2)
cov(df2)
cor(df2)
# (b)
fit <- princomp(df2, cor = T)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit, type = "b")
fit$scores # the principal components
biplot(fit)


# ken1234582@gmail.com
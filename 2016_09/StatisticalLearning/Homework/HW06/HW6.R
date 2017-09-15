#install.packages("ISLR")
library(ISLR)
library(MASS)
library(class)

### 10 ###
data("Weekly")
Weekly <- data.frame(Weekly)
attach(Weekly)
# (a) #
pairs(Weekly)
round(cor(Weekly[,1:8]), 3)

# (b) #
lmb <- glm(Direction ~ Volume+Lag1+Lag2+Lag3+Lag4+Lag5, family = binomial)
summary(lmb)

# (c) #
lmb.prdct <- predict(lmb, type = "response")
dir.prdct <- cut(lmb.prdct, breaks=c(0,0.5,1), labels = c("Down", "Up"))
table(dir.prdct, Direction)
(48+430)/1089

# (d) #
train <- Weekly[Year<=2008, ]
test  <- Weekly[Year>2008, ]

lmd <- glm(Direction ~ Lag2, data = train, family = "binomial")
summary(lmd)
lmd.prdct <- predict(lmd, test, type = "response")
dir.prdct <- cut(lmd.prdct, breaks=c(0,0.5,1), labels = c("Down", "Up"))
table(dir.prdct, test$Direction)
(5+34)/104

# (e) #
lda.e <- lda(Direction ~ Lag2, data = Weekly, subset=Year<=2008)
lda.e.prdct <- predict(lda.e, test)
table(lda.e.prdct$class, test$Direction)
(5+34)/104

# (f) #
qda.f <- qda(Direction ~ Lag2, data = Weekly, subset=Year<=2008)
qda.f.prdct <- predict(qda.f, test)
table(qda.f.prdct$class, test$Direction)
(43+0)/104

# (g) #
train.x <- as.matrix(train$Lag2)
train.dir <- as.matrix(train$Direction)
test.x  <- as.matrix(test$Lag2)
set.seed(1)
knn.prdct <- knn(train.x, test.x, train.dir, k=1)
table(knn.prdct, test$Direction)
(22+30)/104

# (h) #

# (i) #
lmi <- glm(Direction ~ Lag2:Volume, data = train, family = "binomial")
summary(lmi)
lmi.prdct <- predict(lmi, test, type = "response")
dir.prdct <- cut(lmi.prdct, breaks=c(0,0.5,1), labels = c("Down", "Up"))
table(dir.prdct, test$Direction)
mean(dir.prdct != test$Direction)

lda.i <- lda(Direction ~ Lag2:Volume, data = Weekly, subset=Year<=2008)
lda.i.prdct <- predict(lda.i, test)
table(lda.i.prdct$class, test$Direction)
mean(lda.i.prdct$class != test$Direction)


qda.i <- qda(Direction ~ Lag2:Volume, data = Weekly, subset=Year<=2008)
qda.i.prdct <- predict(qda.i, test)
table(qda.i.prdct$class, test$Direction)
mean(qda.i.prdct$class != test$Direction)


train.x <- as.matrix(train$Lag2)
train.dir <- as.matrix(train$Direction)
test.x  <- as.matrix(test$Lag2)
knn.prdct.i10 <- knn(train.x, test.x, train.dir, k=10)
table(knn.prdct.i10, test$Direction)
mean(knn.prdct.i10 != test$Direction)


knn.prdct.i100 <- knn(train.x, test.x, train.dir, k=100)
table(knn.prdct.i100, test$Direction)
mean(knn.prdct.i100 != test$Direction)

library(car)
library(boot)
library(glmnet)
library(leaps)
library(bestglm)
wine <- read.csv("winequality-white.csv", header = TRUE, sep = ";")
test.num <- sample(dim(wine)[1], round(0.1*(dim(wine)[1])))
df   <- wine[-test.num, ]
test <- wine[ test.num, ]

###########################################################################
####### summary statistics
###########################################################################
# correlaion #
library(corrgram)
corrgram(df, order = NULL, lower.panel=panel.shade,
         upper.panel = NULL, text.panel=panel.txt,
         main="The Correlation Plot", abs = T)
cor.df <- as.data.frame(round(cor(df), 2))
names(cor.df) <- rep("", 12)
row.names(cor.df) <- 1:12
paste(cor.df[12, ], collapse = " & ")
###########################################################################
####### Modeling
###########################################################################
# original full model #
fit <- glm(quality ~.  , data = df, family = "gaussian")
summary.0 <- summary(fit)
summary(predict(fit, df))
sqrt(mean((predict(fit, newdata = test, type = "response") - test$quality)^2))
mean(round(predict(fit, newdata = test, type = "response")) == test$quality)
mean(vif(fit) >= 5)
rsp <- predict(fit, newdata = test)
prd <- round(rsp)
tru <- test$quality
table(prd, tru)

# leave-one-in collinearity elimination #
# checking collinearity
which(vif(fit) >= 5)
fit.loi.1.aic <- 
  sapply(which(vif(fit) >= 5), function(i) {
    fit.loi.1 <- glm(quality ~.  , 
                     data = df[, -which(vif(fit) >= 5)[which(which(vif(fit) >= 5) != i)]], 
                     family = "gaussian")
    summary(fit.loi.1)$aic
  })
which(vif(fit) >= 5)[which.min(fit.loi.1.aic)]
df1 <- df[, -which(vif(fit) >= 5)[which(which(vif(fit) >= 5) != 11)]]
# new model without collinearity
fit1 <- glm(quality ~.  , data = df1, family = "gaussian")
summary(fit1)
summary(predict(fit1, df1))
sqrt(mean((predict(fit1, newdata = test, type = "response") - test$quality)^2))
mean(round(predict(fit1, newdata = test, type = "response")) == test$quality)
rsp <- predict(fit1, newdata = test)
prd <- round(rsp)
tru <- test$quality
table(prd, tru)

summary.1 <- summary(fit1)
p.v.1 <- summary.1$coefficients[, 4]
df2 <- df1[, p.v.1 <.05]
# new model without insignificant predictor
fit2 <- glm(quality ~.  , data = df2, family = "gaussian")
summary(fit2)
summary(predict(fit2, df2))
sqrt(mean((predict(fit2, newdata = test, type = "response") - test$quality)^2))
mean(round(predict(fit2, newdata = test, type = "response")) == test$quality)
summary.2 <- summary(fit2)
rsp <- predict(fit2, newdata = test)
prd <- round(rsp)
tru <- test$quality
table(prd, tru)


full.comp <- 
  cbind(
    c(sqrt(mean((predict(fit, newdata = test, type = "response") - test$quality)^2)), 
      mean(round(predict(fit, newdata = test, type = "response")) != test$quality)) , 
    c(sqrt(mean((predict(fit1, newdata = test, type = "response") - test$quality)^2)), 
      mean(round(predict(fit1, newdata = test, type = "response")) != test$quality)) , 
    c(sqrt(mean((predict(fit2, newdata = test, type = "response") - test$quality)^2)), 
      mean(round(predict(fit2, newdata = test, type = "response")) != test$quality))
  )

full.comp <- as.data.frame(round(full.comp, 3))
names(full.comp) <- c("oridinal", "with out collinearity", "significant")
row.names(full.comp) <- c("MSE", " Error Rate")
full.comp <- t(full.comp)


## best subset ##
fit.bs <- bestglm(df, IC = "AIC", family = gaussian)$BestModel
summary(fit.bs)
rsp <- predict(fit.bs, newdata = test)
prd <- round(rsp)
tru <- test$quality
mean((rsp-tru)^2)
table(prd, tru)
mean(prd != tru)
summary(fit.bs)
full.comp <- rbind(full.comp, c(mean((rsp-tru)^2), mean(prd != tru))  )
rownames(full.comp)[4] <- "best subset"


## Lasso ##
xmat <- model.matrix(quality ~., data=df)[,-1]
fit.lasso <- cv.glmnet(xmat, df$quality, alpha=1, family="gaussian")
best.lambda <- fit.lasso$lambda.min
best.lambda
plot(fit.lasso)
round(coef(fit.lasso), 4)
fit.lasso <- glm(quality ~ volatile.acidity + chlorides + pH + alcohol, data = df, family = "gaussian")
rsp <- predict(fit.lasso, newdata = test)
prd <- round(rsp)
tru <- test$quality
mean((rsp-tru)^2)
table(prd, tru)
mean(prd != tru)
full.comp <- rbind(full.comp, c(mean((rsp-tru)^2), mean(prd != tru))  )
rownames(full.comp)[5] <- "Lasso"

## PCA ##
library(psych)
winepca <- wine[,-12]
fit.pca <- principal(winepca,nfactors=6,covar =F,rotate = "none",scores = T)
pcr.data <- as.data.frame(cbind(as.matrix(fit.pca$scores) ,as.matrix(wine$quality)))
train.pcr <- pcr.data[-test.num, ]
test.pcr  <- pcr.data[ test.num, ]
fit.pcr <- glm(V7~.,data = train.pcr,family = "gaussian")
rsp <- predict(fit.pcr, newdata = test.pcr)
prd <- round(rsp)
tru <- test$quality
mean((rsp-tru)^2)
table(prd, tru)
mean(prd != tru)
full.comp <- rbind(full.comp, c(mean((rsp-tru)^2), mean(prd != tru))  )
rownames(full.comp)[6] <- "PCR"

## Tree ##
library(rpart)
library(rpart.plot)
fit.tree <- rpart(as.factor(quality)~., data = df)
summary(fit.tree)
rpart.plot.version1(fit.tree)
prd <- predict(fit.tree, newdata = test, type = "class")
tru <- test$quality
table(prd, tru)
mean(prd != tru)
full.comp <- rbind(full.comp, c(NA, mean(prd != tru)))  
rownames(full.comp)[7] <- "Decision Tree"

## Random Forest ##
library(randomForest)

sqrt(11)
fit.rf <- randomForest(as.factor(df$quality)~.,
                       data = df, mtry = 4,
                       importance = TRUE, ntree = 500)
prd <- predict(fit.rf, newdata = test)
tru <- test$quality
table(prd, tru)
mean(prd != tru)
full.comp <- rbind(full.comp, c(NA, mean(prd != tru)))  
rownames(full.comp)[8] <- "Random Forest"
varImpPlot(fit.rf,type = 2, col = "green4", pch = 16)

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################


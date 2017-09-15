library(MASS)

df <- read.csv("train_u6lujuX_CVtuZ9i.csv", header = T, sep = ",")
sapply(1:dim(df)[2], function(x) c(names(df)[x], levels(df[, x])))
nafactor <- 
  sapply(c(1:6, 12,13), function(x){
    sapply(1:dim(df)[1], function(n){
      ifelse(df[n, x] == "", NA, as.character.factor(df[n, x]))
    })
  })
nafactor <- as.data.frame(nafactor)
names(nafactor) <- names(df)[c(1:6, 12,13)]
for(j in 1:6){
  df[, j] <- nafactor[, j]
}
for(j in 7:8){
  df[, j + 5] <- nafactor[, j]
}
df[,13] <- ifelse(df[, 13]=="Y", 1, 0)
df <- na.omit(df)
test <- read.csv("test_Y3wMUE5_7gLdaTN.csv", header = T)
formula_full <- as.formula(paste0("Loan_Status ~ ", paste(names(df)[2:11], collapse = " + ")))
################################################
glm_full <- glm(formula_full, data = df, family = binomial)
prdct <- ifelse(predict(glm_full, type = "response") > .5, 1, 0)
mean(prdct== df$Loan_Status)
################################################
set.seed(87)
tr_num <- sample(dim(df)[1], 0.5*dim(df)[1])
tr <- df[-tr_num, ]
te <- df[ tr_num, ]
glm_tr <- glm(formula_full, data = tr, family = binomial)
prdct_tr <- ifelse(predict(glm_tr, type = "response") > .5, 1, 0)
mean(prdct_tr== tr$Loan_Status)
################################################
library(glmnet)
tr.mat <- model.matrix(formula_full , data = tr)
te.mat <- model.matrix(formula_full , data = te)
grid <- 10^seq(4, -2, length = 100)
mod.lasso <-  cv.glmnet(tr.mat, tr$Loan_Status, alpha = 1, lambda = grid, thresh = 1e-12)
lambda.best <- mod.lasso$lambda.min
lambda.best
lasso.prd <- predict(mod.lasso, newx = te.mat, s = lambda.best)
lasso.response <- ifelse(lasso.prd >= .5, 1, 0)
mean(te$Loan_Status == lasso.response)
mod.lasso <- glmnet(model.matrix(formula_full, data = tr), tr$Loan_Status, alpha = 1)
predict(mod.lasso, s = lambda.best, type = "coefficients")

################################################
Property_AreaSemiurban <- ifelse(df$Property_Area == "Semiurban", 1, 0)
train_final <- cbind.data.frame(df$Credit_History, Property_AreaSemiurban, df$Loan_Status)
names(train_final) <- c("Credit_History", "Property_AreaSemiurban", "Loan_Status")
final <- glm(Loan_Status ~ Credit_History + Property_AreaSemiurban, data = train_final, family = "binomial")
Property_AreaSemiurban <- ifelse(test$Property_Area == "Semiurban", 1, 0)
test_final <- cbind.data.frame(test$Credit_History, Property_AreaSemiurban)
names(test_final) <- c("Credit_History", "Property_AreaSemiurban")
predict_final <- ifelse(predict(final, 
                                newdata  = test_final, 
                                type = "response") >= 0.5 , 1, 0)
predict_final <- data.frame(cbind(test$Loan_ID, predict_final))
predict_final <- cbind.data.frame(test$Loan_ID, predict_final)
predict_final <- predict_final[, -2]
write.csv(predict_final, "result.csv")


################################################
fit <- glm(Loan_Status ~. , data = df[, -1], family = "binomial")
summary(fit)
car::vif(fit)

fit <- glm(Loan_Status ~ Married + Credit_History + ifelse(Property_Area == "Semiurban", 1, 0)
           , data = df)
summary(fit)
predict_final <- ifelse(predict(fit, newdata = test)<=.5, 0, 1)
predict_final[is.na(predict_final)] <- 0
result <- as.data.frame(cbind(as.character(test$Loan_ID), as.numeric(predict_final)))
names(result) <- names(df)[c(1, 13)]
write.csv(result, "result.csv")

#******* Group information *******

#Group : 

#Please rename this file to Group_.R
#Please write your code in each block.

#******* end of Group information *******
#install.packages("epiDisplay")
library(epiDisplay)
library(data.table)
library(caret)

#******* 1. Download data and do some data management tasks *******

df <- na.omit(read.csv("train.csv", header = T))[, 2:12]
attach(df)

#********** end of 1. **********


#********** 2. fit logit models and report 10-fold MR **********
set.seed(87)
ten_folds <- createFolds(1:714, k = 10)

cv_n_p <- function(p){
  sapply(1:10, function(i){
    trn <- df[-ten_folds[[i]], ]
    tst <- df[ ten_folds[[i]], ]
    mdl <- glm(fmla, data = trn, family = "binomial")
    rsp <- predict(mdl, newdata = tst, type = "response")
    prd <- ifelse(rsp > p, 1, 0)
    act <- tst[, 1]
    mean(prd != act)
  })
}


fmla <- Survived ~ (as.factor(Pclass) + Sex )^2
mn_cv_n_p <- cbind(seq(0, 1, .01), 
                   sapply(seq(0, 1, .01), FUN = function(p) mean(cv_n_p(p))))
plot(mn_cv_n_p[, 2] ~ mn_cv_n_p[, 1], type = "l", 
     main = "10-folds MR", xlab = "cut-off point", ylab = "misclassification rate")
abline(v = mn_cv_n_p[which.min(mn_cv_n_p[, 2]), 1], lty = 2, col = gray(.3))
legend("topright", legend = c(paste0("minimum MR = ", round(min(mn_cv_n_p[, 2]), 4)), 
                                        paste0("cut-off point = ", mn_cv_n_p[which.min(mn_cv_n_p[, 2]), 1])))

#********** end of 2. **********


#********** 3.  Remember Jack and Rose in the movie Titanic?  **********
#********** What are their probabilities of surviving the event? **********
mdl <- glm(formula = fmla, df, family = "binomial")
Jack <- c(0, 3, "Jack", "male", 20, 0, 0, "godie", mean(df$Fare[df$Pclass==3]), NA, "S")
Rose <- c(1, 1, "Rose", "female", 17, 0, 1, "possy", mean(df$Fare[df$Pclass==1]), NA, "S")

JR <- as.data.frame(t(cbind(Jack, Rose)))
for(j in c(1, 2, 5, 6, 7 ,9)){
  JR[, j]<- as.numeric(JR[, j])
}
names(JR) <- names(df)[1:11]

predict(mdl, newdata = JR, type = "response")
#********** end of 3. **********



#********** 4. report result of logit model using epiDisplay
epiDisplay::logistic.display(mdl)


#********** end of 4. **********

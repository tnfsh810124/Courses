##  1  ##
Sys.setenv("HADOOP_CMD" = "/home/hadoop/hadoop/bin/hadoop");
Sys.setenv("HADOOP_STREAMING"="/home/hadoop/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.7.2.jar")
library(rhdfs)
library(ggplot2)
library(plyrmr)
library(rmr2)
library(ISLR)
library(leaps)
library(caTools)
library(nortest)
library(car)
hdfs.init()
dmdf <- data.frame(ggplot2::diamonds)
dmdf <- as.data.frame(lapply(dmdf, FUN = function(col) 
  if (is.factor(col)) col = factor(col, order = FALSE) else col))
to.dfs(dmdf,"/home/M052040003/dmdf.RData")

# 1.1 #
mapreduce(input  = "/home/M052040003/dmdf.RData",
          output = "/home/M052040003/dmdf_col.RData",
          map = function(k, v)
            keyval(key = v$color, val = v$price), 
          reduce = function(k, v)
            keyval(key = k, val = mean(v))
)
from.dfs("/home/M052040003/dmdf_col.RData")

# 1.2 #
to.dfs(data.frame(transmute(group(input("/home/M052040003/dmdf.RData")
                                  %|% where(depth >= 65),.columns = c("cut")),mean(carat))),
       "/home/M052040003/dmdf_avg.RData")
from.dfs("/home/M052040003/dmdf_avg.RData")

# 1.3 #
fit <- step(lm(price ~. , data = dmdf), direction = "both")
summary(fit)
# By the mixed selection method, all of the picked predictors excluding "z" are significant
vif(fit)
# Varify the collinearity, carat, x and y are relatively high.
# Now we will do "leave-one-in" model selection.
colinear.bth <- which(vif(fit)[, 3] > 1.5)
colinear.in.dmdf <- which(names(dmdf) %in% names(colinear.bth))
leave.one.in.ar2 <- 
  sapply(colinear.in.dmdf, function(v) {
    fit.1 <- step(lm(dmdf$price ~. , 
                               data = dmdf[, -colinear.in.dmdf[which(colinear.in.dmdf != v)]]
                               ), direction = "both");
    sum.fit.1 <- summary(fit.1);
    sum.fit.1$adj.r.squared
  })
leave.one.in.ar2 <- as.data.frame(t(leave.one.in.ar2))
names(leave.one.in.ar2) <- names(dmdf)[colinear.in.dmdf]
names(dmdf)[which.max(leave.one.in.ar2)]
# carat is chosen for its largest adj R^2
fit.2 <- step(lm(dmdf$price ~. , 
                           data = dmdf[, -colinear.in.dmdf[which(colinear.in.dmdf != 1)]]), direction = "both")
as.matrix(fit.2$coefficients)
summary(fit.2)
# all the predictors of the new model are significant the adj R^2, 0.9177 is still high

## 2 ##
hitters <- Hitters
hitters <- na.omit(hitters)
set.seed(66)
tr.num <- sample(dim(hitters)[1],round(0.9*dim(hitters)[1]))
train <- hitters[ tr.num, ]
test  <- hitters[-tr.num, ]

# 2.1 #
fit.3 <- step(lm((Salary)^(1/3) ~. , data = hitters), direction = "both")
summary(fit.3)
mean(((predict(fit.3, newdata = test))^3 - test$Salary)^2)
# test error = 66288.2

# 2.2 #
train.mat <- model.matrix(Salary~.,data = train)
test.mat  <- model.matrix(Salary~.,data = test)
fit.4 <- glmnet(train.mat, train$Salary, alpha = 1, lambda = grid, thresh = 1e-12)
cv.4 <- cv.glmnet(train.mat, train$Salary, alpha = 1, lambda = grid, thresh = 1e-12,nfolds = 10)
bestlam.4 <- cv.4$lambda.min
bestlam.4
pred.4 <- predict(fit.4, s = bestlam.4, newx = test.mat)
mean((pred.4 - test$Salary)^2)
# test error = 107725.8
predict(fit.4, s = bestlam.4, type = "coefficients")
# (Intercept)  219.9463927
# AtBat         -1.9730273
# Hits           6.1131546
# HmRun          1.8158681
# Runs          -1.0227424
# RBI            0.7187129
# Walks          5.0679937
# Years        -15.3918671
# CAtBat        -0.2200977
# CHits          0.7746182
# CHmRun         0.7521240
# CRuns          1.1012896
# CWalks        -0.4469642
# LeagueN       64.9321178
# DivisionW   -122.8922496
# PutOuts        0.2874596
# Assists        0.3732633
# Errors        -3.3629923
# NewLeagueN   -22.4965580

# 2.3 #






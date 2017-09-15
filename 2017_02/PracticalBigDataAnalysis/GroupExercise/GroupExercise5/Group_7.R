library(rpart); library(ggplot2); library(parallel);

# Use all CPU cores may freeze your machine!
cl <- makeCluster(max(round(0.8*detectCores()), 1))
# Export randomForest to all R instances
clusterExport(cl, "rpart")

smp <- sample(dim(df)[1], round(0.7*dim(df)[1]))
trn <- df[ smp, ]
tst <- df[-smp, ]

baggggg <- 
  function(trn, tst, k, perc){
    num   <- 0:9
    k.bags <- lapply(1:k, function(x) sample(1:dim(trn)[1], round(dim(trn)[1]*perc)))
    y <- trn[, dim(trn)[2] ]
    fmla <- as.formula(paste(names(trn)[dim(trn)[2]], paste(names(trn)[-dim(trn)[2]], collapse = " + "), sep = " ~ "))
    BagMod <- parLapply(cl, 1:k, function(i) rpart(formula = fmla, data = trn[k.bags[[i]], ]))
    rsp <- 
      sapply(1:k, function(i) list(predict(BagMod[[k]], newdata = trn[-k.bags[[i]], ])))
    name  <- as.factor(names(as.data.frame(rsp[[1]])))
    err <- 
      sapply(1:k, function(k) {
        prd <- sapply(1:dim(rsp[[k]])[1], function(i) name[which.max(rsp[[k]][i, ])])
        mean(trn[-k.bags[[k]], dim(trn)[2]] != prd)
      })
    rsp_n <- sapply(1:k, function(i) list(predict(BagMod[[k]], newdata = tst)))
    prd_n <- sapply(1:dim(rsp_n[[k]])[1], function(i) name[which.max(rsp_n[[k]][i, ])])
    
    list("MR" = mean(err), "Prediction" = prd_n)
  }


baggggg(iris[seq(3, 150, 3), ], iris[-seq(3, 150, 3), ], 100, .9)
num   <- 0:9
prd   <- sapply(1:dim(rsp[[1]])[1], function(i) num[which.max(rsp[[1]][i, ])])



# Stop all worker instances
stopCluster(cl)

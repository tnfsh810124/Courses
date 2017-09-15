library(ggplot2)
library(cvTools)
df <- data.frame(ggplot2::mpg)
attach(df)
### 1
summary(df)

boxplot(cty ~ manufacturer, data = df)

### Bar plots
green_gradient <- colorRampPalette(c("green", "darkgreen"))
barplot(table(df[,1]), col = green_gradient(length(table(df[,1]))), cex.names = .9, 
        xlab = names(df)[1], ylab = "number of models")


### 2
summary(lm(df$cty ~ df$displ))
summary(lm(df$hwy ~ df$displ))

Map(function(x) {fit = lm(x, data = df); anova(fit)$"Pr(>F)"[1]}, 
    Map(as.formula, paste("cty ~", colnames(df)[-c(7, 8)])))
# for pr

Map(function(x) {fit = lm(x, data = df); anova(fit)$"Pr(>F)"[1]}, 
    Map(as.formula, paste("hwy ~", colnames(df)[-c(7, 8)])))


### 3
LOOCV_lm_rmse = function(f, d){
  errs = sapply(1:nrow(d), FUN = function(k){
    reponse_var = all.vars(f)[1]; # Name of reponse variable
    m = lm(f, d[- k,], na.action = na.omit)
    return((d[[reponse_var]][k]) - predict(m, newdata = d[k,]))
  })
  return(round(sqrt(mean(errs ^ 2)),4))
}




f1 <- as.formula(paste0("cty ~ displ + cyl + model" , paste(paste(c("displ", "cyl"), sep = ":" , c("trans" , "manufacturer")), collapse = " + ")))
LOOCV_lm_rmse(f1, df)
LOOCV_lm_rmse(cty ~ (displ + cyl)^2 + model + class:cyl + drv:cyl, df)

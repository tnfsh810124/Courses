# 1. Please load dataset Boston in package MASS. Explore a bit about 
# this dataset by getting some descriptive statistics and/or frequency tables.

library(MASS)
# get decription about this data
?Boston 
# Load "Boston"
bt_df = as.data.frame(Boston)

# To convert "chas" as a factor vector, as it is here considered a categorical variable
bt_df$chas = factor(bt_df$chas, labels  = c("otherwise", "tract bounds river")) 

# Get summary results using fivenum() & table() 
Map(function(x) ifelse( is.numeric(x), list(fivenum(x)), list(table(x, useNA = "always"))), bt_df)

# 2. "Plot your data" to get visual senses of the variables. You may consider boxplots, 
# density plots, or any other plots that help you understand the data.

# Library to plot grid of figures
library(gridExtra)
# Density plots
densityPlots = Map(function(x) qplot(bt_df[,x], geom = "density") + xlab(x), colnames(bt_df)[-4] )
marrangeGrob(densityPlots, ncol = 4, nrow = 4)

# Bar plot
qplot(bt_df$chas, geom = "bar") + xlab("chas")


# 3. Run normality tests on continuous variables and see whether these variables look "normal".
library(nortest)
lapply(subset(bt_df,select =-c(chas)),
       function(x) Map(function(f) f(x) , c(shapiro.test, lillie.test,  ad.test) ))
# All of them seem NOT normal. 

# 4. Check whether the continuous predictors are linearly correlated to 
# the variable “medv" by using appropriate tests of correlations/associations. 

# Spearman's rho
lapply(subset(bt_df,select = -c(chas, medv)), function(x) cor.test(x, bt_df$medv,method="spearman"))
# Or use Hmisc::rcorr()
Hmisc::rcorr( as.matrix(bt_df[, -which(colnames(bt_df) %in% c("chas") )]) ,type = "spearman" )

# Kendall's tau
lapply(subset(bt_df,select = -c(chas, medv)), function(x) cor.test(x, bt_df$medv,method="kendall"))

# It is questionable to claim the linear correlations based only on "p-value < 0.05" and "correlation coefficients".
# Further assessments of the quality of estimates, such as permutation tests or bootstrapping might help. 
# Here is an example of bootstrapping:
library(boot)
# Function to get spearman correlation coefficient
bs_cors = function(data, indices, x, y){
  return(cor(data[indices, x], data[indices, y], method = "spearman"))
}
# Bootstrapping correlations - "crim" vs. "medv"
r = boot(data = bt_df, statistic = bs_cors, R = 2000, x = "crim", y = "medv")
# Getting 95% CI of the cofficient
boot.ci(r, type = "norm")


# 5. Perform a series of bivariate analyses using simple linear models to 
# find out the connections between "medv" (as the target variable) and 
# the rest of variables. Do you see anything interesting?

# Fitting bivariate linear models
LMs = Map(function(x) list( eval(parse(text = paste("lm(medv ~ ", x, ", data = bt_df)"  ))) ),
          colnames(bt_df)[-14])
# Get model summary results
Map(function(x) summary(x[[1]]), LMs)

# How about we put everything in a linear model?
summary(lm(medv ~ ., data = bt_df))
# It seems that p-values of some variables ("indus" and "age") become much higher, which suggests that 
# there might have some complex connections among predictors. 
# Further feature engineering process is needed.  



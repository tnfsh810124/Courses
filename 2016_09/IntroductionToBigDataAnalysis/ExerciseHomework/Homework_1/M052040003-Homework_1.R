### M052040003 Chung, Kuan-I, Applied Mathematics 

library(data.table)
library(rhdfs)
library(rmr2)
library(plyrmr)
library(RSQLite)
library(sqldf)
# 1
# 1.1
# remove the records with “Loans that do not meet the credit policy”
dfa <- fread("LoanStats3a.csv", skip = 1,  
             stringsAsFactors = F, verbose = T, nrows = 39786 , data.table = F , 
             na.strings = "")
dfb <- fread("LoanStats3b.csv", skip = 1,  
             stringsAsFactors = F, verbose = T, nrows = 188183, data.table = F ,
             na.strings = "")
dfc <- fread("LoanStats3c.csv", skip = 1,  
             stringsAsFactors = F, verbose = T, nrows = 235631, data.table = F , 
             na.strings = "")
dfd <- fread("LoanStats3d.csv", skip = 1,  
             stringsAsFactors = F, verbose = T, nrows = 421097, data.table = F ,
             na.strings = "")
# 1.2
# combine dfa-d
df <- rbind(dfa, dfb, dfc, dfd)
# keep those with loan_status in "Fully Paid" and "Charged Off"
df <- df[df$loan_status %in% c("Charged Off", "Fully Paid" ), ]

# 1.3
# remove those columns with any NAs
df <- df[, colSums(is.na(df)) == 0]

# 1.4
# the percentage of the “Charged Off” loan
sum(df$loan_status == "Charged Off")/length(df$loan_status)
# 0.1987344

# 1.5
df$loan_amnt <- as.numeric(df$loan_amnt)
# Split, by emp_length
sp_df <- split(df, df$emp_length)
# Apply, get average df amounts
result <-  sapply(sp_df, function(x) mean(as.numeric(x$loan_amnt))) 
# Combine, into a data frame
result <-  data.frame("Employment_Length" = names(result),
                      "Loan_amount_average" = unname(result)); 
result
# SQL code
# Convert the data type
sqldf::sqldf("Select emp_length, AVG(loan_amnt) FROM df GROUP BY emp_length")

# 1.6
# For those of top (> 5000) loan purposes,
# count the number of loans for different grades SELECT grade, count(id) as Grade_Count FROM loan WHERE purpose IN
sql.1.6 <- 
sqldf("SELECT grade, count(id) as Grade_Count FROM df WHERE purpose IN (SELECT purpose FROM df GROUP BY purpose HAVING count(id) >= 5000) GROUP BY grade")
# Rcode
purpose_num <- 
  sapply(levels(as.factor(df$purpose)), 
         function(x) sum(df$purpose == x))
purpose_names_5000 <- names(purpose_num)[purpose_num >= 5000]
A <- df[df$purpose %in% purpose_names_5000, ]
grade_num <- 
  sapply(levels(as.factor(df$grade)), function(x) sum(df$grade == x))
grade_num <- as.data.frame(grade_num)
names(grade_num)<- "Grade_count"
grade_num

# 1.7
sqldf("SELECT grade, avg(annual_inc) FROM df GROUP BY grade")

hdfs.init()
to.dfs(df, "/home/M052040003/Homework_1/df.RData")
mapreduce(input = "/home/M052040003/Homework_1/df.RData",
          output = "/home/M052040003/Homework_1/df_1.7.RData",
          input.format = "native",
          map = function(k, v) keyval(key = NULL, val = v[,c("grade","annual_inc")]) )
hdfs.ls("/home/M052040003/Homework_1/")

from.dfs(mapreduce(input = "/home/M052040003/Homework_1/df.RData", input.format = "native",
                   map = function(k, v) keyval( v[, c("grade", "annual_ing")], val = v),
                   reduce = function(k, v) keyval(v$grade, mean(v$annual_ing))))

# 2
from.dfs(mapreduce(input = "/home/M052040003/Homework_1/df.RData", input.format = "native",
                   map = function(k, v) keyval(key=NULL,val=na.omit(v))))

# 3
# 3.1
# A closure stores a function with an environment

# 3.2
n_percentage <- 
  function(n){
    function(v){
      order.vector <- sort(v, decreasing = T)
      order.vector[1:floor(length(v)*(n/100))]
    }
  }
ten_percentage <- n_percentage(10)
ten_percentage(1:100)
# [1] 100  99  98  97  96  95  94  93  92  91
set.seed(1); ten_percentage(runif(30, 0, 1) ) 
# [1] 0.9919061 0.9446753 0.9347052

# 4
Nobel <- read.csv("Nobel.csv", header = T)
Nobel <- as.data.frame(Nobel)
Nobel <- Nobel[, 1:5]
names(Nobel) <- c("rank", "country", "laureate", "population", "per10million")
Nobel <- Nobel[, c(2, 5)]
Alcohol <- read.csv("Alcohol.csv")
Alcohol <- data.frame(Alcohol)
Alcohol <- Alcohol[, 2:3]
names(Alcohol) <- c("country", "total")

NoAl <- data.frame(cbind(Alcohol, NA))
names(NoAl) <- c("country", "alcohol", "Nobel")
for(k in 1:191){
  NoAl$Nobel[k] <- ifelse(NoAl$country[k] %in% Nobel$country,
                       Nobel$per10million[which(as.character(Nobel$country) == as.character(NoAl$country[k]))],
                       NA)}
contry.names <- NoAl$country
row.names(NoAl) <- contry.names
NoAl <- NoAl[, -1]
plot(NoAl$Nobel ~ NoAl$alcohol, asp = 1, xlab = "alcohol", ylab = "Nobel rate")
# transform the response, Nobel rate
plot(log(NoAl$Nobel) ~ NoAl$alcohol, asp = 1, pch = 16, cex = 0.6,
     xlab = "alcohol", ylab = "log of Nobel rate")
fit1 <- lm(log(NoAl$Nobel) ~ NoAl$alcohol)
summary(fit1)
abline(fit1, col = "blue")
# alcohol is significant; yet, r - square is 0.2441

fit2 <- lm(log(NoAl$Nobel) ~ NoAl$alcohol + 0)
summary(fit2)
abline(fit2, col = "red")
# alcohol is significant; yet, r - square is 0.275

# r-sqare is too small so that there is no linear correlation

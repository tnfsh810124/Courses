### 1. Please convert built-in dataset "Titanic" into an R data frame. Consider the following SQL/R codes.

## 1) Replace below SQL code with equivalent R data aggregation functions.
## select Class, Sex ,Survived , SUM(Freq) from Titanic group by Class, Sex, Survived

# Check the result of the SQL code with sqldf()
tdf = as.data.frame(Titanic)
sqldf::sqldf("select Class, Sex, Survived, SUM(Freq) from tdf group by Class, Sex, Survived")

# Get equivalent result with R Aggregation function
aggregate(x = tdf$Freq, by = list("Class" = tdf$Class,"Sex" = tdf$Sex,"Survived" = tdf$Survived), FUN = sum)
# Use package plyr if you'd like
plyr::ddply(.data = tdf,  ~ Class + Sex + Survived, .fun = function(df) sum(df$Freq))

## 2) Replace below R code with SQL code that does similar split-apply-combine operations. 
## Suppose that our Titanic data frame is named "tdf".
# by(data=tdf$Freq, INDICES=list("Sex"= tdf$Sex, "Age"=tdf$Age), FUN=sum)
sqldf::sqldf('select Sex, Age, SUM(Freq) from tdf group by Sex, Age')

### 2. Consider the following student score data as R vectors.
Stu_name = c("Reuven Ytzrhak", "Bullwinkle Moose","David Jones", 
             "Janice Hammer", "Cheryl Cushing","John Davis", "Greg Knox", 
             "Joel England", "Mary Rayburn", "Angela Williams")
Chinese = c(50, 60, 41, 85, 69, 51, 40, 65, 87, 92)
Math = c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English = c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)

#1) Please create an R data frame "score" by combining these 4 vectors.
score = data.frame(Stu_name,Chinese,Math,English, stringsAsFactors = F)

#2) Calculate a weighted score “ws” with weights (0.4, 0.4, 0.2) for 
# Chinese, Math, and English respectively. Then, add the new column to the data frame. 

# Weighted score
ws = apply(score[2:4], 1, function(x) sum(x * c(0.4, 0.4, 0.2)))
score = cbind(score, ws)

#3) Split the Students’ name "Stu_name" into two columns : First_Name and Last_Name. 
# (Hint: You may use base::strsplit() ). 
#Solution 1 : using do.call()
score_flnames = cbind(do.call(rbind,strsplit(score$Stu_name," ")), score[,2:5])
#Solution 2 : using Reduce()
score_flnames = cbind(Reduce(rbind, strsplit(score$Stu_name," "), init = NULL), score[,2:5])

# The new score data frame is called "score_flnames". 
# Update column names
colnames(score_flnames)[1:2] = c("First_Name","Last_Name")
score_flnames

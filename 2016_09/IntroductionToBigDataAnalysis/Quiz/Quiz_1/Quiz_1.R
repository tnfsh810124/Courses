# M052040003
# quiz 1
# applied mathematics

## 1 ##
install.packages("sqldf")
library(sqldf)
data("Titanic")
df <- data.frame(Titanic)

# 1.1 #
sqldf::sqldf("select Class, Sex ,Survived , SUM(Freq)
from df group by Class, Sex ,Survived")
 
table.1.1<- aggregate(df$Freq, by = list("Class"=df$Class, "Sex"=df$Sex, "Survived"=df$Survived), FUN = sum)
colnames(table.1.1)[4] <- "SUM(Freq)"
na.omit(table.1.1[order(df$Class,df$Sex, df$Survived),])

# 1.2 #

by(data=tdf$Freq, INDICES=list("Sex"= tdf$Sex, "Age"=tdf$Age), FUN=sum)




## 2 ##
Stu_name = c("Reuven Ytzrhak", "Bullwinkle Moose","David Jones",
             "Janice Hammer", "Cheryl Cushing","John Davis", "Greg Knox",
             "Joel England", "Mary Rayburn", "Angela Williams")
Chinese = c(50, 60, 41, 85, 69, 51, 40, 65, 87, 92)
Math = c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English = c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)

# 2.1 #
score <- data.frame(cbind(Stu_name, Chinese, Math, English))

# 2.2 #
attach(score)
ws <- 0.4*Chinese + 0.4*Math + 0.2*English
score <- cbind(score, ws)

# 2.3 #
First_Name<- sapply(1:10, FUN = function(x) {strsplit(Stu_name, " ")[[x]][1]})
Last_Name <- sapply(1:10, FUN = function(x) {strsplit(Stu_name, " ")[[x]][2]})
score<- cbind(First_Name, Last_Name, score[, -1])

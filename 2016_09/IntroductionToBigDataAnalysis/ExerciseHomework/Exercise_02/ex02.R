#############################################
############## exercise 02 ##################
#############################################
 

### 1 ###
LS3a <- read.csv("LoanStats3a.csv", header = T, sep = ",")

# 1.1 #
LS3a <- LS3a[1:39786, ]
LS3a$loan_status <- as.character(LS3a$loan_status)


# 1.2 #
procFreq <- 
  function(df, x, y){
    crosstab <- table(df[, x], df[, y]);
    chi.t <- chisq.test(crosstab);
    return(list("crosstab : " = chi.t, 
                "chi-statistic = " = chi.t$statistic,
                "p-value = " = chi.t$p.value));
  }

apply(list("grage", "purpose", "term"),MARGIN = 2 , 
      function(y) procFreq(LS3a, "loan_status", y))

Map(function(y) procFreq(LS3a, "loan_status", y), 
    list("grage", "purpose", "term"))

### 2 ###
#install.packages("vcd")
#install.packages("reshape2")
library(vcd)
library(reshape2)
data(Arthritis)

# 2.1 #
table.2 <- dcast(data = Arthritis , 
                 Treatment ~ Improved, 
                 fun.aggregate = mean, value.var = "Age")


# 2.2 #
table.2.1 <- xtabs(formula = ~ Treatment + Sex, data = Arthritis)

fisher.test(table.2.1)
# dependent

install.packages(data.table)
library(data.table)

### 3 ###
cars <- read.csv("cars.csv", header = T, sep = ",")
cars = fread("cars.csv", sep=',', na.strings = c("NULL", "."), 
             verbose = T, header = T, 
             stringsAsFactors = F, data.table = F)

cars <- subset(cars, fuelType == "gas")
cars <- na.omit(cars)
cars = cars[complete.cases(cars),]

# Take a look the result of the SQL code
install.packages("sqldf")
library(sqldf)
sqldf::sqldf("Select bodyStyle, avg(highwayMpg) as avgHwMPG from cars group by bodyStyle order by avgHwMPG")

# Use aggregate(). Feel free to try by() or other functions in package "plyr"
car_style_avgMPG = aggregate(cars$highwayMpg, by = list("bodyStyle" = cars$bodyStyle),FUN = mean )
colnames(car_style_avgMPG)[2] = "avgHwMPG"
car_style_avgMPG = car_style_avgMPG[order(car_style_avgMPG$avgHwMPG), ]
car_style_avgMPG

car_style_avgMPG <- aggregate(cars$highwayMpg, by = list(cars$bodyStyle), FUN = mean)
colnames(car_style_avgMPG) <- c("body style", "average highway mpg")
car_style_avgMPG <- car_style_avgMPG[order(car_style_avgMPG$`average highway mpg`), ]


attach(cars)
sp <- split(highwayMpg, bodyStyle)
avd.hw.mpg <- data.frame(sapply(sp, FUN =  mean))
colnames(avd.hw.mpg) <- "avd.hw.mpg"



data("Titanic")
df_Titanic <- data.frame(Titanic)
attach(df_Titanic)

subset(
  aggregate(Freq, 
            by = list("Sex" = Sex, "Survived" = Survived),
            FUN = sum)
  , Survived=="Yes"
  )

class.sub <- subset(aggregate(Freq, by = list("CLASS"=Class), FUN = sum), x>300)

colnames(class.sub)[2] <- "SUM(Freq)"
sqldf::sqldf("Select Class, SUM(Freq) from df_Titanic group by Class having SUM(Freq) > 300" )



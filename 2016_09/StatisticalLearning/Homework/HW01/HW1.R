############# 8 #############

### 8.a ###
college <- read.csv("College.csv", header = T, sep =",")
attach(college)


### 8.b ###
row.names(college) <- college[,1]
fix(college)

college <- college[,-1]
fix(college)


### 8.c ###
# (i) #
summary(college[, 2:length(college[1,])])

# (ii) #
par(mfrow = c(1,1))
pairs(college[,1:10])

# (iii) #
plot(Outstate~Private)

# (iv) #
Elite <- rep("No", nrow(college))
Elite[college$Top10perc >50] <- "Yes"
Elite <- as.vector(Elite)
summary(college)
college <- data.frame(college, Elite)
plot(Outstate ~ factor(Elite), xlab="Elite")

# (v) #
length(college[1,])
par(mfrow = c(3,3))

college.v <- names(college)
for (k in 2:18) {
  hist(college[,k], main = college.v[k], xlab = " ")
}

par(mfrow = c(1,1))

# (vi) #



############# 9 #############
Auto <- read.csv("Auto.csv", header = T, sep =",")
attach(Auto)

### 9.a ###
# quantitative: 
# quatlitative: cylinders, origin, name

### 9.b.c ###
Auto.bc <- data.frame(matrix(NA, 9,5))
names(Auto.bc) <- c("min", "max", "range", "mean", "sd")
row.names(Auto.bc) <- names(Auto)
Auto.bc <- Auto.bc[-9, ]

for (k in 1:length(Auto[1,])) {
  if (is.numeric(Auto[ ,k])==TRUE){
    Auto.bc[k, 1] <- range(Auto[,k])[1]
    Auto.bc[k, 2] <- range(Auto[,k])[2]
    Auto.bc[k, 3] <- range(Auto[,k])[2] - range(Auto[,k])[1]
    Auto.bc[k, 4] <- mean(Auto[,k])
    Auto.bc[k, 5] <- sd(Auto[,k])
  }
}

HP <- Auto$horsepower
for (k in 1:397) {
  if(is.na(HP[k]==TRUE)){
    HP <- HP[-k]
  }
}

Auto.bc[4,1] <- min(HP)
Auto.bc[4,2] <- max(HP)
Auto.bc[4,3] <- max(HP) - min(HP)
Auto.bc[4,4] <- mean(HP)
Auto.bc[4,5] <- sd(HP)

Auto.bc <- Auto.bc[-c(2,8), ]


### 9.d ###
Auto.d <- Auto[-c(10:85), ]
Auto.dd <- data.frame(matrix(NA, 9,5))
names(Auto.dd) <- c("min", "max", "range", "mean", "sd")
row.names(Auto.dd) <- names(Auto.d)
Auto.dd <- Auto.dd[-9, ]

for (k in 1:length(Auto.d[1,])) {
  if (is.numeric(Auto.d[ ,k])==TRUE){
    Auto.dd[k, 1] <- range(Auto.d[,k])[1]
    Auto.dd[k, 2] <- range(Auto.d[,k])[2]
    Auto.dd[k, 3] <- range(Auto.d[,k])[2] - range(Auto.d[,k])[1]
    Auto.dd[k, 4] <- mean(Auto.d[,k])
    Auto.dd[k, 5] <- sd(Auto.d[,k])
  }
}

HP.d <- Auto.d$horsepower
for (k in 1:length(Auto.d[,1])) {
  if(is.na(HP.d[k]==TRUE)){
    HP.d <- HP.d[-k]
  }
}

Auto.dd[4,1] <- min(HP.d)
Auto.dd[4,2] <- max(HP.d)
Auto.dd[4,3] <- max(HP.d) - min(HP.d)
Auto.dd[4,4] <- mean(HP.d)
Auto.dd[4,5] <- sd(HP.d)

Auto.dd <- Auto.dd[-c(2,8), ]


### 9.e ###
par(mfrow = c(1,1))
pairs(Auto[,-9])

par(mfrow = c(2,3))
for (k in c(1,3,4,5,6,7)) {
  plot(Auto[,k]~ factor(origin), 
       xlab = "origin", ylab = " ", main =names(Auto)[k], 
       col = c(7, 8, 11))
}

par(mfrow = c(2,3))
for (k in c(1,3,4,5,6,7)) {
  plot(Auto[,k]~ factor(cylinders), 
       xlab = "cylinders", ylab = " ", main =names(Auto)[k], 
       col = c(11,7,8,5,6))
}



### 9.f ###
mpg.lm <- lm(mpg ~ factor(cylinders) + displacement + horsepower
    + weight + acceleration + year + factor(origin))

summary(mpg.lm)



############# 10 #############
### 10.a ###
library(MASS)
Boston <- Boston
attach(Boston)
?Boston


### 10.b ###
pairs(Boston)



### 10.c ###

### 10.d ###
par(mfrow=c(1,3))
boxplot(crim, main="crime rates")
boxplot(tax, main="tax rates")
boxplot(ptratio, main=" Pupil-teacher ratios")

### 10.e ###
sum(chas)

### 10.f ###
median(Boston$ptratio)

### 10.g ###
for (k in 1:506) {
  if (Boston$medv[k]==min(Boston$medv)){
    print(k)
  }
}

Boston[c(399, 406), ]
summary(Boston)

### 10.h ###
rm7 <- array(NA, 0)
for (k in 1:506) {
  if (Boston$rm[k]>7){
    rm7[length(rm7)+1] <- Boston$rm[k]
  }
}
length(rm7)


rm8 <- array(NA, 0)
no.rm8 <- array(NA, 0)
for (k in 1:506) {
  if (Boston$rm[k]>8){
    rm8[length(rm8)+1] <- Boston$rm[k]
    no.rm8[length(no.rm8)+1] <- k
  }
}
length(rm8)

Boston[no.rm8, ]


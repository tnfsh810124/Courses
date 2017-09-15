age <- c(20,26,17,40,53,34,57,32,53,38,NA,65,39,27,19,63,69) 
height <- c(165,177,158,168,179,182,164,187,163,NA,172,173,168,153,169,175,189) 
weight <- c(53,57,48,67,75,NA,46,49,52,77,42,93,70,65,59,74,98)


data <- data.frame(cbind(age, height, weight))
attach(data)
# 1) #
data[apply(is.na(data), MARGIN = 1, 
           FUN = function(x) Reduce("|", x)),]
data <- data[-c(6,10,11),]


# 2) #
function(x) as.numeric(names(table(x)))[which.max(table(x))]


# 3) #
Map(function(x) as.numeric(names(table(x)))[which.max(table(x))]
    , data
    )

Map(function(x) mean(x), data)

# 4) #
ca<-
function(x) {
  if(x<=20){print("0-20")} 
  else if(x>=21&x<=40){print("21-40")} 
  else{print("above 40")}
}

tmp.ac <- Map(ca,data$age)

age_cat <- array(NA, 14)


for (x in 1:14) {
  age_cat[x] <- tmp.ac[[x]]
}
data <- cbind(data, age_cat)


# 5) #
BMI <- Map(function(x,y) data$weight/(0.01*data$height)^2, 1:14)[[1]]
data <- cbind(data,BMI)


# 6) #
fbmi <- 
  function(x){
    if(x<18.5){print("underweigth")}
    else if(x<25){print("normal weigth")}
    else if(x<30){print("overweigth")}
    else{print("obesity")}
  }


tmp.bc <- Map(fbmi, data$BMI)

BMI_cat <- array(NA, 14)
for (x in 1:14) {
  BMI_cat[x] = tmp.bc[[x]]
}

data <- cbind(data, BMI_cat)

# 7) #
data.crosstab <- table(data$age_cat, data$BMI_cat)


# 8) #
chisq.test(data.crosstab)
fisher.test(data.crosstab)
# p-value is obviously high so that the two variables are independent.


# 9) #
age <- data$age
height <- data$height
weight <- data$weight

data.intersect <- age>quantile(age)[4] & height>quantile(height)[4] & weight>quantile(weight)[4]
Q4 <- data[data.intersect, ]

# alternative #
data[apply(apply(data[, 1:3], MARGIN = 2, FUN = function(x)x>quantile(x)[4]),
      MARGIN = 1, FUN = function(y) Reduce("&", y)),  ]

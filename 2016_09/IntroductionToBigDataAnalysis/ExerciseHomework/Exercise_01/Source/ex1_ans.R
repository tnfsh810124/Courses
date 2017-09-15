age = c(20,26,17,40,53,34,57,32,53,38,NA,65,39,27,19,63,69)
height = c(165,177,158,168,179,182,164,187,163,
           NA,172,173,168,153,169,175,189)
weight = c(53,57,48,67,75,NA,46,49,52,77,42,93,70,65,59,74,98)

#1
df = data.frame(age,height,weight)
df = na.omit(df)

#2
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(df$age)

#3
apply(df, MARGIN=2, mean)
apply(df, MARGIN=2, getmode)
# Use Map() if you'd like
Map(function(f) sapply(df, f), list(mean, getmode) )

#4
df$age_cat = cut(df[,"age"], breaks = c(0,20,40,Inf),
                 labels = c("0-20","21-40","above 40"))

#5
df$BMI = df[,"weight"] / (df[,"height"] / 100)^2

#6
df$BMI_cat = cut(df[,"BMI"], breaks = c(0,18.5,25,30,Inf),
                 labels = c("Underweight","Normal weight",
                            "Overweight","Obesity"))

#7
ageByBMI = xtabs(formula =  ~ age_cat + BMI_cat, data = df)
ageByBMI = table(df$age_cat,df$BMI_cat) # it works, too

#8
# Chi-squared test of independence
chisq.test(ageByBMI)
# Fisher's exact test might be more appropriate 
# for our small sample with some zero cells 
fisher.test(ageByBMI)

#9
df[apply(apply(df[,1:3], 2, function(x) x > quantile(x)["75%"]) 
         , 1 ,function(y) Reduce("&",y)),]



library(dplyr); library(tibble); library(ggplot2movies); library(data.table); library(sqldf)
movie <- as_tibble(ggplot2movies::movies)



###### 1
adult_tb <- as_tibble(fread("adult.csv"))
colnames(adult_tb)= c("age","workclass","fnlwgt","education","education_num",
                      "marital_status", "occupation","relationship","race", 
                      "sex","capital_gain", "capital_loss","hours_per_week","native_country","salary")

adult_dt <- as.data.table(fread("adult.csv"))
colnames(adult_dt)= c("age","workclass","fnlwgt","education","education_num",
                      "marital_status", "occupation","relationship","race", 
                      "sex","capital_gain", "capital_loss","hours_per_week","native_country","salary")

adult_df <- as.data.frame(fread("adult.csv"))
colnames(adult_df)= c("age","workclass","fnlwgt","education","education_num",
                      "marital_status", "occupation","relationship","race", 
                      "sex","capital_gain", "capital_loss","hours_per_week","native_country","salary")

##### 2

## (a)
adult_tb[adult_tb$age > 20 & adult_tb$age <30, 
         c("education", "race", "sex", "salary")]
tb_tmp <- adult_tb[adult_tb$age > 20 & adult_tb$age <30, 
                   c("education", "race", "sex", "salary")]
tb_tmp <- aggregate(tb_tmp, by = list(tb_tmp$education, tb_tmp$race, tb_tmp$sex, tb_tmp$salary), FUN = length)[, 1:5]
names(tb_tmp) <- c("education", "race", "sex", "salary", "number")
tb_tmp <- tb_tmp[tb_tmp$number > 200, ]
tb_tmp[order(tb_tmp$number), ]

## (b)
adult_tb %>% 
  filter(age > 20 & age <30) %>% 
  group_by(education,race,sex) %>% 
  count(salary) %>% 
  filter(n>200) %>% 
  arrange(n)


##### 3
library(RSQLite); library(rpart)
sqliteDB <- dplyr::src_sqlite("./myDB.Sqlite")
movies_tb <- tbl(sqliteDB, "movies")

movies_tb %>% select(year, length, rating, votes, 
                     r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, 
                     Action, Animation, Comedy, Drama, Documentary, Romance, Short)

rpart(rating ~. , data = movies_tb %>% select(year, length, rating, votes, 
                                              r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, 
                                              Action, Animation, Comedy, Drama, Documentary, Romance, Short))

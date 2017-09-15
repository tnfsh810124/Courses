#******* Group information *******

#Group :

#Please rename this file to Group_.R
#Please write your code in each block.

#******* end of Group information *******




library(dplyr); library(dbplyr); library(RSQLite); library(tibble); library(data.table)
# ******* 1. ********



# ******* end of 1. ********

# ******* 2. ********
#sample output of SQL
sqldf::sqldf("select
             education,race,sex,salary, count(salary)
             from adult_df
             where age between 20 and 30
             group by education, race ,sex,salary
             having count(salary) > 200
             order by count(salary)")



# ******* end of 2. ********


# ******* 3. ********
# Connect to the sqlite file

# ******* end of 3. ********


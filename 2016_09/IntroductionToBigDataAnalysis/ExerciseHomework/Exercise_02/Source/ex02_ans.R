library(data.table)
# 1
dfa <- fread("LoanStats3a.csv", skip = 1,  
             stringsAsFactors = F, verbose = T, nrows = 39786, data.table = F )
dfb <- fread("LoanStats3b.csv", skip = 1,  
             stringsAsFactors = F, verbose = T, nrows = 188183, data.table = F )
dfc <- fread("LoanStats3c.csv", skip = 1,  
             stringsAsFactors = F, verbose = T, nrows = 235631, data.table = F )
dfd <- fread("LoanStats3d.csv", skip = 1,  
             stringsAsFactors = F, verbose = T, nrows = 421097, data.table = F )
# 1.1
# remove the records with “Loans that do not meet the credit policy”

# 1.2
# combine dfa-d
df <- rbind(dfa, dfb, dfc, dfd)
names(df) <- v.names
# keep those with loan_status in "Fully Paid" and "Charged Off"
df <- df[df$loan_status %in% c("Fully Paid", "Charged Off"), ]

# 1.3
# remove those columns with any NAs
B <- apply(df, MARGIN = 2, function(x) as.character(x))
B <- as.data.frame(B)
B <- B[, colSums(is.na(B)) == 0]
df <- df[, colSums(is.na(df)) == 0]

# 1.4
# the percentage of the “Charged Off” loan
sum(df$loan_status == "Charged Off")/length(df$loan_status)
# 0.1987344

# 1.5
df$loan_amnt <- as.numeric(as.character(df$loan_amnt))
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
sqldf::sqldf("Select emp_length, avg(loan_amnt) from df GROUP BY emp_length")

# 1.6
# For those of top (> 5000) loan purposes,
# count the number of loans for different grades SELECT grade, count(id) as Grade_Count FROM loan WHERE purpose IN
sql.1.6 <- 
  sqldf::sqldf("SELECT grade, count(id) as Grade_Count FROM df WHERE purpose IN
               (SELECT purpose FROM df GROUP BY purpose HAVING count(id) >= 5000) GROUP BY grade")
# Rcode
purpose_num <- 
  sapply(levels(df$purpose), 
         function(x) sum(df$purpose == x))
purpose_names_5000 <- names(purpose_num)[purpose_num > 5000]
A <- df[df$purpose %in% purpose_names_5000, ]
grade_num <- 
  sapply(levels(df$grade)[2:8], function(x) sum(df$grade == x))
grade_num <- as.data.frame(grade_num)
names(grade_num)<- "Grade_count"
grade_num

# 1.7





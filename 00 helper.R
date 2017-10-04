train_test_split <- function(DataFrame, DepVar, Split, seed){
  library(caTools)
  set.seed(seed)
  ind <- sample.split(Y = DataFrame[,DepVar], SplitRatio = Split)
  train <- DataFrame[ind,]
  test <- DataFrame[!ind,]
  print("The training and testing datasets have been created.")
  return(list(train = train, test = test))
}

num_NAs <- function(x){
  sapply(x, function(y) sum(is.na(y)))
}

min_max_scaling <- function(train, test){
  
  min_vals <- sapply(train, min)
  range1 <- sapply(train, function(x) diff(range(x)))
  
  # scale the training data
  
  train_scaled <- data.frame(matrix(nrow = nrow(train), ncol = ncol(train)))
  
  for(i in seq_len(ncol(train))){
    column <- (train[,i] - min_vals[i])/range1[i]
    train_scaled[i] <- column
  }
  
  colnames(train_scaled) <- colnames(train)
  
  # scale the testing data using the min and range of the train data
  
  test_scaled <- data.frame(matrix(nrow = nrow(test), ncol = ncol(test)))
  
  for(i in seq_len(ncol(test))){
    column <- (test[,i] - min_vals[i])/range1[i]
    test_scaled[i] <- column
  }
  
  colnames(test_scaled) <- colnames(test)
  
  return(list(train = train_scaled, test = test_scaled))
}

# Older version of the function:

# credit_history <- function(issue_date, credit_line){
#   
#   library(tidyr)
#   library(dplyr)
#   
#   df <- data.frame(issue_date = issue_date, credit_line_date = credit_line)
#   
#   lookup_table <- 1:12
#   names(lookup_table) <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
#   
#   df <- df %>%
#     separate(issue_date, c('issue_month', 'issue_year'), sep = '-') %>%
#     separate(credit_line_date, c('credit_month', 'credit_year'), sep = '-')
#   
#   df$issue_month <- lookup_table[df$issue_month]
#   df$credit_month <- lookup_table[df$credit_month]
#   
#   df <- data.frame(sapply(df, as.numeric))
#   
#   df$credit_month_diff_12 <- 12-df$credit_month
#   df$credit_year_add_1 <- df$credit_year + 1
#   df$issue_year_diff_1 <- df$issue_year - 1
#   df$diff_years <- (df$issue_year_diff_1 - df$credit_year_add_1)*12
#   df$tot <- df$issue_month + df$diff_years + df$credit_month_diff_12
#   
#   return(df$tot)
# }

# issue_date <- X_train$issue_d
# credit_line <- X_train$earliest_cr_line

credit_history <- function(issue_date, credit_line){
  
  # Use issue_d and earliest_credit_line as the two arguments from the data set respectively.
  
  library(tidyr)
  library(dplyr)
  
  df <- data.frame(issue_date = issue_date, credit_line_date = credit_line)
  
  lookup_table <- 1:12
  names(lookup_table) <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  
  df <- df %>%
    separate(issue_date, c('issue_month', 'issue_year'), sep = '-') %>%
    separate(credit_line_date, c('credit_month', 'credit_year'), sep = '-')
  
  df$issue_year <- paste('20', df$issue_year, sep = '')
  
  df <- df %>%
    mutate(credit_year = if_else(as.numeric(credit_year) > as.numeric(substring(year(today()), 3, 4)),
                                 paste('19', credit_year, sep = ''), 
                                 if_else(as.numeric(credit_year) < 10, 
                                         paste('200', substring(credit_year, 2, 2), sep = ''),
                                         paste('20', credit_year, sep = ''))))
  
  df$issue_month <- lookup_table[df$issue_month]
  df$credit_month <- lookup_table[df$credit_month]
  
  
  df <- data.frame(sapply(df, as.numeric))
  
  df$credit_month_diff_12 <- 12 - df$credit_month
  df$credit_year_add_1 <- df$credit_year + 1
  df$issue_year_diff_1 <- df$issue_year - 1
  df$diff_years <- (df$issue_year_diff_1 - df$credit_year_add_1)*12
  df$tot <- df$issue_month + df$diff_years + df$credit_month_diff_12
  
  return(df$tot)
  
}

num_NAs <- function(DataFrame){
  return(sapply(DataFrame, function(x) sum(is.na(x))))
}


print('Helper functions loaded')
source('00 helper.R')
source('library.R')

dat <- fread('/home/jash/Prosper/prepared_data_29-9-17.csv')
mydata <- copy(dat)

# length(unique(mydata$loan_number))
# table(mydata$loan_number)[table(mydata$loan_number) >= 2] # 249983 and 339909

# ensure only distinct rows remain 
mydata <- dplyr::distinct(mydata, loan_number, .keep_all = TRUE)

# Get rid of columns that are available after a loan is funded.

cols_to_del <- c('amount_borrowed', 'age_in_months', 'days_past_due', 'principal_balance','service_fees_paid', 'principal_paid', 'interest_paid', 'prosper_fees_paid', 'late_fees_paid', 'debt_sale_proceeds_received', 'loan_status', 'next_payment_due_amount', 'loan_default_reason', 'loan_default_reason_description', 'next_payment_due_date', 'member_key', 'listing_start_date', 'listing_end_date', 'listing_creation_date', 'listing_status', 'listing_status_reason', 'verification_stage', 'amount_funded', 'amount_remaining', 'percent_funded', 'partial_funding_indicator', 'funding_threshold', 'income_range_description', 'group_indicator', 'group_name', 'investment_typeid', 'whole_loan_start_date', 'whole_loan_end_date', 'last_updated_date', 'amount_delinquent', 'credit_pull_date')

mydata[, (cols_to_del) := NULL]


# Select only CHARGEOFF, COMPLETED and DEFAULTED loans

mydata <- mydata %>%
  filter(loan_status_description %in% c('CHARGEOFF', 'COMPLETED', 'DEFAULTED'))

# Label encode prosper ratings
rating_lookup_table <- 1:7
names(rating_lookup_table) <- c('HR', 'E', 'D', 'C', 'B', 'A', 'AA')

mydata$prosper_rating <- rating_lookup_table[mydata$prosper_rating]

# change CHARGEOFF and DEFAULTED to default so we have only two factors.
mydata$loan_status_description <- ifelse(mydata$loan_status_description %in% c('CHARGEOFF', 'DEFAULTED'), 'default', 'paid')

mydata[mydata == ""] <- NA

# Remove those loans where proser rating is not given

mydata <- mydata %>%
  filter(!(prosper_rating == ""))

# Remove those columns where the number of NAs are greater than 80%

mydata <- mydata[!(colMeans(is.na(mydata)) > 0.8)]

# Keep only those observations where term is 36

mydata <- mydata %>%
  filter(term == 36)

mydata$term <- NULL
mydata$listing_term <- NULL

mydata$loan_number <- NULL
mydata$LoanID <- NULL
mydata$ListingNumber <- NULL
mydata$listing_number <- NULL

# convert origination date, first recorded credit and loan origination date to data format

mydata$origination_date <- ymd_hms(mydata$origination_date)
mydata$first_recorded_credit_line <- ymd_hms(mydata$first_recorded_credit_line)
mydata$loan_origination_date <- ymd_hms(mydata$loan_origination_date)

mydata$income_range <- NULL

# assign a new category - none to NAs in employment status and occupation
mydata$employment_status_description[mydata$employment_status_description == 'Not available'] <- 'none'

mydata$occupation[is.na(mydata$occupation)] <- 'none'

# Borrower state, city and metropolitant area can be used later.

mydata$borrower_state[is.na(mydata$borrower_state)] <- 'none'
mydata$borrower_city <- NULL
mydata$borrower_metropolitan_area <- NULL

# mydata$prior_prosper_loans[mydata$prior_prosper_loans >= 3] <- 3

# mydata$prior_prosper_loans_active[mydata$prior_prosper_loans_active >= 1] <- 1

mydata$channel_code <- NULL # Field has been deprecated


# Label encode fico score

mydata$fico_score[is.na(mydata$fico_score)] <- 'none'

fico_lookup_table <- -1:12
names(fico_lookup_table) <- c('none', '< 600', '600-619', '620-639', '640-659', 
                              '660-679', '680-699', '700-719', '720-739', '740-759', 
                              '760-779', '780-799', '800-819', '820-850')
mydata$fico_score <- fico_lookup_table[mydata$fico_score]


# Create a new variable - number of days of the credit line
mydata$credit_record_duration <- as.numeric(difftime(mydata$origination_date, mydata$first_recorded_credit_line, units = 'days'))

mydata$origination_date <- NULL
mydata$first_recorded_credit_line <- NULL

mydata$oldest_trade_open_date <- NULL

# Certain negative values in months employed, get them to -1
mydata$months_employed[mydata$months_employed < 0] <- -1


# Label encode scorex

mydata$scorex[is.na(mydata$scorex)] <- 'none'
scorex_lookup_table <- -1:10
names(scorex_lookup_table) <- c("none", "< 600", "600-619", "620-639", 
                                "640-649", "650-664", "665-689", "690-701",
                                "702-723", "724-747", "748-777", "778+")
mydata$scorex <- scorex_lookup_table[mydata$scorex]


mydata$loan_origination_date <- NULL

# convert income verifiable and homeowner to a binary variable
mydata$income_verifiable <- ifelse(mydata$income_verifiable == TRUE, 1, 0)

mydata$is_homeowner <- ifelse(mydata$is_homeowner == TRUE, 1, 0)

# Train-Test Split

train_test <- train_test_split(mydata, 'loan_status_description', Split = 0.75, seed = 1)
train <- train_test$train
test <- train_test$test

X_train <- train %>%
  select(-loan_status_description)

X_test <- test %>%
  select(-loan_status_description)

y_train <- train %>%
  select(loan_status_description) %>%
  rename(loan_status = loan_status_description)

y_test <- test %>%
  select(loan_status_description) %>%
  rename(loan_status = loan_status_description)